////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      LS                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///Module contains LDR{B}, STR{B} Instruction Parse and Execution
///Module Dependencies: CommonData,CommonLex, FlexOp2
///Offset values require datatype Op2 and the make-functions from FlexOp2
module LS

open CommonData
open CommonLex
open FlexOp2
open System.Text.RegularExpressions

//----------------------------------Type Definition-------------------------------------------//

///DU, Load or Store
type LORS = |L|S

///DU, Pre or Post
type IndexMode = |PRE|POST 

/// instruction type for ldr and str
type LSInstr =  {Ins: LORS; Byte: bool ; Reg1:RName; Reg2:RName ; Offset: Op2 Option ; PointerUpdate :IndexMode Option; Cond:Condition}

/// parse error, string used
type ErrInstr = string


///type defined for parsing
type Bkt = {
    PreBkt: string;
    InBkt : string;
    PostBkt: string;

}
//--------------------------------Identify OpCode-------------------------//
let LSSpec = {
    InstrC = LS
    Roots = ["LDR";"STR"]
    Suffixes = [""; "B"]
}


let opCodes = opCodeExpand LSSpec

//---------------------Useful functions for general parsing-------------------------//

///Split string into parts using the chars in array as separator. Converted to List, without empty elements, trimmed
let splitIntoWords ( line:string ) (charArray:char []) =
    line.Split (charArray, System.StringSplitOptions.None)
    |> Array.toList 
    |> List.map (fun k -> k.Trim ())

///Convert Char List to string
let convCharListToStr (list:char list) = System.String.Concat (Array.ofList(list))

///Maps Some to Ok, None to Error
let optToRes inp error = 
    match inp with
    |Some k -> Ok (k)
    |None -> Error error  

///remove empty string from list
let removeEmptyStr strList = 
    List.filter (fun k -> k<> "") strList

///return tuple of 2 elements (Result)
let checkTwoEl strList = 
    match strList with
    |a::[b] -> Ok (a,b)
    |[_] -> Error "Invalid syntax: Symbol not found.check symbols such as ',', '[', ']' etc"
    |_ -> Error "Invalid syntax:check symbols such as ',', '[', ']' etc"

 
///parse string into prebracket, inbracket and postbracket        
let parseBktResGen lb rb str= 
    let leftBktSep inp= (splitIntoWords inp [|lb|]) |> checkTwoEl
    let rightBktSep inp = (splitIntoWords inp [|rb|]) |> checkTwoEl             
    match leftBktSep str with
    |Ok (prebkt,rest) -> 
        match rightBktSep rest with
        |Ok (inbkt,postbkt) -> Ok {PreBkt = prebkt; InBkt = inbkt; PostBkt = postbkt}
        |Error k -> Error k
    |Error k -> Error k

//---------------------------------Functions for Offset Parsing------------------------------//


///Arithmetic operations, used to convert expression into Literal
let arithOp op1 op2 sym = 
    match sym with
    |'+' -> op1+op2
    |'-' -> op1-op2
    |'*' -> op1*op2
    |_ -> failwithf "should not happen"

///maps shift operand string to DU, used for parsing string into Offset
let shiftMode = 
    Map.ofList [ 
        "LSL",LSL ; "LSR",LSR ; "ASR",ASR ; "ROR",ROR 
    ] 

///Converts Expression string to uint32
let convExp2Lit (str:string)  (symtab:SymbolTable) = 
    let charList = Seq.toList str

    let convStr2Lit symtab (str:string) = 
        match str.StartsWith ("0X") with
        |false -> 
            match Regex.IsMatch (str,"^[0-9][0-9]*$") with
            |true-> System.Int32.Parse str |>uint32|> makeLiteral
            |false -> Map.tryFind str symtab |> Option.bind makeLiteral
        |true -> 
            let numWithout0x = str.[2..] 
            
            match Regex.IsMatch (numWithout0x,"^[0-9A-F][0-9A-F]*$") with
            |true->  System.Int32.Parse (numWithout0x,System.Globalization.NumberStyles.HexNumber) |>uint32|> makeLiteral
            |false -> None
    let detectFirst list= 
        let firstOpIndex = List.tryFindIndex (fun k -> (k = '+' )|| (k = '-') || (k = '*')) list
        match firstOpIndex with
        |None -> 
            convCharListToStr list
            |>convStr2Lit symtab
            |>(fun k -> (k,[],'!')) //! used as a marker for operator symbols not found
        |Some index -> 
            let (first,rest) = List.splitAt index list
            let firstLitStr = convCharListToStr first
            (convStr2Lit symtab firstLitStr,(List.tail rest),list.[index])
           
    let rec parseExp firstLit operator cList (symtab:SymbolTable) = 
        let (lit,rest,op) = detectFirst cList
        match op with 
        |'!' -> 
            match lit with
            |None -> Error "Invalid Literal"
            |Some (Literal lit') -> Ok (arithOp firstLit lit' operator)
            |_ -> Error "unexpected. should not happen."
        |_ -> 
            match lit with
            |None -> Error "Invalid Literal"
            |Some (Literal lit') ->
                let newFirstLit = arithOp firstLit lit' operator
                parseExp newFirstLit op rest symtab
            |_ -> Error "unexpected. should not happen."

    parseExp (0u) '+' charList symtab
                   


///convert string into literal or register
let litOrReg (el: string) symtab =
    match el with
    |lit when lit.StartsWith "#" -> 
        let litStr = lit.Substring 1
        Result.map Literal (convExp2Lit litStr symtab)
    |reg -> 
        match Map.tryFind reg regNames with
        | Some reg' -> Ok ((Reg reg'))
        | None -> Error "invalid reg"
 

///parse string list, return op2 type, assumption: No empty string in list, all elements trimmed
let str2Offset  (symtab:SymbolTable) (strList:string list)= 
    match strList with
    |[el1] -> litOrReg el1 symtab //one element must be either literal or register
    |el1::[el2] -> //two element must be rrx
        match (el1,el2) with
        |(reg,"RRX") -> 
            let reg' = optToRes (Map.tryFind reg regNames) "Invalid RRX Reg"
            Result.bind (fun k -> optToRes (makeShift RRX k None None) "wont happen, RRX will always execute") reg'
        |_ -> Error "Op2 Invalid"
    |el1::el2::[el3] ->  //3 element offset must be in the form of reg,shifttype and reg/lit
        match Map.tryFind el1 regNames with
        |Some R15 -> Error "PC/R15 not allowed with shift"
        |Some reg' -> 
            match Map.tryFind el2 shiftMode with
            |Some shiftMode' -> 
                let el3' = litOrReg el3 symtab
                match el3' with
                |Ok (Literal lit) -> optToRes (makeShift shiftMode' reg' None ( Some (int32 lit) )) "invalid lit shift operation" 
                |Ok (Reg reg) -> optToRes (makeShift shiftMode' reg' (Some reg) None) "invalid reg shift operation" 
                |Error k -> Error k
                |_ -> failwithf "Error at 3 el. will not happen"
            |None -> Error "Shift type error"
        | None -> 
            Error "invalid reg"
    |_ ->Error "Too many elements "     



//--------------------------------Functions for parsing line data ----------------------------------//



///square bracket parser
let parseSqBktRes = parseBktResGen '[' ']'

///convert string list to op2. 
let strList2Offset strList symtab= 
            strList 
            |> List.collect ( fun a -> ((splitIntoWords a [|' '|])))
            |> removeEmptyStr
            |> str2Offset symtab

///parse LineData into LSInstr
let parse (ls: LineData) : Result<Parse<LSInstr>,string> option =
    let parse' (instrC, (root,suffix,pCond)) =

        /// address this instruction is loaded into memory
        let (WA la) = ls.LoadAddr 

        /// instruction type and byte mode status
        let (ins,byte) = 
            match (root,suffix) with
                |("LDR","")-> (L, false) 
                |("LDR","B")->(L,true) 
                |("STR","")->(S,false)
                |("STR","B")->(S,true)
                |_ -> failwithf "will not happen"

        ///parse operand string into Bkt type
        let bktStrList = parseSqBktRes ls.Operands
        
        ///main parsing. Output of type LSInstr Result
        let instrLS = 
            match bktStrList with
                |Ok (bktStrList') ->
                    /// find reg1 from prebkt
                    let reg1 = 
                        let reg1' = (splitIntoWords bktStrList'.PreBkt [|','|]) 
                        match reg1' with
                        |x::[""] -> Map.tryFind x regNames
                        | _ -> None

                    ///find reg2 and potential offset from in bracket             
                    let (reg2, offsetInB) = 
                        let reg2Offset =   (splitIntoWords bktStrList'.InBkt [|','|]) 
                        match reg2Offset with
                        |[] -> (None,None)
                        |[reg2'] -> ((Map.tryFind reg2' regNames),None)
                        |reg2'::offset -> 
                            let reg22 = Map.tryFind (reg2') regNames 
                            let offset' = 
                                //Assume symtab is some when second parse is completed
                                match ls.SymTab with
                                |None -> None
                                |Some symtab -> Some (strList2Offset offset symtab)
                            (reg22, offset')

                    ///find potential offset and pointer update cue from postbkt
                    let (offsetOutB, exMark) = 
                        let outOfBkt = (splitIntoWords (bktStrList'.PostBkt) [|','|])
                        match outOfBkt with 
                        |[]|[""] -> (None, false)
                        |["!"] -> (None, true)
                        |offset -> 
                            let offset' = 
                                match ls.SymTab with
                                |None -> None
                                |Some symtab -> Some (strList2Offset offset symtab)                           
                            (offset', false)
                    match (reg1, reg2, offsetInB, offsetOutB, exMark) with
                    |(None, _, _, _, _) -> Error "Reg 1 Invalid"
                    |(_,None, _, _, _) -> Error "Reg 2 Invalid"
                    |(_,_,Some (Error k),_,_) -> Error k
                    |(_,_,_,Some (Error k), _) -> Error k
                    //No Offset
                    |(Some reg1',Some reg2',None, None, false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = None; PointerUpdate = None; Cond = pCond }
                    //Offset in bkt found, no pointer update
                    |(Some reg1',Some reg2',Some (Ok offset) , None, false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = None;Cond = pCond }
                    //Offset in bkt found, pre-index
                    |(Some reg1',Some reg2',Some (Ok offset), None, true) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some PRE;Cond = pCond }
                    //Offset out bkt found, post index
                    |(Some reg1',Some reg2',None, Some (Ok offset) , false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some POST;Cond = pCond }
                    //Error for unexpected type
                    |_ -> Error "Offset error"
                |Error k -> Error k

        ///output Parse Type
        match instrLS with 
        |Ok (instrLS' ) -> 
            Ok {
                PInstr=instrLS'; 
                PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                PSize = 4u; 
                PCond = pCond 
                }
        |Error k -> Error k            

    Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
    |> Option.map parse' // if unknown keep none, if known parse it.


/// Parse Active Pattern used by top-level code
let (|IMatch|_|) = parse


//---------------------------------Load and Store Instruction Execution-------------------//

///define range for instruction memory. Can be changed. 
let dataMemAddrStart = 0x1000u

///return function based on byte status
let byteWordTwoFunc byteFunc wordFunc byte=
    match byte with
    |true -> byteFunc
    |false ->wordFunc

///update input record with provided update list
let updateRec (recIn) updateList = 
    updateList
    |>List.fold (fun old (key,newval) -> Map.add key newval old) recIn

/// break word into byte list. Useful in byte functions
let breakWordIntoBytes (word:uint32) = 
    [0;8;16;24]
    |>List.map (fun k -> (word<<<(24-k))>>>24)    

///Check address is above data memory start address as defined above
let checkAddrValidity addr = 
    match addr with
    |k when k <dataMemAddrStart -> Error "Address under 0x1000 not allowed."
    |validAddr -> Ok validAddr

///Check address is aligned, for word function only
let checkAddrAlign addr = 
    match addr with
    |k when k%4u <> 0u -> Error "unaligned memory access"
    |validAddr -> Ok validAddr

///Load Data Word, output uint32 result
let loadMMDataWord addr (dataPath:DataPath<'INS>)= 
    let load addr = 
        match dataPath.MM.TryFind (WA addr) with
        |Some (DataLoc k) -> 
            Ok k
        |None -> Ok 0u ///return 0 for uninitialised memory
        |_ -> Error "Memory access failure"
    checkAddrValidity addr
    |>Result.bind checkAddrAlign
    |>Result.bind load

///load data byte, output uint32 result
let loadMMDataByte addr (dataPath:DataPath<'INS>)= 
    let load addr = 
        let addrOffset = addr%4u
        let baseAddr = addr-addrOffset
        match dataPath.MM.TryFind (WA baseAddr) with
        |Some (DataLoc data) -> 
            let byteList = breakWordIntoBytes data
            Ok (byteList.[int addrOffset])
        |None -> Ok 0u ///return 0 for uninitialised memory
        |_ -> Error "Memory access failure"        
    checkAddrValidity addr
    |> Result.bind load


///convert word to single byte
let word2Byte word:uint32 = 
    word&&&0xFFu

///store data word. output updated MM
let storeMMDataWord addr dataPath word= 
    let store addr = 
        Ok (dataPath.MM.Add (WA addr, DataLoc word))
    checkAddrValidity addr
    |>Result.bind checkAddrAlign
    |>Result.bind store

///store data byte, output updated MM
let storeMMDataByte addr dataPath word = 
    let store addr =  
        let byteData = word2Byte word
        let addrOffset = addr%4u
        let baseAddr = addr-addrOffset
        let shiftedByte = byteData<<<int (8u*addrOffset)
        match dataPath.MM.TryFind (WA baseAddr) with
        |Some (DataLoc data) -> 
            let numToBeSubtracted = ((data<<<int (8u*(3u-addrOffset)))>>>24)<<<int (addrOffset*8u)
            let newWData = (data-numToBeSubtracted)+shiftedByte
            Ok (dataPath.MM.Add (WA baseAddr, DataLoc newWData))
        |None -> Ok (dataPath.MM.Add (WA baseAddr, DataLoc shiftedByte)) ///return 0 for uninitialised memory
        |_ -> Error "Memory access failure"  
    checkAddrValidity addr
    |>Result.bind store

///load function 
let loadFunc byte= byteWordTwoFunc loadMMDataByte loadMMDataWord byte

///Return data path with PC+4
let PCPlus4 dataPath = 
    let initialPC = dataPath.Regs.[R15]
    {dataPath with Regs = dataPath.Regs.Add (R15,initialPC+4u)}

///Store Data, return MM
let storeDataReturnMM addr dataPath source byte= 
    let func = byteWordTwoFunc storeMMDataByte storeMMDataWord byte
    func addr dataPath source

///Main execution function
let LSMain reg1 reg2 offset byte ins (ptrupdate:IndexMode Option) (dataPath: DataPath<'INS>) = 
    match reg2 with
    |R15 -> Error "R15 not allowed as destination (STR) or source (LDR)"
    |reg when (reg = reg1 && ptrupdate.IsSome) -> Error "Destination must be different from source base addr"
    |_ -> 
        let addrInReg = dataPath.Regs.[reg2]
        let source = 
            match ins with 
            |L ->
                match (offset, ptrupdate) with
                |(None,None) |(Some _, Some POST) -> 
                    loadFunc byte addrInReg dataPath
                |(Some offset, None) |(Some offset, Some PRE) ->                       
                    let offsetAddr = addrInReg + offset
                    loadFunc byte offsetAddr dataPath
                |_ -> failwithf "unexpected combination of offset and pointerupdate"
            |S ->  Ok (dataPath.Regs.[reg1])
        match source with
        |Ok source'-> 
            match (ins, ptrupdate) with
            |(L,None) -> Ok (PCPlus4 {dataPath with Regs = dataPath.Regs.Add(reg1,source')})
            |(L,Some PRE)|(L,Some POST) ->
                match offset with
                |Some offset ->  
                    let regUpdateList = [(reg1,source');(reg2,addrInReg+offset)]
                    let newRegs = updateRec dataPath.Regs regUpdateList
                    Ok (PCPlus4 {dataPath with Regs =newRegs})
                |None -> failwithf "LS. will not happen"
            |(S,None) -> 
                match offset with
                |Some offsetVal ->   
                    let offsetAddr = addrInReg + offsetVal
                    let newMM = storeDataReturnMM offsetAddr dataPath source' byte
                    Result.map (fun k -> PCPlus4 {dataPath with MM = k}) newMM
                |None -> 
                    let newMM = storeDataReturnMM addrInReg dataPath source' byte
                    Result.map (fun k -> PCPlus4 {dataPath with MM = k}) newMM
            |(S,Some mode)  ->
                match offset with
                |Some offsetVal ->   
                    let offsetAddr = addrInReg + offsetVal
                    let newAddr = 
                        match mode with
                        |PRE -> offsetAddr
                        |POST -> addrInReg
                    let newMM = storeDataReturnMM newAddr dataPath source' byte
                    Result.map (fun mm -> PCPlus4{dataPath with MM=mm;Regs = dataPath.Regs.Add(reg2,offsetAddr)}) newMM
                    
                |None -> Error "Offset value missing"
        |Error k -> Error k

///Top level Load Store Execution Function
let execLS (instr:LSInstr) (dataPath: DataPath<'INS>) = 
    
    let reg1 = instr.Reg1
    let reg2 = instr.Reg2
    let offset = Option.map (fun k -> flexOp2 k dataPath) instr.Offset
    let byte = instr.Byte
    let ins = instr.Ins
    let ptrupdate = instr.PointerUpdate
    LSMain reg1 reg2 offset byte ins ptrupdate dataPath
