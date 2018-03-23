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
//----------------------------------Type Definition-------------------------------------------//

///DU, Load or Store
type LORS = |L|S

///DU, Pre or Post
type IndexMode = |PRE|POST 



/// instruction type for ldr and str
type LSInstr =  {Ins: LORS; Byte: bool ; Reg1:RName; Reg2:RName ; Offset: Op2 Option ; PointerUpdate :IndexMode Option; Cond:Condition}

type LDRPseudo = {Dest:RName; Value:uint32;Cond:Condition}

type Instr = LS of LSInstr | LDRP of LDRPseudo

/// parse error, string used
type ErrInstr = string


///type defined for parsing

//--------------------------------Identify OpCode-------------------------//
let LSSpec = {
    InstrC = LSC
    Roots = ["LDR";"STR"]
    Suffixes = [""; "B"; ]
}


let opCodes = opCodeExpand LSSpec 


///parse LineData into LSInstr
let parse (ls: LineData) : Result<Parse<Instr>,string> option =
    let parse' (_, (root,suffix,pCond)) =

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
                    |(Some reg1',Some reg2',None, None, false) -> Ok (LS {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = None; PointerUpdate = None; Cond = pCond })
                    //Offset in bkt found, no pointer update
                    |(Some reg1',Some reg2',Some (Ok offset) , None, false) -> Ok (LS {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = None;Cond = pCond })
                    //Offset in bkt found, pre-index
                    |(Some reg1',Some reg2',Some (Ok offset), None, true) -> Ok (LS {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some PRE;Cond = pCond })
                    //Offset out bkt found, post index
                    |(Some reg1',Some reg2',None, Some (Ok offset) , false) -> Ok (LS {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some POST;Cond = pCond })
                    //Error for unexpected type
                    |_ -> Error "Offset error"
                |Error k -> 
                    let strList = splitStrIntoList ls.Operands
                    match root,strList with
                    |"LDR",reg::[value] when value.StartsWith "=" -> 
                        let dest = Map.tryFind reg regNames
                        let value' = 
                            match ls.SymTab with
                            |None -> Ok 0u
                            |Some symtab -> convExp2LitNoCheck value.[1..] symtab
                        match dest,value' with
                        |(Some reg, Ok k) -> Ok (LDRP { Dest =reg; Value = k; Cond = pCond})
                        |None,_ -> Error "destination Reg invalid"
                        |_,Error k -> Error k
                    |_ -> Error k

                        
                        


                        


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



///return function based on byte status
let byteWordTwoFunc byteFunc wordFunc byte=
    match byte with
    |true -> byteFunc
    |false ->wordFunc



/// break word into byte list. Useful in byte functions
let breakWordIntoBytes (word:uint32) = 
    [0;8;16;24]
    |>List.map (fun k -> (word<<<(24-k))>>>24)    

///Check address is above data memory start address as defined above
let checkAddrValidity addr = 
    match addr with
    |k when k <dataMemAddrStart -> Error "Address under 0x100 not allowed."
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



///Store Data, return MM
let storeDataReturnMM addr dataPath source byte= 
    let func = byteWordTwoFunc storeMMDataByte storeMMDataWord byte
    func addr dataPath source

///Main execution function
let LSMain reg1 reg2 offset byte ins (ptrupdate:IndexMode Option) (dataPath: DataPath<'INS>) = 
    match ins, byte, reg2 with
    |S,_,R15 | L,true,R15 -> Error "R15 not allowed as destination (STR) or source (LDR)"
    |_,_,reg when (reg = reg1 && ptrupdate.IsSome) -> Error "Destination must be different from source base addr"
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

let LDRPseudoExec dest value (dp:DataPath<'INS>) = 
    Ok (PCPlus4{dp with Regs = dp.Regs.Add (dest,value)})

///Top level Load Store Execution Function
let execLS (ins:Instr) (dataPath: DataPath<'INS>) = 
    match ins with
    |LS instr -> 
        let reg1 = instr.Reg1
        let reg2 = instr.Reg2
        let offset = Option.map (fun k -> flexOp2 k dataPath) instr.Offset
        let byte = instr.Byte
        let ins = instr.Ins
        let ptrupdate = instr.PointerUpdate
    
        LSMain reg1 reg2 offset byte ins ptrupdate dataPath
    |LDRP instr -> 
        LDRPseudoExec instr.Dest instr.Value dataPath
