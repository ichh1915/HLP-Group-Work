//////////////////////////////////////////////////////////////////////////////////////////
//                                  INITPROJECTLEXER                                    //
//////////////////////////////////////////////////////////////////////////////////////////
namespace VisualTest
(*
    Change Log
    v1.00 initial version
*)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      CommonData                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
///Common Data Structure required for all modules
///Changes made to original CommonData: 
///None
module CommonData = 
        //////////////////////////////////////////////////////////////////////////////////////
        //                   Common types and code used by all modules
        //////////////////////////////////////////////////////////////////////////////////////

        /// ARM Status bits
    type Flags = { N: bool; C:bool; Z: bool; V:bool}


    ////////////////////////ARM register names and operations/////////////////////////////


    /// ARM register names
    /// NB R15 is the program counter as read
    [<Struct>]
    type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 
                 | R8 | R9 | R10 | R11 | R12 | R13 | R14 | R15



    /// Map used to convert strings into RName values, 
    /// includes register aliasses PC, LR, SP
    let regNames = 
        Map.ofList [ 
            "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; "R5",R5
            "R6",R6 ; "R7",R7 ; "R8",R8 ; "R9", R9 ; "R10",R10 ; "R11",R11 ; 
            "R12",R12 ; "R13",R13 ; "R14",R14 ; "R15",R15 ; 
            "PC",R15 ; "LR",R14 ; "SP",R13 
        ] 

    // various functions used to convert between string, RName, and register number

    /// Inverse of regNames, used to convert RName values to strings
    /// NB The string chosen will always be the register (not alias)
    let regStrings = 
        regNames
        |> Map.toList
        |> List.map (fun (s,rn)-> (rn,s)) 
        |> List.filter (fun (_,s:string) -> s.StartsWith "R")
        |> Map.ofList

    /// Map converts RName into register number (no aliasses)
    let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings

    /// Map converts register number into RName (no aliasses)
    let inverseRegNums = 
        regNums |> Map.toList 
        |> List.map (fun (rn,n)->(n,rn)) |> Map.ofList

    /// Property on RName to return register number, for convenience
    /// Aliasses not included, since they are not RNames
    type RName with
        /// Return the number of a register as an integer
        member r.RegNum = regNums.[r]

    /// Return a register name from an integer
    let register n = 
        if 0 <= n && n < 16 
        then inverseRegNums.[n] 
        else (failwithf "Register %d does not exist!" n)

    /// Type to represent the contents of one memory location
    /// 'INS is a parameter set to the type of an instruction
    /// needed because instruction type is only defined
    /// at top level.
    type MemLoc<'INS> =
        | DataLoc of uint32
        | Code of 'INS

    /// type to represent a (word) address
    /// there is some ambiguity. Does this contain the real address
    /// which is always divisible by 4
    /// or does it contain the word number (real address dvided by 4)
    /// either way multiply/divide by 4 will cause problems!
    /// document this well and be consistent.
    type WAddr = | WA of uint32

    /// type to represent memory
    type MachineMemory<'INS> = Map<WAddr,MemLoc<'INS>> 

    /// ARM state as values of all registers and status bits
    /// NB PC can be found as R15 - 8. (Pipelining)
    type DataPath<'INS> = {
        Fl: Flags; // Flags
        Regs:Map<RName,uint32> // map representing registers. 
                               // Must be correctly initialised
        MM: MachineMemory<'INS> // map showing the contents of all memory
        }

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      CommonTLex                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Common code for Instruction Definition and Parsing
///Changes made to original CommonLex:
///InstrClass=| ARI | LOG | LS | LSM | Branch
///Must be changed during group phase
///Module dependencies: CommonData
module CommonLex= 

    open CommonData

    /// ARM execution conditions
    type Condition =

        | Ceq
        | Cne
        | Cmi
        | Cpl
        | Chi
        | Chs
        | Clo
        | Cls
        | Cge
        | Cgt
        | Cle
        | Clt
        | Cvs
        | Cvc
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions (example, add/change this is needed)
    type InstrClass = | ARI | LOG | LS | LSM | Branch

    /// specification of set of instructions
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }

    type SymbolTable = Map<string,uint32>

    /// result returned from instruction-specific module parsing
    /// an instruction class. If symbol definitions are found in a 
    /// symbol table then a complete parse will be output
    /// otherwise some fields will be None
    type Parse<'INS> = {
            /// value representing instruction. NB type varies with instruction class
            PInstr: 'INS 
            /// name and value of label defined on this line, if one is.
            PLabel: (string * uint32) option 
            /// number of bytes in memory taken up by this instruction
            PSize: uint32 
            /// execution condition for instruction
            PCond: Condition
        }

    /// data given to instruction-specific parse function
    type LineData = {
        /// memory address this instruction is loaded. Must be word address
        LoadAddr: WAddr 
        /// name of label defined on this line, if one exists
        Label: string option 
        /// table of symbols with defined values. 
        /// if this is given we are phase 2 and all symbols should be defined
        /// if this is not given we are phase 1 and no symbols are defined
        SymTab: SymbolTable option
        /// opcode string
        OpCode: string
        /// string of all the operands
        Operands: string
    }


    /// Strings with corresponding execution condition
    /// Note some conditions have multiple strings
    /// Note "" is a valid condition string (always execute condition)
    let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                    "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                    "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;
                    "NV",Cnv ; "AL",Cal ; "",Cal; "",Cal] |> Map.ofList

    /// list of all strings representing execution conditions
    /// includes ""
    let condStrings = 
        condMap
        |> Map.toList
        |> List.map fst
        |> List.distinct    

    /// generate all possible opcode strings for given specification
    /// each string is paired with info about instruction
    /// and the three parts of the opcode
    let opCodeExpand (spec: OpSpec) 
        //    opcode    class        root    suffix   instr cond
        : Map<string, InstrClass * (string * string * Condition)> =
        spec.Roots
        |> List.collect (fun r -> 
            spec.Suffixes
            |> List.collect (fun s -> 
                condStrings
                |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
                |> Map.ofList

    /// function used to change PInstr field of a Result<Parse<'INS>,'E>
    /// the output has this field mapped with fMap
    /// or if Error has this value chnaged by fMapE
    let pResultInstrMap fMap fMapE paRes =
        match paRes with
        | Ok ({PInstr=ins} as pr) -> 
            // Note subtle point. {pr with Pinst = ...} will not work here
            // That is because applying fmap changes the type of PInstr
            // and therefore the type of the record.
            Ok {
            PInstr = fMap ins 
            PLabel = pr.PLabel
            PCond = pr.PCond
            PSize = pr.PSize
            }
        | Error e -> Error (fMapE e)

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      FlexOp2                                                       //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Module contains the datatype Op2 and functions to generate them
///Module Dependencies: Common Data
module FlexOp2=

    open CommonData

    ///Type Definitions
    ///-----------------------------------------------------------------
    [<Struct>]
    type ShiftRotateOption = |LSL|ASR|LSR|ROR|RRX

    type ShiftVal = |Lit of uint32 | RShift of RName

    type Op2 = 
        |Literal of uint32
        |Reg of RName
        |RegS of RName*ShiftRotateOption*ShiftVal
        |RegRRX of RName

    ///Basic Shift/Rotate Functions
    ///-----------------------------------------------------------------
    let rOR num (rBit:uint32) = (uint32(num)>>> int32(rBit)) + (uint32(num)<<<(32-int32(rBit)))

    let lSR num sBit = (uint32(num))>>>int32(sBit)

    let lSL num sBit = (uint32(num))<<<int32(sBit)

    let rRX num cFlag= 
        let carryBit = 
            match cFlag with
            |true -> 1
            |false -> 0
        (uint32(num)>>> 1) + (uint32(carryBit)<<<31)

    let aSR num sBit = 
        match (int32 (num)>=0) with 
            |true -> uint32(num)>>>int32(sBit)
            |false -> (uint32(num)>>> int32(sBit)) + (0xFFFFFFFFu<<<(32-int32(sBit)))
        
    ///Make Functions
    ///------------------------------------------------------------------------  
    let makeShift (shiftType:ShiftRotateOption) (rOp2:RName) (rShift: RName option) (s: int option) = 
        match shiftType with
        |RRX -> Some (RegRRX rOp2)
        |_ -> 
            match (rShift,s) with
            |None,Some(lit) when (lit >=1 && lit <=31) -> Some (RegS (rOp2,shiftType,Lit (uint32 (lit))))
            |Some(reg),None -> Some (RegS (rOp2, shiftType, RShift reg))
            |None,None -> Some (RegS (rOp2, shiftType, Lit (1u)))
            |_,_ -> None

    let checkLitValidity lit = 
        [0u..15u]
        |> List.map (fun n -> rOR lit n) 
        |> List.exists (fun x -> (x>=0u) && (x <=255u))

    let makeLiteral (literalData:uint32) = 
        match literalData with
        | lit when checkLitValidity lit -> Some (Literal lit)
        | _ -> None


    ///flexOp2
    ///---------------------------------------------------------------------------
    let flexOp2 (op2:Op2) (cpuData:DataPath<'INS>) = 
        let convertedOp2 = 
            match op2 with
            |RegS (r,s,RShift rName) -> RegS (r,s,Lit (cpuData.Regs.[rName]%32u))
            |others -> others
        match convertedOp2 with 
        | Literal lit -> lit
        | Reg reg -> cpuData.Regs.[reg]
        | RegS (rName,LSR,Lit lit) -> lSR cpuData.Regs.[rName] lit
        | RegS (rName,LSL,Lit lit) -> lSL cpuData.Regs.[rName] lit
        | RegS (rName,ASR,Lit lit) -> aSR cpuData.Regs.[rName] lit
        | RegS (rName,ROR,Lit lit) -> rOR cpuData.Regs.[rName] lit
        | RegRRX (rName) -> rRX cpuData.Regs.[rName] cpuData.Fl.C
        | _ -> failwithf("Should not happen")

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      LS                                                            //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


///Module contains LDR{B}, STR{B} Instruction Parse and Execution
///Module Dependencies: CommonData,CommonLex, FlexOp2
///Offset values require datatype Op2 and the make-functions from FlexOp2
module LS=

    open CommonData
    open CommonLex
    open FlexOp2

    //----------------------------------Type Definition-------------------------------------------//

    ///DU, Load or Store
    type LORS = |L|S

    ///DU, Pre or Post
    type IndexMode = |PRE|POST 

    /// instruction type for ldr and str
    type LSInstr =  {Ins: LORS; Byte: bool ; Reg1:RName; Reg2:RName ; Offset: Op2 Option ; PointerUpdate :IndexMode Option}

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
 
        let convStr2Lit symtab str = 
            match System.UInt32.TryParse str with
            |(true, lit') -> makeLiteral lit'
            |(false,_) -> Map.tryFind str symtab |> Option.bind makeLiteral
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
                        |(Some reg1',Some reg2',None, None, false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = None; PointerUpdate = None }
                        //Offset in bkt found, no pointer update
                        |(Some reg1',Some reg2',Some (Ok offset) , None, false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = None }
                        //Offset in bkt found, pre-index
                        |(Some reg1',Some reg2',Some (Ok offset), None, true) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some PRE }
                        //Offset out bkt found, post index
                        |(Some reg1',Some reg2',None, Some (Ok offset) , false) -> Ok {Ins = ins; Byte = byte; Reg1 = reg1'; Reg2 = reg2'; Offset = Some offset; PointerUpdate = Some POST }
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
                        
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      LSM                                                           //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Module contains LDM, STM Instruction Parse and Execution
///Module Depencences: CommonData,CommonLex,LS
///LS: 
///type LORS -> DU for Load or Store                   
///type Bkt -> store product of bracket parsing
///function parseBktResGen for bracket parsing
///function splitIntoWords for general parsing
///function opToRes to convert option type to result type
///function PCPlus4
///function updateRec
module LSM=

    open CommonData
    open CommonLex
    open LS



    //-------------------------------Type defined for LSM instructions--------------------//
    
    type DIR = |FA|FD|EA|ED

    /// instruction type for ldm and stm
    type LSMInstr =  {Ins: LORS; Dir: DIR; Reg1:RName; RegList:RName list; PtrUpdate: bool}

    /// parse error. use default string type
    type ErrInstr = string


    //--------------------------Identify Opcode-----------------------------//
    ///Maps string to instruction (load or store) and indexing direction. All possible options included
    let LSMOpCodeMap = 
        [("LDM",(L,FD));("LDMFD",(L,FD));("LDMIA",(L,FD));
            ("LDMEA",(L,EA));("LDMDB",(L,EA));
            ("LDMFA",(L,FA));("LDMDA",(L,FA));
            ("LDMED",(L,ED));("LDMIB",(L,ED));
            ("STM",(S,EA));("STMEA",(S,EA));("STMIA",(S,EA));
            ("STMFD",(S,FD));("STMDB",(S,FD));
            ("STMED",(S,ED));("STMDA",(S,ED));
            ("STMFA",(S,FA));("STMIB",(S,FA))]
        |>Map.ofList
    
    let LSMSpec = {
        InstrC = LSM
        Roots = ["LDM";"STM"]
        Suffixes = ["FA"; "FD";"EA";"ED"; ""; "IA";"DB";"IB";"DA"]
    }


    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand LSMSpec


    //--------------------------Functions for parsing------------------------//
    
    ///split string into bkt type with curly bkt as separator
    let parseCurBktRes = parseBktResGen '{' '}' 

    ///parse prebkt into (Reg1, pointerupdate)
    ///return None when reg1 not found
    let parsePreBkt (str:string) = 
        match str with
        |k when k.EndsWith ","-> 
            let rstr = (k.TrimEnd [|','|]).Trim[|' '|]
            match rstr with
            |reg when reg.EndsWith "!" ->  
                let rnameStr = (reg.TrimEnd [|'!'|]).Trim[|' '|]
                let rname = Map.tryFind rnameStr regNames
                (rname,true)
            |reg -> ((Map.tryFind reg regNames),false)
        |_ -> (None,true)

    ///Function to convert string into rname list
    ///Example: R2-R5 -> [R2;R3;R4;R5]
    ///output is option type. None indicates Error
    let regRangeToList str = 
        let regStartEnd = splitIntoWords str [|'-'|]
        match regStartEnd with
        |first::[last] -> 
            let r1 = Map.tryFind first regNames
            let r2 = Map.tryFind last regNames
            match (r1,r2) with
            | (Some r1',Some r2') -> 
                [r1'.RegNum .. r2'.RegNum]
                |>List.map register 
                |>Some
            |_ -> None
        |[reg] -> 
            (Map.tryFind reg regNames)
            |>Option.map (fun k -> [k])                  
        |_ -> None

    ///parse string in bracket to list of registers. None indicates Error          
    let parseInBkt (str:string) = 
        let convSomeToItem inp = 
            match inp with
            |Some k -> k
            |None -> failwithf "will not happen"
        let regList = 
            splitIntoWords str [|','|]
            |>List.map regRangeToList
        match List.contains None regList  with
        |true -> None
        |false -> 
            regList
            |> List.collect (convSomeToItem)
            |> Some

    ///top level parse for LSM module
    let parse (ls: LineData) : Result<Parse<LSMInstr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr 
            let (ins,dir)=LSMOpCodeMap.[root+suffix]
            let bktStrList = parseCurBktRes ls.Operands            
            
            let parseBkt bkt = 
                let prebkt = parsePreBkt bkt.PreBkt
                let inbkt = parseInBkt bkt.InBkt
                let outbkt = bkt.PostBkt
                match (prebkt,inbkt,outbkt) with
                |((Some reg1,ptrUpdate),Some regList, "") -> Ok {Ins= ins; Dir=dir; Reg1=reg1; RegList=regList; PtrUpdate = ptrUpdate}
                |((None,_),_,_) -> Error "Invalid Reg 1"
                |(_,None,_) -> Error "Invalid Reg List"
                |(_,_,k) -> Error "Unexpected chars after brackets"
           
            let instrLSM =  
                bktStrList
                |>Result.bind parseBkt

            match instrLSM with 
            |Ok (instrLSM' ) -> 
                Ok {
                    PInstr=instrLSM'; 
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                    PSize = 4u; 
                    PCond = pCond 
                    }
            |Error k -> Error k            

        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse
    
    //---------------------------------functions for LSM execution-------------------------//

    
    ///Function that stores/loads multiple registers
    let storeLoadM ls dir addr dataPath regList = 
        let listLength = List.length regList
        let addrList =     
            match (ls,dir) with
            |(S,EA)|(S,FA)|(L,ED)|(L,FD) ->
                [0..4..((listLength-1)*4)]
                |> List.map (fun k -> addr + uint32 k )
            |(S,ED)|(S,FD)|(L,EA)|(L,FA) ->
                [0..4..((listLength-1)*4)]
                |> List.map (fun k -> addr - uint32 k )
        let finalAddr = List.last addrList
        let invalidStatus =  List.exists (fun k -> k<0x1000u) addrList 
        let unalignedStatus = List.exists (fun k -> k%4u<>0u) addrList
        match (invalidStatus,unalignedStatus) with
        |(false,false) -> 
            match ls with
            |S -> 
                let regValues = 
                    regList
                    |>List.map (fun k -> dataPath.Regs.[k])
                let updateList = 
                    List.map2 (fun a b -> (WA a,DataLoc b)) addrList regValues
                let updatedMM =  updateRec dataPath.MM updateList    
                Ok ({dataPath with MM = updatedMM},finalAddr)
            |L -> 
                let mmData = 
                    addrList
                    |>List.map  (WA >> dataPath.MM.TryFind>>fun k ->
                        match k with
                        |Some (DataLoc data) -> data
                        |None -> 0u
                        |_ -> failwithf "will not happen. Memory of instruction area read")

                let updateList = 
                    List.map2 (fun a b -> (a,b)) regList mmData
                let updatedRegs = updateRec dataPath.Regs updateList
                Ok ({dataPath with Regs = updatedRegs}, finalAddr)
        |(true,_) -> Error "Invalid Address" //when address less than 0x1000u
        |(_,true) -> Error "unaligned word address"   //when address is not a multiple of 4                         
            

    ///Main LSM execution       
    let LSMMain dir ins ptrupdate reg1 reglist datapath =
        let sort ins dir regs= 
            match (ins,dir) with
            |(S,EA)|(S,FA)|(L,ED)|(L,FD)->List.sort regs
            |(S,ED)|(S,FD)|(L,EA)|(L,FA) ->List.sortDescending regs
        let regNameList = 
            reglist
            |> List.map (fun k -> regNums.[k]) 
            |> sort ins dir
            |> List.map register
        let ptrAddr = datapath.Regs.[reg1]
        let firstElAddr = 
            match (ins,dir) with
                |(S,EA)|(S,ED)|(L,FA)|(L,FD)->ptrAddr
                |(S,FA)|(L,ED) ->ptrAddr+4u
                |(S,FD)|(L,EA) ->ptrAddr-4u
        let newDPAndLastAddr = 
            match reglist,reg1 with
            |(reglist',_) when List.exists (fun k -> k=R15 || k=R13) reglist' -> Error "List of Registers cannot contain R13 and R15"
            |(reglist',reg1') when List.contains reg1' reglist' -> Error "Stack pointer cannot be in list of registers"
            |(_,reg1') when reg1'=R15 -> Error " Stack pointer must be from R0 to R14."
            |_ -> (storeLoadM ins dir firstElAddr datapath regNameList)
        match newDPAndLastAddr with
        |Ok (DP,lastAddr) ->
            let updatedReg1 = 
                match ptrupdate with
                |false -> ptrAddr
                |true -> 
                    match (ins,dir) with
                    |(S,FA)|(S,FD)|(L,EA)|(L,ED)->lastAddr
                    |(S,EA)|(L,FD) ->lastAddr+4u
                    |(S,ED)|(L,FA) ->lastAddr-4u
            
            Ok (PCPlus4 {DP with Regs = DP.Regs.Add (reg1,updatedReg1)})   
        |Error k -> Error k                         

    //Top level LSM execution
    let execLSM (instr:LSMInstr) (dataPath: DataPath<'INS>) = 
        let dir = instr.Dir
        let ins = instr.Ins
        let ptrupdate = instr.PtrUpdate
        let reg1 = instr.Reg1
        let reglist = instr.RegList
        LSMMain dir ins ptrupdate reg1 reglist dataPath

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                       Branch                                                       //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Module contains B, BL Instruction Parse and Execution
///Module Depencences: CommonData,CommonLex,LS
///LS: 
///function splitIntoWords for general parsing
///function updateRec
///FlexOp2:
///function convExpToLit
module Branch=

    open CommonData
    open CommonLex
    open LS
    open FlexOp2

    //------------------------------Branch Instruction Type Definition--------------------//


    /// instruction type for branch 
    type BInstr =  {R14Val:uint32 Option; TargetAddr: uint32 Option}

    /// parse error, default used
    type ErrInstr = string


    //-----------------------------Identify Opcode----------------------------------//
    
    
    ///Branch Instruction Specs
    let bSpec = {
        InstrC = Branch
        Roots = ["B";"BL"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand bSpec


    //----------------------------Branch Parsing----------------------------//
    let parse (ls: LineData) : Result<Parse<BInstr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr 
            
            let targetSymb = splitIntoWords ls.Operands  [|' '|]  |>removeEmptyStr
            let target = 
                match targetSymb with
                |[target'] -> 
                    match ls.SymTab with
                    |Some symtab -> 
                        Result.map Some (convExp2Lit target' symtab)                   
                    |None -> Ok (None)
                |[] -> Error "No target"
                |_ -> Error "Too many target input" 

            let instrB = 
                match target with
                |Ok (Some addr) -> //Symbol Table present, and symbol found in table 
                    match root with
                    |"B" -> Ok {R14Val=None; TargetAddr =Some addr}
                    |"BL" ->Ok {R14Val =Some (la+4u); TargetAddr =Some addr}
                    |_ -> failwithf "will not happen"
                |Ok (None) -> 
                    match root with //Symbol Table absent
                    |"B" -> Ok {R14Val=None; TargetAddr = None}
                    |"BL" ->Ok {R14Val =Some (la+4u); TargetAddr = None}
                    |_ -> failwithf "will not happen"
                |Error k -> Error k //Error propagated

            match instrB with 
            |Ok (instrB' ) -> 
                Ok {
                    PInstr=instrB'; 
                    PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                    PSize = 4u; 
                    PCond = pCond 
                    }
            |Error k -> Error k            

        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse

    //--------------------------------------Branch Instruciton Execution------------------------------------//
    
    let execB (instr:BInstr) (dataPath:DataPath<'INS>) = 
    //change R15(PC) to target address
    //change R14 to address after BL if it is BL
        match instr.TargetAddr with
        |Some address -> 
            match instr.R14Val with
            |None -> 
                Ok {dataPath with Regs = updateRec dataPath.Regs [(R15, address+8u)]}
            |Some (r14Value) -> 
                let regUpdate = [(R15, address+8u);(R14,r14Value)]
                Ok {dataPath with Regs = updateRec dataPath.Regs regUpdate}
        |None -> failwithf "should not happen"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      CommonTop                                                     //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



///Common Top provided
///Slightly modified
///For convenience in testing, allows feeding same string input into visual and function 
module CommonTop =

    open CommonLex
    open CommonData

    /// allows different modules to return different instruction types
    type Instr =
        | ILSM of LSM.LSMInstr
        | ILS of LS.LSInstr
        | IB of Branch.BInstr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRILSM of LSM.ErrInstr
        | ERRILS of LS.ErrInstr
        | ERRIB of Branch.ErrInstr
        | ERRTOPLEVEL of string

    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible isntruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches
    let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | Branch.IMatch pa -> pConv IB ERRIB pa
        | LS.IMatch pa -> pConv ILS ERRILS pa
        | LSM.IMatch pa -> pConv ILSM ERRILSM pa
        | _ -> None
    

    type CondInstr = Condition * Instr

    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat " " operands
            Label=None
            LoadAddr = loadAddr
            SymTab = symtab
        }
        /// remove comments from string
        let removeComment (txt:string) =
            txt.Split(';')
            |> function 
                | [|x|] -> x 
                | [||] -> "" 
                | lineWithComment -> lineWithComment.[0]
        /// split line on whitespace into an array
        let splitIntoWords ( line:string ) =
            line.Split( ([||] : char array), 
                System.StringSplitOptions.RemoveEmptyEntries)
        /// try to parse 1st word, or 2nd word, as opcode
        /// If 2nd word is opcode 1st word must be label
        let matchLine words =
            let pNoLabel =
                match words with
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label} 
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        asmLine.ToUpper()
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine
    

        


        

            
