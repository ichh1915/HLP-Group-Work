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
module LSM

open CommonData
open CommonLex



//-------------------------------Type defined for LSM instructions--------------------//

///DU, Load or Store
type LORS = |L|S

type DIR = |FA|FD|EA|ED

/// instruction type for ldm and stm
type LSMInstr =  {Ins: LORS; Dir: DIR; Reg1:RName; RegList:RName list; PtrUpdate: bool;Cond:Condition}

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
            |((Some reg1,ptrUpdate),Some regList, "") -> Ok {Ins= ins; Dir=dir; Reg1=reg1; RegList=regList; PtrUpdate = ptrUpdate;Cond = pCond}
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

