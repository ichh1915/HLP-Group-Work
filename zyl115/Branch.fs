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
module Branch

open CommonData
open CommonLex
open LS


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


