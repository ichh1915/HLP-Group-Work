////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                       ADR                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////


module ADR

open CommonData
open CommonLex
open FlexOp2

//------------------------------ADR Instruction Type Definition--------------------//



/// instruction type for ADR 
type Instr =  {Reg:RName;Value:uint32;Cond:Condition}

/// parse error, default used
type ErrInstr = string


//-----------------------------Identify Opcode----------------------------------//


///ADR Instruction Specs
let bSpec = {
    InstrC = ADR
    Roots = ["ADR"]
    Suffixes = [""]
}

/// map of all possible opcodes recognised
let opCodes = opCodeExpand bSpec


//----------------------------ADR Parsing----------------------------//
let parse (ls: LineData) : Result<Parse<Instr>,string> option =
    let parse' (instrC, (root,suffix,pCond)) =
        let (WA la) = ls.LoadAddr
        let strList = splitStrIntoList ls.Operands
        let (reg,addr) = 
            match strList with
            |reg::[addr] -> 
                let reg' = Map.tryFind reg regNames
                let addr' = 
                    match ls.SymTab with
                    |None -> 0u|>Some
                    |Some symtab -> 
                        let num = convExp2LitNoCheck addr symtab
                        match num with
                        |Ok k -> Some k
                        |Error _ -> None
                (reg',addr')
            |_ -> (None,None)

        let instr =    
            match (reg,addr) with
            |(Some reg', Some addr') -> Ok {Reg = reg';  Value = addr';Cond = pCond}
            |(None,_) -> Error "Dest error"
            |(_,None) -> Error "Expression Error"
       
        match instr with 
        |Ok (instr' ) -> 
            Ok {
                PInstr=instr'; 
                PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                PSize = 4u; 
                PCond = pCond 
                }
        |Error k -> Error k    

    Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
    |> Option.map parse' // if unknown keep none, if known parse it.


/// Parse Active Pattern used by top-level code
let (|IMatch|_|) = parse

let exec (instr:Instr) (dataPath:DataPath<'INS>)= 
    let destReg = instr.Reg
    let pcVal = dataPath.Regs.[R15]-4u
    let addr = instr.Value
    let diff = addr-pcVal
    let validity = diff |> checkLitValidity
    match validity with
    |true -> Ok (PCPlus4{dataPath with Regs = dataPath.Regs.Add (destReg,addr)})
    |false -> Error "Offset out of range for address load"