//////////////////////////////////////////////////////////////////////////////////////////
//                                    DCD/EQU/FILL
//////////////////////////////////////////////////////////////////////////////////////////
module EQUFILL

    open CommonData
    open CommonLex
    open TokenizeOperandsV3
    open LS
    open FlexOp2

    type EQUCode = EQU|FILL
    let opcodeMap = 
        Map.ofList ["EQU",EQU;"FILL",FILL]

    type Instr =  {Opcode:EQUCode;
                   Setflag:bool;
                   Express:uint32;
                   Cond:Condition
    }


    /// parse error (dummy, but will do)
    type ErrInstr = string

    let EQUSpec = {
        InstrC = EQUC
        Roots = ["EQU";"FILL"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand EQUSpec


    let FILLEQUparse (ls: LineData) : Result<Parse<Instr>,string> option =
        let (WA la) = ls.LoadAddr
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>splitStrIntoList|>ParseFILLEQUOps root ls
              match oprands with
              |Ok op -> 
                  Ok { 
                     PInstr={Opcode=opcodeMap.[root];
                             Setflag=suffix|> function|"S"->true|_->false;  
                             Express=op;
                             Cond = pCond
                     }; 
                     PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                     PSize = 4u; 
                     PCond = pCond }
               |Error k -> Error k

        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'
    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = FILLEQUparse
