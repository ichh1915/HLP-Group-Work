//////////////////////////////////////////////////////////////////////////////////////////
//                                    MOV/MVN
//////////////////////////////////////////////////////////////////////////////////////////
module MV2

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open LS
    open FlexOp2

    type MVCode = MOV| MVN
    let opcodeMap = 
        Map.ofList ["MOV",MOV;"MVN",MVN]

    type Instr =  {Opcode:MVCode;
                   Setflag:bool;
                   Rdest:RName;
                   Op2:Op2;
                   Cond :Condition
    }


    /// parse error (dummy, but will do)
    type ErrInstr = string

    let MVSpec = {
        InstrC = MOVC
        Roots = ["MOV";"MVN"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand MVSpec

    let MVparse (ls: LineData) : Result<Parse<Instr>,string> option =
        let (WA la) = ls.LoadAddr
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>splitStrIntoList|>ParseMOVOps ls
              match oprands with
              |Ok op -> 
                  Ok { 
                     PInstr={Opcode=opcodeMap.[root];
                             Setflag=suffix|> function|"S"->true|_->false;  
                             Rdest=op.Dest;
                             Op2 = op.Op2;
                             Cond = pCond
                     }; 
                     PLabel = ls.Label |> Option.map (fun lab -> lab, la) ;
                     PSize = 4u; 
                     PCond = pCond }
               |Error k -> Error k

        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'
    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = MVparse

//////////////////////////////////////////////////////////////////////////////////////////
//                   MOV/MVN execution implementation modules
//////////////////////////////////////////////////////////////////////////////////////////   

    let updateFlRegs (cpuData':DataPath<'INS>) (rdest:RName) (setF:bool) (setC:bool option) (result:uint32):DataPath<'INS> =
        let checkC = match setC with
                     |None->cpuData'.Fl.C
                     |Some true->true
                     |Some false->false
        let checkN = if int32 result < 0 then true else false
        let checkZ = if int32 result = 0 then true else false
        match setF with
            |true ->  { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=Map.add rdest result cpuData'.Regs ; MM = cpuData'.MM}
            |false -> { Fl=cpuData'.Fl; Regs=Map.add rdest result cpuData'.Regs; MM = cpuData'.MM }  



    let MovsExecute (cpuData:DataPath<'INS>) (instr): DataPath<'INS> = 
        let setC = instr.Op2|>Op2SetCFlag cpuData
        let rop2 = 
            match instr.Rdest with
            |R15 -> instr.Op2|> fun k -> (flexOp2 k cpuData)+4u
            |_ -> instr.Op2|> fun k -> (flexOp2 k cpuData)
        let updateFlRegs'= updateFlRegs cpuData instr.Rdest instr.Setflag setC
        
        match instr.Opcode with
        |MOV -> rop2|>updateFlRegs' |>PCPlus4
        |MVN -> ~~~ rop2|>updateFlRegs'|>PCPlus4
        