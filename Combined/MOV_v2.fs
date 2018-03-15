//////////////////////////////////////////////////////////////////////////////////////////
//                                    MOV/MVN
//////////////////////////////////////////////////////////////////////////////////////////
module MV2

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open SF2

    type MVCode = MOV| MVN
    let opcodeMap = 
        Map.ofList ["MOV",MOV;"MVN",MVN]

    type Instr =  {Opcode:MVCode;
                   Setflag:bool;
                   Rdest:RName;
                   Op2:FlexOp2;
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
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>tokenize|>ParseMOVOps 
              Ok { 
                 PInstr={Opcode=opcodeMap.[root];
                         Setflag=suffix|> function|"S"->true|_->false;  
                         Rdest=oprands.Dest;
                         Op2 = oprands.Op2;
                 }; 
                 PLabel = None ; 
                 PSize = 4u; 
                 PCond = pCond }

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



    let MovsExecute (cpuData:DataPath<'INS>) (instr:Parse<Instr>): DataPath<'INS> = 
        let setC = instr.PInstr.Op2|>Op2SetCFlag cpuData
        let rop2 = instr.PInstr.Op2|>FlexOp2 cpuData
        let updateFlRegs'= updateFlRegs cpuData instr.PInstr.Rdest instr.PInstr.Setflag setC
        match CheckCond cpuData instr.PCond with
        |true -> 
                match instr.PInstr.Opcode with
                |MOV -> rop2|>updateFlRegs'
                |MVN -> ~~~ rop2|>updateFlRegs'
        |false->
                cpuData