//////////////////////////////////////////////////////////////////////////////////////////
//                                 AND/EOR/BIC/ORR
//////////////////////////////////////////////////////////////////////////////////////////
module BT2 

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open LS
    

    type BTCode = AND|EOR|BIC|ORR
    let opcodeMap = 
        Map.ofList ["AND",AND;"EOR",EOR;"BIC",BIC;"ORR",ORR]

    type Instr =  {Opcode:BTCode;
                   Setflag:bool;
                   Rdest:RName;
                   Op1:RName;
                   Op2:FlexOp2;
                   Cond :Condition
    }


    /// parse error (dummy, but will do)
    type ErrInstr = string

    let MVSpec = {
        InstrC = BITC
        Roots = ["AND";"EOR";"BIC";"ORR"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand MVSpec

    let BTparse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>tokenize|>ParseBITOps 
              Ok { 
                 PInstr={Opcode=opcodeMap.[root];
                         Setflag=suffix|> function|"S"->true|_->false;  
                         Rdest=oprands.Dest;
                         Op1 = oprands.Op1;
                         Op2 = oprands.Op2;
                         Cond = pCond
                 }; 
                 PLabel = None ; 
                 PSize = 4u; 
                 PCond = pCond }

        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'
    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = BTparse

//////////////////////////////////////////////////////////////////////////////////////////
//                    MOV/MVN execution implementation modules
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



    let BitwiseExecute (cpuData:DataPath<'INS>) (instr): DataPath<'INS> = 
        let rop1=cpuData.Regs.[instr.Op1]
        let setC = instr.Op2|>Op2SetCFlag cpuData
        let rop2 = instr.Op2|>FlexOp2 cpuData
        let updateFlRegs'= updateFlRegs cpuData instr.Rdest instr.Setflag setC
        
        match instr.Opcode with
        |AND -> rop1 &&& rop2|>updateFlRegs' |>PCPlus4
        |EOR -> rop1 ^^^ rop2|>updateFlRegs' |>PCPlus4
        |BIC -> rop1 &&& (~~~rop2)|>updateFlRegs'|>PCPlus4
        |ORR -> rop1 ||| rop2|>updateFlRegs'|>PCPlus4
        