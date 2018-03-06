//////////////////////////////////////////////////////////////////////////////////////////
//                                 AND/EOR/BIC/ORR
//////////////////////////////////////////////////////////////////////////////////////////
module BT 

    open CommonData
    open CommonLex
    open TokenizeOperands
    open SF

    type BTCode = AND|EOR|BIC|ORR
    let opcodeMap = 
        Map.ofList ["AND",AND;"EOR",EOR;"BIC",BIC;"ORR",ORR]

    type Instr =  {Opcode:BTCode;
                   Setflag:bool;
                   Rdest:RName;
                   Op1:uint32;
                   Op2:uint32;
                   SetC:bool option;
    }

    type ErrInstr = string

    let MVSpec = {
        InstrC = BITC
        Roots = ["AND";"EOR";"BIC";"ORR"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand MVSpec

    let BTparse (cpuData:DataPath) (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>tokenize|>ParseBITOps cpuData
              Ok { 
                 PInstr={Opcode=opcodeMap.[root];
                         Setflag=suffix|> function|"S"->true|_->false;  
                         Rdest=oprands.Dest;
                         Op1 = oprands.Op1;
                         Op2 = oprands.Op2;
                         SetC = oprands.SetC;
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

    let updateFlRegs (cpuData':DataPath) (rdest:RName) (setF:bool) (setC:bool option) (result:uint32):DataPath =
        let checkC = match setC with
                     |None->cpuData'.Fl.C
                     |Some true->true
                     |Some false->false
        let checkN = if int32 result < 0 then true else false
        let checkZ = if int32 result = 0 then true else false
        match setF with
            |true ->  { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=Map.add rdest result cpuData'.Regs }
            |false -> { Fl=cpuData'.Fl; Regs=Map.add rdest result cpuData'.Regs }  



    let BitwiseExecute (cpuData:DataPath) (instr:Parse<Instr>): DataPath = 
        let updateFlRegs'= updateFlRegs cpuData instr.PInstr.Rdest instr.PInstr.Setflag instr.PInstr.SetC
        match CheckCond cpuData instr.PCond with
        |true -> 
                match instr.PInstr.Opcode with
                |AND -> instr.PInstr.Op1 &&& instr.PInstr.Op2|>updateFlRegs'
                |EOR -> instr.PInstr.Op1 ^^^ instr.PInstr.Op2|>updateFlRegs'
                |BIC -> instr.PInstr.Op1 &&& (~~~instr.PInstr.Op2)|>updateFlRegs'
                |ORR -> instr.PInstr.Op1 ||| instr.PInstr.Op2|>updateFlRegs'
        |false->
                cpuData