//////////////////////////////////////////////////////////////////////////////////////////
//                                    TST/TEQ
//////////////////////////////////////////////////////////////////////////////////////////
module TT2 

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open SF2

    type TTCode = TST|TEQ
    let opcodeMap = 
        Map.ofList ["TST",TST;"TEQ",TEQ]

    type Instr =  {Opcode:TTCode;
                   Op1:RName;
                   Op2:FlexOp2;
    }


    /// parse error (dummy, but will do)
    type ErrInstr = string

    let MVSpec = {
        InstrC = TSTC
        Roots = ["TST";"TEQ"]
        Suffixes = [""]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand MVSpec

    let TTparse (cpuData:DataPath) (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>tokenize|>ParseTSTOps 
              Ok { 
                 PInstr={Opcode=opcodeMap.[root];
                         Op1 = oprands.Op1;
                         Op2 = oprands.Op2;
                 }; 
                 PLabel = None ; 
                 PSize = 4u; 
                 PCond = pCond }

        Map.tryFind ls.OpCode opCodes
        |> Option.map parse'
    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|)  = TTparse

//////////////////////////////////////////////////////////////////////////////////////////
//                    TST/TEQ execution implementation modules
//////////////////////////////////////////////////////////////////////////////////////////   

    let updateFl (cpuData':DataPath) (setC:bool option) (result:uint32):DataPath =
        let checkC = match setC with
                     |None->cpuData'.Fl.C
                     |Some true->true
                     |Some false->false
        let checkN = if int32 result < 0 then true else false
        let checkZ = if int32 result = 0 then true else false
        { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=cpuData'.Regs }
         

    let TestExecute (cpuData:DataPath) (instr:Parse<Instr>):DataPath = 
        let rop1=cpuData.Regs.[instr.PInstr.Op1]
        let setC = instr.PInstr.Op2|>Op2SetCFlag cpuData
        let rop2 = instr.PInstr.Op2|>FlexOp2 cpuData
        let updateFlRegs'= updateFl cpuData setC
        match CheckCond cpuData instr.PCond with
        |true -> 
                match instr.PInstr.Opcode with
                |TST -> (rop1 &&& rop2)|>updateFlRegs'
                |TEQ -> (rop1 ^^^ rop2)|>updateFlRegs'
        |false->
                cpuData