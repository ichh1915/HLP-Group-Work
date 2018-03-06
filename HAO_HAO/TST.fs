//////////////////////////////////////////////////////////////////////////////////////////
//                                    TST/TEQ
//////////////////////////////////////////////////////////////////////////////////////////
module TT 

    open CommonData
    open CommonLex
    open TokenizeOperands
    open SF

    type TTCode = TST|TEQ
    let opcodeMap = 
        Map.ofList ["TST",TST;"TEQ",TEQ]

    type Instr =  {Opcode:TTCode;
                   Op1:uint32;
                   Op2:uint32;
                   SetC:bool option;
    }

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
              let oprands = ls.Operands|>tokenize|>ParseTSTOps cpuData
              Ok { 
                 PInstr={Opcode=opcodeMap.[root];
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
        let updateFlRegs'= updateFl cpuData instr.PInstr.SetC
        match CheckCond cpuData instr.PCond with
        |true -> 
                match instr.PInstr.Opcode with
                |TST -> (instr.PInstr.Op1 &&& instr.PInstr.Op2)|>updateFlRegs'
                |TEQ -> (instr.PInstr.Op1 ^^^ instr.PInstr.Op2)|>updateFlRegs'
        |false->
                cpuData