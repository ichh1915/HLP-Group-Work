//////////////////////////////////////////////////////////////////////////////////////////
//                                    TST/TEQ
//////////////////////////////////////////////////////////////////////////////////////////
module TT2 

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open FlexOp2

    type TTCode = TST|TEQ
    let opcodeMap = 
        Map.ofList ["TST",TST;"TEQ",TEQ]

    type Instr =  {Opcode:TTCode;
                   Op1:RName;
                   Op2:Op2;
                   Cond : Condition
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

    let TTparse (ls: LineData) : Result<Parse<Instr>,string> option =
        let (WA la) = ls.LoadAddr
        let parse' (instrC, (root,suffix,pCond)) =
              let oprands = ls.Operands|>splitStrIntoList|>ParseTSTOps ls
              match oprands with
              |Ok op -> 
                  Ok { 
                     PInstr={Opcode=opcodeMap.[root];
                             Op1 = op.Op1;
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
    let (|IMatch|_|)  = TTparse

//////////////////////////////////////////////////////////////////////////////////////////
//                    TST/TEQ execution implementation modules
//////////////////////////////////////////////////////////////////////////////////////////   

    let updateFl (cpuData':DataPath<'INS>) (setC:bool option) (result:uint32):DataPath<'INS> =
        let checkC = match setC with
                     |None->cpuData'.Fl.C
                     |Some true->true
                     |Some false->false
        let checkN = if int32 result < 0 then true else false
        let checkZ = if int32 result = 0 then true else false
        { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=cpuData'.Regs ; MM = cpuData'.MM}
         

    let TestExecute (cpuData:DataPath<'INS>) (instr):DataPath<'INS> = 
        let rop1=cpuData.Regs.[instr.Op1]
        let setC = instr.Op2|>Op2SetCFlag cpuData
        let rop2 = instr.Op2|>fun k -> flexOp2 k cpuData
        let updateFlRegs'= updateFl cpuData setC
        
        match instr.Opcode with
        |TST -> (rop1 &&& rop2)|>updateFlRegs'|>PCPlus4
        |TEQ -> (rop1 ^^^ rop2)|>updateFlRegs'|>PCPlus4
