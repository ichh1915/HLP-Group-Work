//////////////////////////////////////////////////////////////////////////////////////////
//                               LSL/LSR/ASR/ROR/RRX
//////////////////////////////////////////////////////////////////////////////////////////
module SF 

    open CommonData
    open CommonLex
    open TokenizeOperands

    type ShiftCode = |LSL |LSR |ASR |ROR |RRX

    let opcodeMap = 
        Map.ofList ["LSL",LSL;"LSR",LSR;"ASR",ASR;"ROR",ROR;"RRX",RRX]

    //Instrution
    type Instr =  {Opcode:ShiftCode ;
                   Setflag:bool;
                   Rdest:RName;
                   Op1:uint32;
                   Op2:uint32;
    }

    type ErrInstr = string

    let SFSpec = {
        InstrC = SFTC
        Roots = ["LSL";"LSR";"ASR";"ROR";"RRX"]
        Suffixes = [""; "S"]
    }

    /// map of all possible opcodes recognised
    let opCodes = opCodeExpand SFSpec

    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)
    let SFparse (cpuData: DataPath) (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr // address this instruction is loaded into memory

            ///string of all operands->{Dest:RName; Op1:Uint32 option; op2:uint32}
            let operands = ls.Operands|>tokenize|>ParseSHIFTOps cpuData  
            Ok { 
               PInstr= {
                         Opcode= opcodeMap.[root];
                         Setflag= suffix|> function|"S"->true|_->false;                                                    
                         Rdest= operands.Dest;                                               
                         Op1= operands.Op1;
                         Op2=operands.Op2;
                }; 
                PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 
                PSize = 4u; 
                PCond = pCond;           
                }

        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = SFparse

    //Check if Flags matchs Condition   
    let CheckCond (cpuData:DataPath) (cond:Condition): bool = 
        match cond with
        |Ceq when cpuData.Fl.Z=true-> true
        |Cne when cpuData.Fl.Z=false-> true
        |Cmi when cpuData.Fl.N=true-> true
        |Cpl when cpuData.Fl.N=false-> true

        |Cvs when cpuData.Fl.V=true-> true
        |Cvc when cpuData.Fl.V=false-> true
        |Chs when cpuData.Fl.C=true-> true
        |Clo when cpuData.Fl.C=false-> true

        |Chi when cpuData.Fl.C=true && cpuData.Fl.Z=false-> true
        |Cls when cpuData.Fl.C=false || cpuData.Fl.Z=true-> true
        |Cge when cpuData.Fl.N = cpuData.Fl.V-> true
        |Clt when cpuData.Fl.N <> cpuData.Fl.V-> true

        |Cgt when cpuData.Fl.Z=false && cpuData.Fl.N = cpuData.Fl.V-> true
        |Cle when cpuData.Fl.Z=true || cpuData.Fl.N <> cpuData.Fl.V-> true
        |Cnv-> false
        |Cal-> true

        |_-> false


    //Update Fl and Regs after an execution
    let updateFlRegs (cpuData':DataPath) (instr:ShiftCode) (rdest:RName) (op1:uint32) (op2:uint32) (setF:bool) (result:uint32):DataPath =
        let checkN = if int32 result < 0 then true else false
        let checkZ = if int32 result =  0 then true else false
        let checkC =
            match instr with
            |LSL -> 
                    if int32 op2>32 then false
                    else if int32 op2 = 0 then cpuData'.Fl.C 
                    else if int32 (op1<<<(int32 op2-1)) < 0  then true else false
            |LSR ->
                    if int32 op2>32 then false 
                    else if int32 op2 = 0 then cpuData'.Fl.C
                    else if int32 (op1>>>(int32 op2-1)) &&& 1 = 1 then true else false
            |ASR ->
                    if int32 op2>32 && int32(op1) < 0 then true 
                    else if int32 op2>32 && int32(op1) >= 0 then false
                    else if int32 op2 = 0 then cpuData'.Fl.C
                    else if int32 (op1>>>(int32 op2-1)) &&& 1 = 1 then true else false
            |ROR -> 
                    let int32 op2 = int32 op2 % 32
                    if int32 op2 = 0 then cpuData'.Fl.C 
                    else if int32 (op1>>>(int32 op2-1)) &&& 1 = 1 then true else false //note here shift is in the range 0-31
            |RRX -> if (int32 op1) &&& 1 = 1 then true else false
        match setF with
            |true ->  { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=Map.add rdest result cpuData'.Regs }
            |false -> { Fl=cpuData'.Fl; Regs=Map.add rdest result cpuData'.Regs }  


    //Execute Shift Operations
    let ShiftExecute (cpuData:DataPath) (instr:Parse<Instr>) : DataPath = 
        let updateFlRegs' = updateFlRegs cpuData instr.PInstr.Opcode instr.PInstr.Rdest instr.PInstr.Op1 instr.PInstr.Op2 instr.PInstr.Setflag 

        //For the case Rd and Op1 are the same register
        let op1BeforeExecuting = instr.PInstr.Op1

        //Only the least significant 5 bits of the literal are used for shift
        let modulo32 op2 = int32 (op2<<<27)>>>27

        match CheckCond cpuData instr.PCond with
        |true->
               match instr.PInstr.Opcode with
               |LSL-> if instr.PInstr.Op2 > 31u then 0u |> updateFlRegs' 
                      else (instr.PInstr.Op1) <<< modulo32 instr.PInstr.Op2 |> updateFlRegs'
                   
               |LSR-> if instr.PInstr.Op2 > 31u then 0u |> updateFlRegs'
                      else (instr.PInstr.Op1) >>> modulo32 instr.PInstr.Op2 |> updateFlRegs'

               |ASR-> if instr.PInstr.Op2 > 31u then 0u |> updateFlRegs' 
                      else uint32(int32(instr.PInstr.Op1) >>> modulo32 instr.PInstr.Op2) |> updateFlRegs'

               |ROR-> ( (instr.PInstr.Op1<<<(32-modulo32 instr.PInstr.Op2)) ||| (instr.PInstr.Op1>>>modulo32 instr.PInstr.Op2))|> updateFlRegs'

               |RRX->match cpuData.Fl.C with
                     |true -> ( instr.PInstr.Op1>>>1 ||| uint32 (1<<<31) ) |> updateFlRegs'
                     |false -> instr.PInstr.Op1>>>1 |> updateFlRegs'
        |false-> 
                cpuData
                  
             


                                                      

