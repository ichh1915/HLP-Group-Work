//////////////////////////////////////////////////////////////////////////////////////////
//                               LSL/LSR/ASR/ROR/RRX
//////////////////////////////////////////////////////////////////////////////////////////
module SF2 

    open CommonData
    open CommonLex
    open TokenizeOperandsV2
    open LS
    open FlexOp2

    type ShiftCode = |LSL |LSR |ASR |ROR |RRX

    let opcodeMap = 
        Map.ofList ["LSL",LSL;"LSR",LSR;"ASR",ASR;"ROR",ROR;"RRX",RRX]

    //Instrution
    type Instr =  {Opcode:ShiftCode ;
                   Setflag:bool;
                   Rdest:RName;
                   Op1:RName;
                   Op2:Op2 option;
                   Cond:Condition
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
    let SFparse (ls: LineData) : Result<Parse<Instr>,string> option =
        let parse' (instrC, (root,suffix,pCond)) =

            let (WA la) = ls.LoadAddr // address this instruction is loaded into memory

            ///string of all operands->{Dest:RName; Op1:Uint32 option; op2:uint32}
            let operands = ls.Operands|>splitStrIntoList|>ParseSHIFTOps root ls
            match operands with 
            |Ok op -> 
                 
                Ok { 
                   PInstr= {
                             Opcode= opcodeMap.[root];
                             Setflag= suffix|> function|"S"->true|_->false;                                                    
                             Rdest= op.Dest;                                               
                             Op1=op.Op1;
                             Op2=op.Op2;
                             Cond = pCond
                    }; 
                PLabel = ls.Label |> Option.map (fun lab -> lab, la) ; 
                PSize = 4u; 
                PCond = pCond;           
                }
            |Error k -> Error k

        Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
        |> Option.map parse' // if unknown keep none, if known parse it.

    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = SFparse

    //Check if Flags matchs Condition   
    let CheckCond (cpuData:DataPath<'INS>) (cond:Condition): bool = 
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
    let updateFlRegs (cpuData':DataPath<'INS>) (instr:ShiftCode) (rdest:RName) (op1:uint32) (op2:uint32) (setF:bool) (result:uint32):DataPath<'INS> =
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
            |true->  { Fl={N=checkN; Z=checkZ; C=checkC; V=cpuData'.Fl.V}; Regs=Map.add rdest result cpuData'.Regs ; MM = cpuData'.MM}
            |false -> { Fl=cpuData'.Fl; Regs=Map.add rdest result cpuData'.Regs ; MM = cpuData'.MM}  
            

    //Execute Shift Operations
    let ShiftExecute (cpuData:DataPath<'INS>) (instr) : DataPath<'INS> = 
        let r1=cpuData.Regs.[instr.Op1]
        let r2=
            match instr.Op2 with
            |Some op2 -> flexOp2 op2 cpuData
            |None -> 0u
        let updateFlRegs' = updateFlRegs cpuData instr.Opcode instr.Rdest r1 r2 instr.Setflag 

        //For the case Rd and Op1 are the same register
        let op1BeforeExecuting = instr.Op1

        //Only the least significant 5 bits of the literal are used for shift
        let modulo32 op2 = int32 (op2<<<27)>>>27


        match instr.Opcode with
        |LSL-> if r2 > 31u then 0u |> updateFlRegs' |>PCPlus4
                else (r1) <<< modulo32 r2 |> updateFlRegs'|>PCPlus4
               
        |LSR-> if r2 > 31u then 0u |> updateFlRegs'|>PCPlus4
                  else (r1) >>> modulo32 r2 |> updateFlRegs'|>PCPlus4

        |ASR-> if r2 > 31u then 0u |> updateFlRegs' |>PCPlus4
                  else uint32(int32(r1) >>> modulo32 r2) |> updateFlRegs'|>PCPlus4

        |ROR-> ( (r1<<<(32-modulo32 r2)) ||| (r1>>>modulo32 r2))|> updateFlRegs'|>PCPlus4

        |RRX->match cpuData.Fl.C with
                 |true -> ( r1>>>1 ||| uint32 (1<<<31) ) |> updateFlRegs'|>PCPlus4
                 |false -> r1>>>1 |> updateFlRegs'|>PCPlus4

              
         


                                              

