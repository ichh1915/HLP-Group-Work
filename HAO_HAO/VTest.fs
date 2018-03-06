namespace VisualTest

module VTest =

    open Expecto
    open Expecto.ExpectoFsCheck

    open CommonData
    open CommonLex
    open EEExtensions
    open TokenizeOperands
    open SF
    open BT
    open MV
    open TT

    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO

    /// parameters setting up the testing framework
    /// WARNING: PostludeLength must be changed if Postlude is changed
    /// WARNING: global cache (CacheFileName) must be deleted if Postlude is changed
    /// Postlude can contain instructions to move CPU state (flags, memory locations) into rgeisters
    /// standard Postlude moves flags into R1
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    let defaultParas = {
        Parallel = false              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"../visualapp/visual/"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"../VisualWork/"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"../VisualWork/Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=false}
        InitRegs = [0u..10u..140u]          // initial values of registers R0..R14
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    
    
    /// run an expecto test of VisUAL
    /// name - name of test
    ///
    let VisualUnitTest paras name src (flagsExpected:string) (outExpected: (Out * int) list) :Test =
        testCase name <| fun () ->
            let flagsActual, outActual = RunVisualWithFlagsOut paras src
            Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            //output Regs specified 
            let outRegsNoted = 
                outExpected 
                |> List.map fst

            //actual output Regs as specified
            let outActualNoted =  
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal outActualNoted (outExpected |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src

    let VisualFrameworkTest paras =
        testCase "Framework test failed" <| fun () ->
            let parasExpected = 
                paras.InitRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = RunVisualWithFlagsOut paras ""
            let outSorted = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            Expecto.Expect.equal flagsActual  paras.InitFlags "Status flags don't match"
            Expecto.Expect.equal outSorted parasExpected <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs" outActual.Regs


    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      

    let VisualFrameworkRun (regs: rType,flags:Flags) =
        let performTest() =
            let initRegs = 
                rType2List regs
                |> List.map uint32
        
            let expectedRegs =
                initRegs
                |> List.indexed
                |> List.map (fun (n,v) -> R n, int v)

            let flagsActual, outActual = 
                    RunVisualWithFlagsOut { 
                        defaultParas with 
                            InitFlags=flags;
                            InitRegs=initRegs
                        } ""
            let actualRegs = 
                outActual.Regs
                |> List.sort
                |> List.take 15
            let flagsOK = flagsActual = flags
            let regsOK = actualRegs = expectedRegs 
            if not flagsOK then 
                printfn "Framework error: Bad flags: %A" flagsActual
                System.Console.ReadKey() |> ignore
            if not regsOK then 
                printfn "Framework error: Bad registers %A" actualRegs
                System.Console.ReadKey() |> ignore
            flagsOK && regsOK
        match flags with
        | {FN=true;FZ=true} -> true // prevent test with impossible input
        | _ -> performTest()
            
    let testParas = defaultParas
 

    let vTest = VisualUnitTest testParas

   
    /// to test the testbench, create many tests with assembler
    /// this is enough for each test to need being run separately
    
    let manyTests n = 
        [0..n] 
        |> List.map (fun n -> 
            let n' = 1 + (n % 254)
            vTest (sprintf "SUBS%d test" n') (sprintf "SUBS R0, R0, #%d" n') "1000" [R 0, -n'])


         
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//       Unit tests with ramdomized input states: 
//       checking when given the same input states to F# and visUAL, they produce the same output states
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

    ///Generate random initial registers and initial flags
    type System.Random with
        member this.GetValues(minValue, maxValue) =
            Seq.initInfinite (fun _ -> this.Next(minValue, maxValue))
    let r = System.Random()

    //Randomly generated initial registers
    let randRegs = r.GetValues(0, 251658239) |> Seq.take 15 |>Seq.toList|>List.map (fun a->uint32 a)

    //Randomly generated initial flags
    let filterNV flg = match flg with |{FN=true; FZ=true; FC=_; FV=_}->failwithf "N and V cannot be the same!" |_->flg
    let randFlags = r.GetValues(0,2) |>Seq.take 4 |>Seq.toList |>List.map(fun a-> match a with |0->false |1->true |_->false) 
                                                               |>function|[n;z;c;v]->{FN=n;FZ=z;FC=c;FV=v} |_->failwithf "flag error"
                                                               |>filterNV

    let defaultParas' = {
        Parallel = false              
        MaxConcurrentVisualDirs = 10 
        Cached = true                

        VisualPath =  
            @"../visualapp/visual/"  
        WorkFileDir = 
            @"../VisualWork/"
        CacheFileName = 
            @"../VisualWork/Cache"   
        CacheLimit = 10               
        InitFlags = randFlags
        InitRegs = randRegs
       
        MemReadBase = 0x1000u
        Postlude = ""                 
        Prelude = ""                  
    } 

    type testType = |SFT |MVT |BTT |TTT

    let unitTests (testtype:testType) (name:string) (paras:Params) (opcode:string) (oprands:string) = 
        testCase name <| fun () ->
            let (getDatapath:DataPath) = {Fl=(paras.InitFlags|>function {FN=bn; FZ=bz; FC=bc; FV=bv}->{N=bn;Z=bz;C=bc;V=bv});
                                        Regs=(paras.InitRegs
                                              |>function 
                                                |[v0;v1;v2;v3;v4;v5;v6;v7;v8;v9;v10;v11;v12;v13;v14]
                                                  ->[R0,v0;R1,v1;R2,v2;R3,v3;R4,v4;R5,v5;R6,v6;R7,v7;R8,v8;R9,v9;R10,v10;R11,v11;R12,v12;R13,v13;R14,v14]
                                                    |>Map.ofList
                                                |_->failwithf "Invalid initial regiter"
                                              )      
                                        }
            let getLineData = {LoadAddr=WA (uint32 0); Label=None; SymTab=None; OpCode=opcode; Operands=oprands}

            //F# assembler output
            let resultcpuData = 
                match testtype with
                |SFT -> SFparse getDatapath getLineData 
                        |> function |Some (Ok a)->a |Some (Error _)-> failwithf "Error" |None -> failwithf "Error"
                        |> ShiftExecute getDatapath
                |MVT -> MVparse getDatapath getLineData 
                        |> function |Some (Ok a)->a |Some (Error _)-> failwithf "Error" |None -> failwithf "Error"
                        |> MovsExecute getDatapath
                |BTT -> BTparse getDatapath getLineData 
                        |> function |Some (Ok a)->a |Some (Error _)-> failwithf "Error" |None -> failwithf "Error"
                        |> BitwiseExecute getDatapath
                |TTT -> TTparse getDatapath getLineData 
                        |> function |Some (Ok a)->a |Some (Error _)-> failwithf "Error" |None -> failwithf "Error"
                        |> TestExecute getDatapath                 
            let testMap = resultcpuData.Regs

            //visUAL output
            let flagsActual, outActual = RunVisualWithFlagsOut paras (opcode+" "+oprands)   
            let VisMap = outActual.Regs |> List.map (fun (R a,b)->(register a,uint32 b)) |> Map.ofList |>Map.remove R15

            Expecto.Expect.equal testMap VisMap "Regs"    
            Expecto.Expect.equal resultcpuData.Fl (flagsActual |>function {FN=bn; FZ=bz; FC=bc; FV=bv}->{N=bn;Z=bz;C=bc;V=bv}) "Flags" 

    [<Tests>]  
    let uinttest= 
        Expecto.Tests.testList "unit execution tests"
            [
               //unitTests SFT "SF1_0" defaultParas' "LSRS" "R2,R1,#0x44"  //Expected to fail (visUAL does not restrict the shift length to be 0-31)
               //unitTests SFT "SF1_1" defaultParas' "LSRS" "R2,R1,#100"   //Expected to fail (visUAL does not restrict the shift length to be 0-31)
               unitTests SFT "SF1_2" defaultParas' "LSRS" "R2,R1,#0x1A"
               unitTests SFT "SF1_3" defaultParas' "LSRS" "R1,R1,R14"
               unitTests SFT "SF2_0" defaultParas' "LSLS" "R3,R4,#31"
               unitTests SFT "SF2_1" defaultParas' "LSLS" "R5,R5,#0x1C"
               unitTests SFT "SF2_2" defaultParas' "LSLS" "R5,R6,R5"
               unitTests SFT "SF2_3" defaultParas' "LSLS" "R7,R7,R9"
               unitTests SFT "SF3_0" defaultParas' "RORS" "R7,R9,#30"
               unitTests SFT "SF3_1" defaultParas' "RORS" "R8,R10,#0x18"
               unitTests SFT "SF3_2" defaultParas' "RORS" "R11,R11,R8"
               unitTests SFT "SF3_3" defaultParas' "RORS" "R12,R12,R5"
               unitTests SFT "SF4_0" defaultParas' "RRXS" "R2,R2"
               unitTests SFT "SF4_1" defaultParas' "RRXS" "R3,R4"
               unitTests SFT "SF4_2" defaultParas' "RRXS" "R5,R6"
               unitTests SFT "SF4_3" defaultParas' "RRXS" "R7,R8"
               unitTests SFT "SF5_0" defaultParas' "ASRS" "R6,R9,#31"
               unitTests SFT "SF5_1" defaultParas' "ASRS" "R5,R12,#20"
               unitTests SFT "SF5_2" defaultParas' "ASRS" "R11,R11,R4"
               unitTests SFT "SF5_3" defaultParas' "ASRS" "R3,R10,R7"

               unitTests BTT "BIT1_0" defaultParas' "ANDS" "R1,R1,R3,LSR #0x1D"
               unitTests BTT "BIT1_1" defaultParas' "ANDS" "R4,R1,R3,LSL R8"  
               unitTests BTT "BIT1_1_2" defaultParas' "ANDS" "R1,R1,R3,ROR R9"  
               unitTests BTT "BIT1_2" defaultParas' "AND" "R1,R2,R3"
               unitTests BTT "BIT1_3" defaultParas' "ANDS" "R1,R1,#0xFF"

               //unitTests BTT "BIT2_0" defaultParas' "ORRS" "R1,R1,R3,RRX #1"   Expected to have error, the syntax for visUAL omitted #1
               unitTests BTT "BIT2_1" defaultParas' "ORRS" "R4,R1,R3,LSR R8"  
               unitTests BTT "BIT2_2" defaultParas' "ORRS" "R1,R1,R3,ASR R9"  
               unitTests BTT "BIT2_3" defaultParas' "ORRS" "R1,R2,R3"
               unitTests BTT "BIT2_4" defaultParas' "ORRS" "R1,R1,#0xFF"
               unitTests BTT "BIT3_0" defaultParas' "BICS" "R12,R7,R5,LSR #0x1F"
               unitTests BTT "BIT3_1" defaultParas' "BICS" "R4,R1,R3,ASR R8"  
               unitTests BTT "BIT3_2" defaultParas' "BICS" "R1,R1,R3,LSL R9"  
               unitTests BTT "BIT3_3" defaultParas' "BICS" "R1,R2,R3"
               unitTests BTT "BIT3_4" defaultParas' "BICS" "R1,R1,#0xFF"
               unitTests BTT "BIT4" defaultParas' "EORS" "R10,R2,R7,ROR #0x1F"
               unitTests BTT "BIT4_1" defaultParas' "EORS" "R4,R1,R3,LSR R8"  
               unitTests BTT "BIT4_2" defaultParas' "EORS" "R1,R1,R3,LSR R9"  
               unitTests BTT "BIT4_3" defaultParas' "EORS" "R1,R2,R3"
               unitTests BTT "BIT4_4" defaultParas' "EORS" "R1,R1,#0xFF"

               unitTests MVT "MOV1_0" defaultParas' "MOVS" "R1,R1,LSL #4"
               unitTests MVT "MOV1_1" defaultParas' "MOVS" "R1,R1,ASR R6"
               unitTests MVT "MOV1_2" defaultParas' "MOVS" "R1,R1,LSL #4"
               unitTests MVT "MOV1_3" defaultParas' "MOVS" "R1,R1,LSL #4"
               unitTests MVT "MOV2_0" defaultParas' "MVNS" "R5,R10,ROR R6"

               unitTests TTT "TST1" defaultParas' "TST" "R1,R2,ROR #20"
               unitTests TTT "TST2" defaultParas' "TST" "R1,R2,LSR R10"
               unitTests TTT "TST3" defaultParas' "TST" "R1,R2,ASR #25"
               unitTests TTT "TST4" defaultParas' "TST" "R1,R2,LSL #15"
               unitTests TTT "TST5" defaultParas' "TEQ" "R5,R10,ROR #31"  

               unitTests MVT "EQ" defaultParas' "MVNSEQ" "R5,R10,ROR R6"
               unitTests MVT "NE" defaultParas' "MVNSNE" "R5,R10,ROR R6"
               unitTests MVT "MI" defaultParas' "MVNSMI" "R5,R10,ROR R6"
               unitTests MVT "PL" defaultParas' "MVNSPL" "R5,R10,ROR R6"
               unitTests MVT "HI" defaultParas' "MVNSHI" "R5,R10,ROR R6"
               unitTests MVT "HS" defaultParas' "MVNSHS" "R5,R10,ROR R6"
               unitTests MVT "LO" defaultParas' "MVNSLO" "R5,R10,ROR R6"
               unitTests MVT "LS" defaultParas' "MVNSLS" "R5,R10,ROR R6"
               unitTests MVT "GE" defaultParas' "MVNSGE" "R5,R10,ROR R6"
               unitTests MVT "GT" defaultParas' "MVNSGT" "R5,R10,ROR R6"
               unitTests MVT "LE" defaultParas' "MVNSLE" "R5,R10,ROR R6"
               unitTests MVT "LT" defaultParas' "MVNSLT" "R5,R10,ROR R6"
               unitTests MVT "VS" defaultParas' "MVNSVS" "R5,R10,ROR R6"
               unitTests MVT "VC" defaultParas' "MVNSVC" "R5,R10,ROR R6"
               unitTests MVT "AL" defaultParas' "MVNSAL" "R5,R10,ROR R6"
               //unitTests MVT "NV" defaultParas' "MVNSNV" "R5,R10,ROR R6"   //Expected to have error, NV is not a valid syntax in visUAL           

            ]           


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                 unit tests on Tokenizer without running visUAL
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
    [<Tests>]
    let tokenizerTest = 
                  let makeTokenizeTest name inp outp =
                      testCase name <| fun () ->
                          let toks = tokenize inp
                          Expect.equal toks outp (sprintf "Tokenize '%s' " inp)
                  Expecto.Tests.testList "Tokenize tests"
                      [
                          makeTokenizeTest "Tokenize1" "R1,R2"  [Reg ("R1",0); Reg ("R2",1); END]
                          makeTokenizeTest "Tokenize2" "R2,R3,R4"  [Reg ("R2",0); Reg ("R3",1);Reg ("R4",2); END]
                          makeTokenizeTest "Tokenize4" "R1,R2,R3,ROR,#0xFF"  [Reg ("R1",0); Reg ("R2",1); Reg ("R3",2); Opr "ROR"; Lit "0xFF"; END]
                          makeTokenizeTest "Tokenize3" "R1,R2,R3,LSL,R5"  [Reg ("R1",0); Reg ("R2",1); Reg ("R3",2); Opr "LSL"; Reg ("R5",3); END]
                      ]




