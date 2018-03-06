namespace VisualTest

module VTest =

    open Expecto
    open FsCheck
    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO
    open CommonData
    open CommonLex
    open FlexOp2
    open LS
    open LSM
    open Branch
    open CommonTop

    /// parameters setting up the testing framework
    /// WARNING: PostludeLength must be changed if Postlude is changed
    /// WARNING: global cache (CacheFileName) must be deleted if Postlude is changed
    /// Postlude can contain instructions to move CPU state (flags, memory locations) into rgeisters
    /// standard Postlude moves flags into R1   
    /// Simulation reads VisUAL log and returns registers after test assembly code, and also after postlude
    let initMM = 
        [0x1000u..4u..0x1080u]
        |>List.map (fun k -> (k,k))
    let defaultParas = {
        Parallel = true              // parallel testing is now supported!
        MaxConcurrentVisualDirs = 10 // should only need the same number as of cores
        Cached = true                // true if results are stored in a cache on disk and reused to speed 
                                     // up future repeat simulations
        VisualPath =  
            @"C:\Users\user\Desktop\HLP\VisualTesting\visualapp\visual\"  // the directory in which the downloaded VisUAL.exe can be found
        WorkFileDir = 
            @"C:\Users\user\Desktop\HLP\VisualTesting\visualWork\"        // the directory in which both temporary files and the persistent cache file are put
        CacheFileName = 
            @"C:\Users\user\Desktop\HLP\VisualTesting\visualWork\Cache"   // the file name of the global cache
        CacheLimit = 10               // the number of results before adding to global cache
        InitFlags = {FN=false;FZ=false; FC=false;FV=false}
        InitRegs = [0x101Cu;0x1004u]@[4u..4u..52u]         // initial values of registers R0..R14
        InitMM= initMM
        MemReadBase = 0x1000u          // locations read from memory (currently 13 consecutive words are read)
        Postlude = ""                 // this is overwritten by code
        Prelude = ""                  // this is overwritten by code
    } 
    
  
    let testParas = defaultParas

    type rType = {
        R0:int;R1:int;R2:int;R3:int;R4:int;R5:int;R6:int;R7:int;
        R8:int;R9:int;R10:int;R11:int;R12:int;R13:int;R14:int
    }

    let rType2List (r:rType)=
        [r.R0;r.R1;r.R2;r.R3;r.R4; r.R5;r.R6;r.R7;
         r.R8;r.R9;r.R10;r.R11;r.R12;r.R13;r.R14]
      
    
    let (initialDataPath:DataPath<BInstr>) = 
        let regVals = defaultParas.InitRegs
        let memVals = defaultParas.InitMM
        let regs = 
            regVals@[0u]
            |>List.map2 (fun a b -> (register a, b)) [0..15]
            |>Map.ofList
        let mm = 
            memVals
            |>List.map (fun (a,b) -> (WA a,DataLoc b))
            |>Map.ofList
        let flags = { N=false; C=false; Z=false; V=false}
        {Regs = regs; Fl=flags;MM=mm}

    let convMM2Tuple k = 
        match k with
        |(WA addr,DataLoc data) -> (addr,data)
        |_ -> failwithf "irrelevant"

    let readDataZeroIfNone (mm:MachineMemory<'INS>) wa = 
        match Map.tryFind (WA wa) mm with
        |Some (DataLoc data) -> data
        |None -> 0u
        |_ ->failwithf "wont happen. Read instruction memory"

    let VisualUnitRegMemTest paras name src=
            
        testCase name <| fun () ->
            let mem',outActual = RunVisualWithMemOut paras src
            let mem = List.rev mem'
            // Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            let test = parseLine (Some Map.empty)  (WA 0u) src
            let resultDataPath = 
                match test with
                |Ok parseRes -> 
                    match parseRes.PInstr with
                    |ILS inst -> execLS inst initialDataPath
                    |ILSM inst -> execLSM inst initialDataPath
                    |_ -> failwithf "FAILED123"
                |Error k -> failwithf "%A" k
            let actualRegs = 
                match resultDataPath with
                |Ok datapath -> 
                    datapath.Regs
                    |>Map.toList
                |Error k -> failwithf "akfjh"
            let actualRegsList = 
                actualRegs 
                |> List.map (fun (rname, value) -> (rname.RegNum,int value))
                |>List.filter (fun (a,_) -> a<15)
                |>List.map (fun (a,b) -> (R a,b))
            let actualMem = 
                match resultDataPath with
                |Ok datapath -> 
                    [0u..12u]
                    |>List.map (fun k ->readDataZeroIfNone datapath.MM (k*4u + paras.MemReadBase))
                |Error k -> failwithf "akfjh"
            
                ///filter out memory from membase to membase + (4*12)
                ///compare with mem:uint32 list

            let outRegsNoted = 
                [0..14]
                |>List.map (fun k -> R k)
     
            let outActualNotedVisual = 
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal (actualRegsList |> List.sort) outActualNotedVisual<|
                 sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src
            Expecto.Expect.equal actualMem mem<|
                sprintf "Memory outputs>\n%A\n<don't match expected outputs, src=%s" mem src


    let vTest1 membase = VisualUnitRegMemTest {defaultParas with MemReadBase = membase}            
    [<Tests>]
    let LSAndLSMtests = 
        testList "Load Store Instr Unit Tests"
            [
            vTest1 0x1000u "STR test1" "STR  R3   , [ R1 ]  " 
            vTest1 0x1000u "STR test2" "STR    R5 ,  [ R1 ]" 
            vTest1 0x1000u "STR test3" "STR R2, [R1, #4]" 
            vTest1 0x1000u "STR test4" "STR R2, [R1, #4]!" 
            vTest1 0x1100u "STR test5" "STR R2, [R1, R3,LSL #4+6+4+2]"
            vTest1 0x1100u "STR test6" "STR R2, [R1, R3,LSL #4*4]!"
            vTest1 0x1400u "STR test7" "STR R2, [R1, R3,LSL R0]"
            vTest1 0x1400u "STR test8" "STR R2, [R1, R3,LSL R0]!"
            vTest1 0x1000u "STR test9" "STR R2, [R1, R3]"
            vTest1 0x1000u "STR test10" "stR R2, [R1, R3]!"
            vTest1 0x1000u "STR test11" "STR R2, [R1], R3,LSL R0"
            vTest1 0x1000u "STR test12" "STR R2, [R1], R3"
            vTest1 0x1000u "STR test13" "STR R2, [R1], #4"


            vTest1 0x1000u "LDR test1" "LDR R1,[R1]" 
            vTest1 0x1000u "LDR test2" "LDR R4,[R1, #4]"
            vTest1 0x1000u "LDR test3" "LDR R4,[R1, #4]!"
            vTest1 0x1000u "LDR test4" "LDR R4,[R1, #400]" 
            vTest1 0x1000u "LDR test5" "LDR R4,[R1, #400]     !" 
            vTest1 0x1000u "LDR test6" "LDR R4,[R1, R3]"
            vTest1 0x1000u "LDR test7" "LDR R4,[R1, R3]!"
            vTest1 0x1000u "LDR test8" "LDR R4,[R1,R10,LSL R3]!" 
            vTest1 0x1000u "LDR test9" "LDR R4,[R1,R10,LSL #6]!"
            vTest1 0x1000u "LDR test10" "LDR R4,[R1,R10,LSL R3]"
            vTest1 0x1000u "LDR test11" "LDR R4,[R1,R10,LSL #6]"
            vTest1 0x1000u "LDR test12" "LDR R4,[R1],R10,LSL R3"
            vTest1 0x1000u "LDR test13" "LDR R4,[R1],R10,LSL #6"
            vTest1 0x1000u "LDR test14" "LDR R4,[R1],R10"
            vTest1 0x1000u "LDR test15" "LDR R4,[R1],#80"


            vTest1 0x1000u "STRB test1" "STRB R4, [R1]" 
            vTest1 0x1000u "STRB test2" "STRB R5, [R1]" 
            vTest1 0x1000u "STRB test3" "STRB R2, [R1, #7]" 
            vTest1 0x1000u "STRB test4" "STRB R2, [R1, #13]" 
            vTest1 0x1100u "STRB test5" "STRB R2, [R1, R3,LSL #4]"
            vTest1 0x1100u "STRB test6" "STRB R2, [R1, R3,LSL #4]!"
            vTest1 0x1400u "STRB test7" "STRB R2, [R1, R3,LSL R0]"
            vTest1 0x1400u "STRB test8" "STRB R2, [R1, R3,LSL R0]!"
            vTest1 0x1000u "STRB test9" "STRB R2, [R1, R3]"
            vTest1 0x1000u "STRB test10" "STRB R2, [R1, R3]!"
            vTest1 0x1000u "STRB test11" "STRB R2, [R1], R3,LSL R0"
            vTest1 0x1000u "STRB test12" "STRB R2, [R1], R3"
            vTest1 0x1000u "STRB test13" "STRB R2, [R1], #4"

            vTest1 0x1000u "LDRB test1" "LDRB R4,[R1]"
            vTest1 0x1000u "LDRB test2" "LDRB R4,[R1, #7]"
            vTest1 0x1000u "LDRB test3" "LDRB R4,[R1, #5]!"
            vTest1 0x1000u "LDRB test4" "LDRB R4,[R1, #400+99]" 
            vTest1 0x1000u "LDRB test5" "LDRB R4,[R1, #400-254]     !" 
            vTest1 0x1000u "LDRB test6" "LDRB R4,[R1, R3]"
            vTest1 0x1000u "LDRB test7" "LDRB R4,[R1, R3]!"
            vTest1 0x1000u "LDRB test8" "LDRB R4,[R1,R10,LSL R3]!"
            vTest1 0x1000u "LDRB test9" "LDRB R4,[R1,R10,LSL #31*1-9]!"
            vTest1 0x1000u "LDRB test10" "LDRB R4,[R1,R10,LSL R3]"
            vTest1 0x1000u "LDRB test11" "LDRB R4,[R1,R10,LSL #6]"
            vTest1 0x1000u "LDRB test12" "LDRB R4,[R1],R10,LSL R3"
            vTest1 0x1000u "LDRB test13" "LDRB R4,[R1],R10,LSL #5-3"
            vTest1 0x1000u "LDRB test14" "LDRB R4 , [R1 ],  R10 "
            vTest1 0x1000u "LDRB test15" "LDRB R4,[R1],#80"

            vTest1 0x1000u "STMEA1" "STMEA R1,{R2-R5,R8}"
            vTest1 0x1000u "STMEA2" "STMEA R1!,{R2-R5,R8}"
            vTest1 0x1000u "STMFA1" "STMFA R1,{R2-R4,R8,R9}"
            vTest1 0x1000u "STMFA2" "STMFA R1!,{R2-R5,R5,R8}"
            vTest1 0x1000u "STMED1" "STMED R0,{R2-R5,R8}"
            vTest1 0x1000u "STMED2" "STMIA R0!,{R2-R5,R8}"
            vTest1 0x1000u "STMFD1" "STMFD R0,{R2-R4,R8,R9}" 
            vTest1 0x1000u "STMFD2" "STMDA R0!,{R2-R5,R5,R8}" 

            vTest1 0x1000u "LDMED1" "LDMIB R1,{R2-R5,R8}"
            vTest1 0x1000u "LDMED2" "LDMIA R1!,{R2-R5,R8}"
            vTest1 0x1000u "LDMFD1" "LDMFD R1,{R2-R4,R8,R9}"
            vTest1 0x1000u "LDMFD2" "LDMFD R1!,{R2-R5,R5,R8}"
            vTest1 0x1000u "LDMEA1" "LDMEA R0,{R2-R5,R8}"
            vTest1 0x1000u "LDMEA2" "LDMEA R0!,{R2-R5,R8}"
            vTest1 0x1000u "LDMFA1" "LDMFA R0,{R2-R4,R8,R9}"
            vTest1 0x1000u "LDMFA2" "LDMFA R0!,{R2-R5,R5,R8}" ///

            // vTest 0x1000u "LDMED1" "LDMED R1,{R1,R2-R5,R8}" stack ptr cannot be in list of registers

            ]
    


    [<Tests>]
    let BTestParse = 
        testProperty "Branch Parse Test" <| fun symtabPresent ->
            let symTab:SymbolTable = 
                [
                ("BranchHere",24u);
                ("NotHere", 48u)
                ]
                |>Map.ofList

            let lineData = {
                OpCode="B"
                Operands="BranchHere"
                Label=None
                LoadAddr = WA 8u
                SymTab = 
                    match symtabPresent with
                    |true -> Some symTab
                    |false -> None
            }
            
            match Branch.parse lineData with
            |(Some (Ok parse')) when (parse'.PInstr.TargetAddr.IsNone) -> Expecto.Expect.equal symtabPresent false <| sprintf "Error: return None when symtab is present"
            |(Some (Ok parse')) when (parse'.PInstr.TargetAddr.IsSome) -> Expecto.Expect.equal symtabPresent true  <|sprintf "Error: return Some when symtab is absent"
            |_ -> failwithf "will not happen in this test" 
            

    [<Tests>]
    let BExecTest = 
        testProperty "Branch Exec Test" <| fun (r15:uint32) ->
            let symTab:SymbolTable = 
                [
                ("BranchHere",24u);
                ("NotHere", 48u)
                ]
                |>Map.ofList
            

            let lineData = {
                OpCode="B"
                Operands="BranchHere"
                Label=None
                LoadAddr = WA (r15-8u)
                SymTab = Some symTab
            }
            let parseRes = 
                match Branch.parse lineData with
                |Some (Ok res) -> res
                |Some (Error k) -> failwithf "%s" k
                |None -> failwithf "will not happen"
            let resultDataPath = 
                match execB parseRes.PInstr {initialDataPath with Regs = (initialDataPath.Regs.Add (R15,r15))} with
                |Ok res -> res
                |Error k -> failwithf "%s" k
            let actualPC = resultDataPath.Regs.[R15]
            Expecto.Expect.equal actualPC (symTab.["BranchHere"]+8u)<|
                     sprintf "Actual PC don't match expected PC. Expected = %A , Actual = %A" (symTab.["BranchHere"]+8u) actualPC 
            

    [<Tests>]
    let BLExecTest = 
        testProperty "Branch Link Exec Test" <| fun (r14:uint32) (r15:uint32) ->
            let symTab:SymbolTable = 
                [
                ("BranchHere",24u);
                ("NotHere", 48u)
                ]
                |>Map.ofList
            

            let lineData = {
                OpCode="BL"
                Operands="BranchHere"
                Label=None
                LoadAddr = WA (r15-8u)
                SymTab = Some symTab
            }
            let parseRes = 
                match Branch.parse lineData with
                |Some (Ok res) -> res
                |Some (Error k) -> failwithf "%s" k
                |None -> failwithf "will not happen"
            let resultDataPath = 
                match execB parseRes.PInstr {initialDataPath with Regs = (initialDataPath.Regs.Add (R15,r15)).Add(R14,r14)} with
                |Ok res -> res
                |Error k -> failwithf "%s" k
            let actualPC = resultDataPath.Regs.[R15]
            let actualR14 = resultDataPath.Regs.[R14]
            Expecto.Expect.equal actualPC (symTab.["BranchHere"]+8u)<|
                     sprintf "Actual PC don't match expected PC. Expected = %A , Actual = %A" (symTab.["BranchHere"]+8u) actualPC 
            Expecto.Expect.equal actualR14 (r15-4u)<|
                     sprintf "Actual r14 don't match expected r14. Expected = %A , Actual = %A" (r15-4u) actualR14

    type DPGen() =
        static member Datapath() : Arbitrary<DataPath<'INS>> =
            let genAddressWithData = 
                Arb.generate<DoNotSize<int32>>
                |>Gen.map (fun (DoNotSize x) -> uint32 x)
                |>Gen.two 
                |>Gen.map (fun (addr,data) -> (WA addr, DataLoc data))
                |>Gen.listOfLength 1000
                
            let reglist =
                    [0..15]
                    |>List.map (fun k -> inverseRegNums.[k])
            let genFlags = Arb.generate<Flags>
                    
            let genRegData = 
                Arb.generate<DoNotSize<int32>>
                |>Gen.map (fun (DoNotSize x) ->uint32 x)
                |>Gen.listOfLength 16
                
            let createDataPath regdata mm flag=
                {MM = mm|>Map.ofList; Regs = (List.map2 (fun reg value-> (reg,value)) reglist regdata)|>Map.ofList ; Fl=flag}
    
            let genUser =
                createDataPath <!> genRegData <*> genAddressWithData <*> genFlags
            genUser |> Arb.fromGen

    type RListGen() =
        static member Reglist() : Arbitrary<RName list> =
            ///generate list of 5 registers
            let genRList = 
                Gen.choose (0,15)
                |>Gen.map (register)
                |>Gen.listOfLength 5
            genRList |> Arb.fromGen

    let configLS = { FsCheckConfig.defaultConfig with arbitrary = [typeof<DPGen>] }
 

    let configLSM = {FsCheckConfig.defaultConfig with arbitrary = [typeof<RListGen>;typeof<DPGen>]}



       

    [<Tests>]
    let StoreByteTest = 
        testPropertyWithConfig configLS "store byte property test" <| fun dp offset ptrupdate->
            ///r1 with data, r2 with addr. STR R1,R2
            let resultDataPath = LSMain R1 R2 (Some offset) true S ptrupdate dp
  
            let data = dp.Regs.[R1]
            let addr = dp.Regs.[R2]
            
            let initialAddr = 
                match ptrupdate with
                    |None|Some PRE -> addr+offset
                    |Some POST -> addr
            let byteData = word2Byte data
            let addrOffset = initialAddr%4u
            let baseAddr = initialAddr-addrOffset
            let shiftedByte = byteData<<<int (8u*addrOffset)                
            let updatedMM = 
                
                match dp.MM.TryFind (WA baseAddr) with
                |Some (DataLoc data) -> 
                    let newWData = ((~~~(0xFFu<<<int addrOffset*8)) &&& data)+shiftedByte
                    dp.MM.Add (WA baseAddr, DataLoc newWData)
                |None -> dp.MM.Add (WA baseAddr, DataLoc shiftedByte)///return 0 for uninitialised memory
                |_ -> failwithf "Will not happen, as memory is initialised without instruction content"  
            let expectedDataPath = 
                match ptrupdate with
                |None -> PCPlus4{dp with MM = updatedMM} 
                |Some PRE -> PCPlus4{dp with MM = updatedMM; Regs = dp.Regs.Add (R2,addr+offset)}
                |Some POST -> PCPlus4{dp with MM =updatedMM; Regs = dp.Regs.Add (R2,addr+offset)}
                        
            match initialAddr with
            |addr' when (addr' < 0x1000u) -> 
                Expecto.Expect.equal resultDataPath (Error "Address under 0x1000 not allowed.")<|
                     sprintf "Error detection of prohibited memory address failed"
            
            |_ -> Expecto.Expect.equal resultDataPath (Ok expectedDataPath)<|
                     sprintf "Data output not equal to Expected output, Expected
                        map updated value:%A, ActualValue: %A" expectedDataPath.MM.[WA baseAddr] 
                        (Result.map (fun k->k.MM.[WA baseAddr]) resultDataPath) 

    [<Tests>]
    let StoreWordTest = 
        testPropertyWithConfig configLS "store word property test" <| fun dp offset ptrupdate-> 
            let resultDataPath = LSMain R1 R2 (Some offset) false S ptrupdate dp
            
            let data = dp.Regs.[R1]
            let addr = dp.Regs.[R2]
            
            let expectedDataPath = 
                match ptrupdate with
                |None -> PCPlus4{dp with MM = dp.MM.Add (WA (addr+offset),DataLoc data)} 
                |Some PRE -> PCPlus4{dp with MM = dp.MM.Add (WA (addr+offset),DataLoc data); Regs = dp.Regs.Add (R2,addr+offset)}
                |Some POST -> PCPlus4{dp with MM = dp.MM.Add (WA (addr),DataLoc data); Regs =dp.Regs.Add (R2,addr+offset)}
            let readDataAddr = 
                match ptrupdate with
                |None|Some PRE -> addr+offset
                |Some POST -> addr
            match readDataAddr with
            |addr' when (addr' < 0x1000u) -> 
                Expecto.Expect.equal resultDataPath (Error "Address under 0x1000 not allowed.")<|
                     sprintf "Error detection of prohibited memory address failed"
            |addr' when ((addr'%4u) <> 0u) ->
                Expecto.Expect.equal resultDataPath (Error "unaligned memory access")<|
                     sprintf "Error detection of unaligned word address failed"
            |_ -> Expecto.Expect.equal resultDataPath (Ok expectedDataPath)<|
                     sprintf "Data output not equal to Expected output, Expected
                        map updated value:%A, ActualValue: %A" expectedDataPath.MM.[WA readDataAddr] 
                        (Result.map (fun k->k.MM.[WA readDataAddr]) resultDataPath) 

    [<Tests>]
    let LoadByteTest = 
        testPropertyWithConfig configLS "Load Byte Property Test" <| fun dp offset ptrupdate->
            ///r1 is destination, r2 is address. LDR R1,R2
            let resultDataPath = LSMain R1 R2 (Some offset) true L ptrupdate dp
            let addr = dp.Regs.[R2]
            let initAddr = 
                match ptrupdate with
                    |None|Some PRE -> addr+offset
                    |Some POST -> addr
            let addrOffset = initAddr%4u
            let baseAddr = initAddr-addrOffset
            let data = 
                match Map.tryFind (WA baseAddr) dp.MM with
                |None -> 0u
                |Some (DataLoc k) -> 
                    let byteList = breakWordIntoBytes k
                    byteList.[int addrOffset]
                |_ -> failwithf "Will not happen, as memory is initialised without instruction content"  
                       
            let updatedRegs = dp.Regs.Add (R1,data)
            let expectedDataPath = 
                match ptrupdate with
                |None -> PCPlus4{dp with Regs = updatedRegs} 
                |Some PRE -> PCPlus4{dp with Regs = updatedRegs.Add (R2,addr+offset)}
                |Some POST -> PCPlus4{dp with Regs = updatedRegs.Add (R2,addr+offset)}
                        
            match initAddr with
            |addr' when (addr' < 0x1000u) -> 
                Expecto.Expect.equal resultDataPath (Error "Address under 0x1000 not allowed.")<|
                     sprintf "Error detection of prohibited memory address failed"
            
            |_ -> Expecto.Expect.equal resultDataPath (Ok expectedDataPath)<|
                     sprintf "Data output not equal to Expected output, Expected
                        map updated value:%A, ActualValue: %A" expectedDataPath.Regs.[R1] 
                        (Result.map (fun k->k.Regs.[R1]) resultDataPath) 

    [<Tests>]
    let LoadWordTest = 
        testPropertyWithConfig configLS "Load Word Property Test" <| fun dp offset ptrupdate->
            ///r1 is destination, r2 is address. LDR R1,R2
            let resultDataPath = LSMain R1 R2 (Some offset) false L ptrupdate dp
            let addr = dp.Regs.[R2]
            let initAddr = 
                match ptrupdate with
                    |None|Some PRE -> addr+offset
                    |Some POST -> addr
            let data = 
                match Map.tryFind (WA initAddr) dp.MM with
                |None -> 0u
                |Some (DataLoc k) -> k
                |_ -> failwithf "Will not happen, as memory is initialised without instruction content"  
                       
            let updatedRegs = dp.Regs.Add (R1,data)
            let expectedDataPath = 
                match ptrupdate with
                |None -> PCPlus4{dp with Regs = updatedRegs} 
                |Some PRE -> PCPlus4{dp with Regs = updatedRegs.Add (R2,addr+offset)}
                |Some POST -> PCPlus4{dp with Regs = updatedRegs.Add (R2,addr+offset)}
                        
            match initAddr with
            |addr' when (addr' < 0x1000u) -> 
                Expecto.Expect.equal resultDataPath (Error "Address under 0x1000 not allowed.")<|
                     sprintf "Error detection of prohibited memory address failed"
            |addr' when ((addr'%4u) <> 0u) ->
                Expecto.Expect.equal resultDataPath (Error "unaligned memory access")<|
                     sprintf "Error detection of unaligned word address failed"
            |_ -> Expecto.Expect.equal resultDataPath (Ok expectedDataPath)<|
                     sprintf "Data output not equal to Expected output, Expected
                        map updated value:%A, ActualValue: %A" expectedDataPath.Regs.[R1] 
                        (Result.map (fun k->k.Regs.[R1]) resultDataPath)     



    [<Tests>]
    let StoreLoadCheckRegsTest = 
        testPropertyWithConfig configLSM "Load Store Multiple Word Property Test" <| fun dp dir reglist->
            ///r1 address, LDM{dir} R1 {list}
            let storeRes = LSMMain dir S true R0 reglist dp
            let zeroRegsTuple = 
                reglist
                |>List.map (fun k -> (k,0u))
            let DPWithChangedRegs = Result.map (fun k -> {k with Regs = updateRec k.Regs zeroRegsTuple}) storeRes
            let loadRes = Result.bind (LSMMain dir L true R0 reglist) DPWithChangedRegs
            let expectedRegs = dp.Regs.Add (R15,dp.Regs.[R15]+8u)
            match loadRes with 
            |Ok DP -> 
                Expecto.Expect.equal DP.Regs expectedRegs<|
                     sprintf "Data path registers changed after store and load operation."
            |Error k -> 
                Expecto.Expect.equal 1 1<| sprintf "Ignore Error For Now"   


              
            
