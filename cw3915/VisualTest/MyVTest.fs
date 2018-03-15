namespace VisualTest

/// top-level code demonstrating how to run tests

module MyVTest =


    open GroupProj
    open CommonData
    open CommonLex
    open DP
    open CommonTop
    open Expecto
    open VCommon
    open VLog
    open Visual
    open System.Threading
    open System.IO

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


    let testParas = defaultParas

    //Unit test
    let myVisualUnitTest paras name src =
        testCase name <| fun () -> 
            let initDP = 
                let flag: CommonData.Flags = 
                    match paras.InitFlags with
                    |{FN = n ; FZ = z ; FC = c ; FV = v} -> {N = n ; Z = z ; C = c ; V = v}
                let data = 
                    paras.InitRegs
                    |> List.indexed
                    |> List.map (fun (n,v) -> inverseRegNums.[n], v)
                    |> Map.ofList
                {Fl = flag ; Regs = data}

            let instr = parseLine None (WA(100u)) src 

            let arithInstr = 
                match instr with
                | (Ok y) -> 
                    match y.PInstr with
                    |IDP instr' -> 
                        Some (arith (instr') initDP)
                    | _ -> failwithf "Invalid instruction" //Yet to incorporate with group
                | (Error _) -> None
            

            let outExpected = 
                match arithInstr with
                | Some (Ok y) -> 
                    [0..14]
                    |> List.map (fun n -> (R n , int(y.Regs.[inverseRegNums.[n]])))
                | _ -> []

            let flagsExpected = 
                let getNZ (fl: CommonData.Flags) = 
                    match fl.N, fl.Z with
                    | false, false -> "00"
                    | false, true -> "01"
                    | true, false -> "10"
                    | true, true -> failwithf "Not possible flag combination: N = 1, Z = 1"
                let getCV (fl: CommonData.Flags) = 
                    match fl.C, fl.V with
                    | true, true -> "11"
                    | true, false -> "10"
                    | false, true -> "01"
                    | false, false -> "00"

                match arithInstr with
                | Some (Ok y) -> getNZ (y.Fl) + getCV (y.Fl)
                | _ -> "0000"

            let flagsActual, outActual = RunVisualWithFlagsOut paras src
            Expecto.Expect.equal flagsActual (flagsExpected |> strToFlags) "Status flags don't match"
            let outRegsNoted = 
                outExpected 
                |> List.map fst
            let outActualNoted = 
                outActual.Regs 
                |> List.filter (fun (r,_) -> List.contains r outRegsNoted)
                |> List.sort
            Expecto.Expect.equal outActualNoted (outExpected |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outActual.Regs src
    
    //Property test:- test whether ADDS R2, #1, #2 gives the same result as SUBS R2, #1, #-2
    //Only test for adds, subs, rsbs
    let myVisualPropertyTest paras name (src: string) = 
        testCase name <| fun () -> 
            let initDP = 
                let flag: CommonData.Flags = 
                    match paras.InitFlags with
                    |{FN = n ; FZ = z ; FC = c ; FV = v} -> {N = n ; Z = z ; C = c ; V = v}
                let data = 
                    paras.InitRegs
                    |> List.indexed
                    |> List.map (fun (n,v) -> inverseRegNums.[n], v)
                    |> Map.ofList
                {Fl = flag ; Regs = data}

            let instr = 
                let splitSrc = 
                    let splitIntoWords (line: string) =
                        line.Split( ([||] : char array), System.StringSplitOptions.RemoveEmptyEntries)
                    
                    src.Split(',')
                    |> List.ofArray 
                    |> List.map splitIntoWords
                    |> List.map List.ofArray
                    |> fun lst -> List.fold (fun x lst' -> x @ lst') [] lst
                
                let buildComplimentSrc op dest op1 op2 =
                    op + " " + dest + ", " + op1 + ", " + op2

                match splitSrc with
                | x when x.[0] = "ADDS" -> 
                    let complimentSrc = buildComplimentSrc "SUBS" x.[1] x.[2] ("#-" + x.[3].[1..String.length x.[3] - 1])
                    (parseLine None (WA(100u)) src , parseLine None (WA(100u)) complimentSrc)
                | x when x.[0] = "SUBS" -> 
                    let complimentSrc = buildComplimentSrc "ADDS " x.[1] x.[2] ("#-" + x.[3].[1..String.length x.[3] - 1])
                    (parseLine None (WA(100u)) src , parseLine None (WA(100u)) complimentSrc)
                | x when x.[0] = "RSBS" -> 
                    let complimentSrc = buildComplimentSrc "SUBS" x.[1] x.[3] x.[2]
                    (parseLine None (WA(100u)) src , parseLine None (WA(100u)) complimentSrc)
                | _ -> (parseLine None (WA(100u)) src, parseLine None (WA(100u)) src) //Not yet implemented

            let arithInstr = 
                match instr with
                | (Ok y1, Ok y2) -> 
                    match y1.PInstr, y2.PInstr with
                    |IDP instr', IDP instr'' -> Some ((arith (instr') initDP), arith instr'' initDP)
                    | _ -> failwithf "Invalid instruction" //Yet to incorporate with group
                | _ -> None
            
            let out (instr: DataPath) = 
                [0..14]
                |> List.map (fun n -> (R n , int(instr.Regs.[inverseRegNums.[n]])))


            let flags (instr: DataPath) = 
                let getNZ (fl: CommonData.Flags) = 
                    match fl.N, fl.Z with
                    | false, false -> "00"
                    | false, true -> "01"
                    | true, false -> "10"
                    | true, true -> failwithf "Not possible flag combination: N = 1, Z = 1"
                let getCV (fl: CommonData.Flags)= 
                    match fl.C, fl.V with
                    | true, true -> "11"
                    | true, false -> "10"
                    | false, true -> "01"
                    | false, false -> "00"

                getNZ (instr.Fl) + getCV (instr.Fl)

            let outExpected, outCompliment = 
                match arithInstr with
                | Some (Ok x, Ok y) -> out x, out y
                | _ -> failtestf "Error Instructions"

            let flagExpected, flagCompliment = 
                match arithInstr with
                | Some (Ok x, Ok y) -> flags x, flags y
                | _ -> failtestf "Error Instructions"

            Expecto.Expect.equal flagExpected (flagCompliment) "Status flags don't match"

            Expecto.Expect.equal (outExpected |> List.sort) (outCompliment |> List.sort) <|
                sprintf "Register outputs>\n%A\n<don't match expected outputs, src=%s" outExpected src

    let myVTest = myVisualUnitTest testParas

    let myVProTest = myVisualPropertyTest testParas

    let typeOfTestLst = [
        "SBCS"
        "SUBS"
        "ADDS"
        "ADCS"
        "RSBS"
        "RSCS"
        "CMP"
        "CMN"
        ]
    
    //Generate random tests for op2 = literal
    let rndLitTstLst = 

        let genRandomNumbers count =
            let rnd = System.Random()
            let init = Seq.initInfinite (fun _ -> rnd.Next(0, 255))

            init 
            |> Seq.distinct //remove repeated values
            |> Seq.take(count)
            |> Seq.toList

        //number of random numbers generated
        let rndNum = genRandomNumbers 10
            
        //act as a filter, so that only valid number is passed to ViSUAL
        let makeLiteral (literalData: uint32) = 
            let ROR m = (literalData >>> m) ||| (literalData <<<(32 - m))
            let resList = [0..15]
                          |> List.map (fun m' -> ROR (2*m')) 
                          |> List.collect (fun x -> if x >= 0u && x <= 255u then [x] else [])
            if resList.IsEmpty then None else Some (literalData)
        
        //create instruction to feed into myVTest
        let createTest lit (typeOfTest:string) =
            match lit with 
            | Some y -> 
                match typeOfTest with
                | x when (x = "CMP") || (x = "CMN") -> 
                    Some (myVTest ( x + " Test" + string(lit.Value)) (x + " R2" + ", #" + string(lit.Value)))
                | _ -> 
                    Some (myVTest ( typeOfTest + " Test" + string(lit.Value)) (typeOfTest + " R2, R0" + ", #" + string(lit.Value)))
            | None -> None

        rndNum
        |> List.map (fun x -> makeLiteral (uint32(x)))
        |> List.collect (fun x -> List.map (fun str -> createTest x str) typeOfTestLst)
        |> List.collect (fun x -> if x.IsSome then [x.Value] else [])
    
    //Generate random tests for op2 = register
    let rndRegTstLst = 
        let rnd = System.Random()

        let regLst = 
            //remove registers that are not supposed to appear in arith instructions
            let keys = ["R13";"R15";"PC";"LR";"SP"] //exclude r13, as it imposed certain constraints
            let filteredLst = List.fold (fun mapPrev key -> Map.remove key mapPrev) regNames keys
            filteredLst
            |> Map.toList
            |> List.map (fun (str, _) -> str)

        let rndDest = List.item (rnd.Next(regLst.Length)) regLst
        let rndOp1 = List.item (rnd.Next(regLst.Length)) regLst
        let rndOp2 = List.item (rnd.Next(regLst.Length)) regLst
        let rndReg = (rndDest, rndOp1, rndOp2)

        let createTest dest op1 op2 (typeOfTest:string) =
            match typeOfTest with
            | x when (x = "CMP") || (x = "CMN") -> 
                myVTest (x + " Test") (x + " " + op1 + ", " + op2)
            | _ -> 
                myVTest (typeOfTest + " Test") (typeOfTest + " " + dest + ", " + op1 + ", " + op2)

        rndReg 
        |> fun (dest, op1, op2) -> List.map (fun typeOfTest -> createTest dest op1 op2 typeOfTest) typeOfTestLst
    
    //Test on specific case
    let definedTstLst = 
        [
            myVTest "LSL Op2 Test" "ADDS R0, R1, R2, LSL #5"
            myVTest "LSR Op2 Test" "ADDS R0, R1, R2, LSR #5"
            myVTest "ASR Op2 Test" "ADDS R0, R1, R2, ASR #5"
            myVTest "ROR Op2 Test" "ADDS R0, R1, R2, ROR #5"
            myVTest "RRX Op2 Test" "ADDS R0, R1, R2, RRX"
            myVTest "LSL Op2 Reg Test" "ADDS R0, R1, R2, LSL R1"
            myVTest "LSR Op2 Reg Test" "ADDS R0, R1, R2, LSR R1"
            myVTest "ASR Op2 Reg Test" "ADDS R0, R1, R2, ASR R1"
            myVTest "ROR Op2 Reg Test" "ADDS R0, R1, R2, ROR R1"
            myVTest "ADDS with Lit Test" "ADDS R0, R1, #5 + 5 + 3"
            myVTest "SUBS with Lit Test" "SUBS R0, R1, #5 + 5 * 3"
        ]

    [<Tests>]
    let myUnitTest = 
        testList "Arith ViSUAL Unit Tests"
            (rndLitTstLst @ rndRegTstLst @ definedTstLst)
     
    [<Tests>]
    let myPropTest = 
        testList "Arith Property Tests"
            [
            myVProTest "ADDS Test" "ADDS R0, R1, #2"
            myVProTest "SUBS Test" "SUBS R0, R1, #1"
            myVProTest "RSBS Test" "RSBS R0, R1, R2"
            ]