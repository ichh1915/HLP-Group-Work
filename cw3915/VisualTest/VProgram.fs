namespace VisualTest
module VProgram =

    open Expecto
    open VCommon
    open VLog
    open Visual
    open VTest
    open MyVTest
    open System.Threading
    open System.IO

    open GroupProj
    open CommonTop
    open CommonData
    open CommonLex
    open DP
    open System

    /// configuration for this testing framework      
    /// configuration for expecto. Note that by default tests will be run in parallel
    /// this is set by the fields oif testParas above
    let expectoConfig = { Expecto.Tests.defaultConfig with 
                            parallel = testParas.Parallel
                            parallelWorkers = 6 // try increasing this if CPU use is less than 100%
                    }
    
    ///  Sample code to use my code
    //let paras = VisualTest.MyVTest.defaultParas

    //let initDP = 
    //    let flag: CommonData.Flags = 
    //        match paras.InitFlags with
    //        |{FN = n ; FZ = z ; FC = c ; FV = v} -> {N = n ; Z = z ; C = c ; V = v}
    //    let data = 
    //        paras.InitRegs
    //        |> List.indexed
    //        |> List.map (fun (n,v) -> inverseRegNums.[n], v)
    //        |> Map.ofList
    //    {Fl = flag ; Regs = data}

    //let test = parseLine None (WA(100u))     
    //let parseRes = test "ADDS R0, R1, #0x5 + 0x5 * 0x3"

    //let arithInstr = 
        //match parseRes with
        //| (Ok y) -> 
        //    match y.PInstr with
        //    |IDP instr' -> 
        //        Some (arith (instr') initDP)
        //    | _ -> failwithf "Invalid instruction" //Yet to incorporate with group
        //| (Error _) -> None

    [<EntryPoint>]
    let main _ = 

        //arithInstr |> printfn "%A"
        //0
        initCaches testParas
        let rc = runTestsInAssembly expectoConfig [||]
        finaliseCaches testParas
        System.Console.ReadKey() |> ignore                
        rc // return an integer exit code - 0 if all tests pass