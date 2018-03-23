module Group


open Expecto
open Expecto.ExpectoFsCheck
open CommonTop
open CommonData

let output = 
    let input = ""
    let parseres = 
        input
        |>inputToLines
        |>genParsedDPSymtab

    let simulateoutput = 
        parseres 
        |>Result.bind (fun (dp,_) -> simulate 0u dp)

    let symtab = 
        parseres
        |>Result.map (fun (_,symt) -> symt)
    simulateoutput,symtab


[<Tests>]
let FILLTest = 
    testProperty "FILL Test" <| fun (B:byte) ->
        let N = int B
        let nStr = sprintf "%A" N
        let inputStr = "here FILL "+nStr
        let parseres = 
            inputStr
            |>inputToLines
            |>genParsedDPSymtab
        let simulateoutput = 
            parseres 
            |>Result.bind (fun (dp,_) -> simulate 0u dp)
        match N with
        |n when n%4=0 -> 
            match simulateoutput with
            |Ok dp -> 
                let mm = 
                    dp.MM
                    |>Map.toList
                    |>List.filter (fun (WA k,_) -> k>=0x100u)
                let expectedmm = 
                    [0..4..(N-4)]
                    |>List.map (fun k -> (WA ((uint32 k)+0x100u), DataLoc 0u))
                Expecto.Expect.equal mm expectedmm<|
                     sprintf "expected mm to be %A but actual mm is %A" expectedmm mm
            |Error k -> failwithf "%A" k
        |n -> 
            Expecto.Expect.equal simulateoutput (Error (ERRIEQUFILL "Number of bytes to fill not a multiple of 4"))<|
                     sprintf "error should be %A" simulateoutput    

[<Tests>]
let DCDTest = 
    testProperty "DCD Test" <| fun (el1:int32) (el2:int32) (el3:int32) ->
        
        let inputStr = sprintf ";This is a comment \r\n here DCD %A,%A,%A " el1 el2 el3
        let parseres = 
            inputStr
            |>inputToLines
            |>genParsedDPSymtab
        let simulateoutput = 
            parseres 
            |>Result.bind (fun (dp,_) -> simulate 0u dp)
        
        match simulateoutput with
        |Ok dp -> 
            let mm = 
                dp.MM
                |>Map.toList
                |>List.filter (fun (WA k,_) -> k>=0x100u)
            let expectedmm = 
                [0;4;8]
                |>List.map2 (fun data num -> (WA ((uint32 num)+0x100u), DataLoc (uint32 data))) [el1;el2;el3]
            Expecto.Expect.equal mm expectedmm<|
                 sprintf "expected mm to be %A but actual mm is %A" expectedmm mm
        |Error k -> failwithf "%A" k



[<EntryPoint>]
let main argv =
    Expecto.Tests.runTestsInAssembly Expecto.Tests.defaultConfig [||] |> ignore
    0 // return an integer exit code


