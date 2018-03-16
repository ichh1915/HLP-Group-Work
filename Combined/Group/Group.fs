module Group

open CommonData
open CommonTop
open System

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            mov r1,#0x8000
            tst r1,r2
            adds R0,R0, #1
            
            """

    let output = 
        input
        |>inputToLines
        |>genParsedDP initialDataPath
        |>Result.bind (simulate 0u)
            
    printf "%A" output
    printfn "%A" argv
    0 // return an integer exit code
