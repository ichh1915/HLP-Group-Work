module Group

open CommonData
open CommonTop
open System

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            mov r0,#5
            mov r1,#0x30
            mov r2,#0b111
            
            
            
            
            END
            
            """

    let output = 
        input
        |>inputToLines
        |>genParsedDP initialDataPath
        |>Result.bind (simulate 0u)

    let regVals out = 
        out.Regs
        |>Map.toList
        |>List.map (fun (_,b) -> b)

    let valList = Result.map regVals output

    printf "%A" output
    printf "%A" valList
    
    printfn "%A" argv
    0 // return an integer exit code
