module Group

open CommonData
open CommonTop
open System

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            mov r1,#5
            BACKHERE SUBS R1,R1,#1
            BNE BACKHERE
            MOV R0,#55
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
