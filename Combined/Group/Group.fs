module Group

open CommonData
open CommonTop
open System
open FlexOp2

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            
            mov r0,#0xFFFFFFF
            

            
            
            
            
            
            
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
    printfn "%A" (checkLitValidity 100u)
    0 // return an integer exit code


