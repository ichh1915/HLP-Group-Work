module Group

open CommonData
open CommonTop

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            
            mov r0,#0xFFFFFFF
            bl jumphere
            add r5,r5,#5
            end
            mov r1,#3
            jumphere mov r4,#22
            mov pc,lr
            END

            """

    let output = 
        input
        |>inputToLines
        |>genParsedDP 
        |>Result.bind (simulate 0u)

    let regVals out = 
        out.Regs
        |>Map.toList
        |>List.map (fun (_,b) -> b)

    let valList = Result.map regVals output

   
    printf "%A" output
    printf "%A" valList
    0 // return an integer exit code


