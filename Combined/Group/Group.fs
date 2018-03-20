module Group

open CommonData
open CommonTop

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            here FILL 12
            this equ 1
            mov r0,#here
            mov r1,#this
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


