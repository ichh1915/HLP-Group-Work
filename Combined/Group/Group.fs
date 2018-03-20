module Group

open CommonData
open CommonTop

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            
            mov r0,#here
            mov r1,#this
            that equ this
            this equ here
            here FILL 12
            mov r4,#that
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


