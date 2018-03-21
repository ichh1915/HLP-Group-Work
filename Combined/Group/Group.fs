module Group

open CommonData
open CommonTop

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            
            mov		r0,#0x100
            ldr r5,=98427
		    str		r1,[r0,#4092]

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


