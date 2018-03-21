module Group

open CommonData
open CommonTop

[<EntryPoint>]
let main argv =
    


    let input = 
            """
            
            
    	here equ 1
        mov r0,#here

            """

    let output = 
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


    let regVals out = 
        out.Regs
        |>Map.toList
        |>List.map (fun (_,b) -> b)

    let valList = Result.map regVals ((fun (a,b) -> a) output)
    let symtab = (fun (a,b) -> b) output 

   
    printf "%A" output
    printf "%A" symtab
    printf "%A" valList
    0 // return an integer exit code


