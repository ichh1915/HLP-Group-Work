namespace Emulator

[<AutoOpen>]
module TopLevel =
    open CommonTop
    open CommonData

    let output input = 
        input
        |>inputToLines
        |>genParsedDP initialDataPath
        |>Result.bind (simulate 0u)

    let regs = 
        match output "add R0, R1, #0x55\n" with
        | Ok x -> x.Regs
        | Error y -> failwithf "Error!"
    
    let regList = 
        regs 
        |> Map.toList
        |> List.map (fun (x, y) -> x.RegNum, y)


    printfn "%A" (regList)
