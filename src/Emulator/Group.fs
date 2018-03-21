namespace Emulator

module Group = 

    open CommonData
    open CommonTop
    open Fable.Core
    open Fable.Core.JsInterop
    open Fable.Import.Browser

    let output input = 
        let res = 
            input
            |>inputToLines
            |>genParsedDP 
            |>Result.bind (simulate 0u)
          
        match res with
        | Ok x -> 
            let regs = 
                x.Regs
                |> Map.toList
                |> List.map (fun (x,b) -> x.RegNum, b)
            
            let fls = 
                x.Fl
                |> fun x -> [ ("N", x.N); ("Z", x.Z) ; ("C", x.C) ; ("V", x.V) ]
            
            let mm = 
                let mmList = 
                    x.MM
                    |> Map.toList
                        
                let getDataLoc mm =
                    match mm with
                    | WA x, DataLoc y -> Some (x, y)
                    | _ -> None
                                
                mmList
                |> List.map (fun m -> getDataLoc m)
                |> List.filter (fun x -> x <> None)
                |> List.map (fun x -> x.Value)  
                
            regs, fls, mm
            
        | Error y -> 
            window.alert (sprintf "%A" y) 
            failwithf "Error regs"