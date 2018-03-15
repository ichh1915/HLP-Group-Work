open CommonTop


let input = 
        "ADD R0,R0,#25*6"

// let output = 
//     input
//     |> parseLine None (WA 0x1000u)

let output = 
    input
    |>inputToLines
    |>genParsedDP initialDataPath
    |>Result.bind (simulate 0u)
        
printf "%A" output