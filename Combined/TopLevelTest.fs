open CommonTop


let input = 
        """ADD R0,R0,#25*6
        abc EQU 0x80000
        ADD R1, R0,#abc
        """

// let output = 
//     input
//     |> parseLine None (WA 0x1000u)

let output = 
    input
    |>inputToLines
    |>genParsedDP initialDataPath
    |>Result.bind (simulate 0u)
        
printf "%A" output