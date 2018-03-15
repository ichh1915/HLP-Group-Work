open CommonTop
let input = 
        """STR R0,[R1]
        POKEMON END"""

let output = 
    input
    |>inputToLines
    |>genParsedDP initialDataPath
    |>Result.bind (simulate 0u)
        
printf "%A" output