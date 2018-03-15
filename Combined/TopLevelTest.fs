open CommonData
open CommonTop
open System


let input = 
        """
        cmp R0, #1
        
        """

let output = 
    input
    |>inputToLines
    |>genParsedDP initialDataPath
    |>Result.bind (simulate 0u)
        
printf "%A" output

// uint32 "0x123"