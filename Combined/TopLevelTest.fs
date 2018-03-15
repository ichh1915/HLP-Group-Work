open CommonData
open CommonTop
open System


let input = 
        """
        Moveq R0,#5
        ADD R0,R0,#0X25
        pokemon EQU 3456
        ADD R3,R4, #pokemon       
        END
        """

let output = 
    input
    |>inputToLines
    |>genParsedDP initialDataPath
    |>Result.bind (simulate 0u)
        
printf "%A" output

// uint32 "0x123"