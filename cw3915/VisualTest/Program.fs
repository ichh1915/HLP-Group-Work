// Learn more about F# at http://fsharp.org
namespace GroupProj

module Main = 

    open CommonTop
    open CommonData
    open CommonLex
    open DP
    open System

    /// test the initProjectLexer code
    let test = parseLine None (WA 0u)

    let initFlags = { N = false; C = false; Z = false; V = false}

    let initRegMap = 
        Map.ofList [ 
            R0, uint32(1) ; R1, uint32(1) ; R2, uint32(2) ; R3, uint32(3) ; R4, uint32(0) ; R5, uint32(0) ;
            R6, uint32(0) ; R7, uint32(0) ; R8, uint32(0) ; R9, uint32(0) ; R10, uint32(0) ; R11, uint32(0) ; 
            R12, uint32(0) ; R13, uint32(0) ; R14, uint32(0) ; R15, uint32(0) ; 
            R15, uint32(0) ; R14, uint32(0) ; R13, uint32(0) 
        ] 


    let lineData = {
            LoadAddr = WA (uint32(100))
            Label = None
            SymTab = None
            OpCode = "SUBS"
            Operands = "R2 , R0 , R1"
        }

    let lineData2 = {
            LoadAddr = WA (uint32(100))
            Label = None
            SymTab = None
            OpCode = "CMP"
            Operands = "R0 , R1"
        }

    let initData = {Fl = initFlags; Regs = initRegMap}

    let arithInstr = 
        let instr = parse lineData
        match instr with
        | Some (Ok y) -> Some (arith y initData y.PInstr.arithType)
        | Some (Error _) -> None
        | _ -> None

    let res = 
        match arithInstr with
        | Some (Ok y) -> y.Regs.TryFind R1
        | _ -> None

    [<EntryPoint>]
    let main argv =
        printfn "Hello World from F#!"
        //add parseTest 
        arithInstr |> printfn "%A"
        0 // return an integer exit code