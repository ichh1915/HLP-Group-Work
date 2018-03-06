//////////////////////////////////////////////////////////////////////////////////////////
//                   Sample (skeleton) instruction implementation modules
//////////////////////////////////////////////////////////////////////////////////////////
namespace GroupProj

module DP = 

    open CommonLex
    open CommonData

    // change these types as required

    /// instruction (dummy: must change)
   
    type Literal = {K: uint32; R: int}

    type Reg = RName

    type SVal = 
        | Nms of Literal
        | Rg of Reg

    type Shift = 
        | LSL 
        | LSR 
        | ASR 
        | ROR 

    type Op2 = 
        | Nm of Literal
        | Ro of Reg
        | Shifted of Reg * Shift * SVal
        | RRX of Reg

    type AriInstr = {Dest: RName option; Op1: RName option; Op2: Op2 option; Suffix: bool; arithType: string}

    //Might want to add different type of Instruction in the future
    type Instr =  AriInstr 

    /// parse error (dummy, but will do)
    type ErrInstr = string

    //  create a shift map
    let shiftOpMap = 
        Map.ofList [
            "LSL", LSL
            "LSR", LSR
            "ASR", ASR
            "ROR", ROR
        ]

    /// sample specification for set of instructions
    let dPSpec = {
        InstrC = DP
        Roots = ["add";"sub";"rsb";"adc";"sbc";"rsc";"cmp";"cmn"]
        Suffixes = [""; "s"]
    }

    /// Do arithmetic operation from instr 
    let arith (instr: Instr) (data: DataPath) : Result<DataPath, string> = 
        let dest = instr.Dest
        let op1 = instr.Op1.Value
        let op2 = instr.Op2
        let suf = instr.Suffix
        let operator = instr.arithType

        let carry = if data.Fl.C then 1u else 0u
        let one = 1u
        let op1Val = data.Regs.[op1]

        // Restriction on the literal
        //let makeLiteral (literalData: uint32) = 
            //let ROR m = (literalData >>> m) ||| (literalData <<<(32 - m))
            //let resList = [0..15]
            //              |> List.map (fun m' -> ROR (2*m')) 
            //              |> List.collect (fun x -> if x >= 0u && x <= 255u then [x] else [])
            //if resList.IsEmpty then None else Some (literalData)

        // return uint32 version of the answer
        let fst = fun (a, b, c) -> a

        //sum is a triplet of sum, cSum, vSum
        let setFlag sum = 
            let checkC (fl: Flags) = 
                match sum with
                | x, cSum, _ when uint64(x) <> cSum -> { N = fl.N; C = true; Z = fl.Z; V = fl.V}
                | _ -> { N = fl.N; C = fl.C; Z = fl.Z; V = fl.V}
            
            let checkZ (fl: Flags) = 
                match sum with
                | x, _, _ when x = 0u -> { N = fl.N; C = fl.C; Z = true; V = fl.V}
                | _ -> { N = fl.N; C = fl.C; Z = fl.Z; V = fl.V}

            let checkN (fl: Flags) = 
                match sum with
                | x, _, _ when int(x) < 0 -> { N = true; C = fl.C; Z = fl.Z; V = fl.V}
                | _ -> { N = fl.N; C = fl.C; Z = fl.Z; V = fl.V}

            let checkV (fl: Flags) = 
                match sum with
                | x, _, vSum when int64(int32(x)) <> vSum -> { N = fl.N; C = fl.C; Z = fl.Z; V = true}
                | _ -> { N = fl.N; C = fl.C; Z = fl.Z; V = fl.V}

            { N = data.Fl.N ; C = data.Fl.C; Z = data.Fl.Z; V = data.Fl.Z}
            |> checkC
            |> checkZ
            |> checkN
            |> checkV
        
        let createSum op1Val op2Val =
            //create a triplet of (sum, cSum, vSum)
            let actualSum (op1Val: uint32) (op2Val: uint32) = uint32(int(op1Val) + int(op2Val))
            let cSum (op1Val:uint64) (op2Val:uint64) = op1Val + op2Val
            let vSum (op1Val:uint32) (op2Val:uint32) = int64(int32(op1Val)) + int64(int32(op2Val))
            let actualNegVal (opVal: uint32) = ~~~opVal + 1u
            let cNegVal (opVal: uint32) = uint64(~~~opVal) + 1UL

            match operator with
            |"add" -> 
                actualSum op1Val op2Val, 
                cSum (uint64(op1Val)) (uint64(op2Val)), 
                vSum op1Val op2Val
            |"sub" -> 
                actualSum op1Val (actualNegVal op2Val), 
                cSum (uint64(op1Val)) (cNegVal (op2Val)), 
                vSum op1Val (actualNegVal op2Val)
            |"rsb" -> 
                actualSum op2Val (actualNegVal op1Val), 
                cSum (uint64(op2Val)) (cNegVal (op1Val)), 
                vSum op2Val (uint32(actualNegVal op1Val))
            |"adc" -> 
                actualSum op1Val (op2Val+carry), 
                cSum (uint64(op1Val)) (uint64(op2Val+carry)), 
                vSum op1Val (op2Val+carry)
            |"sbc" -> 
                actualSum op1Val (actualNegVal (op2Val - carry + one)), 
                cSum (uint64(op1Val)) (cNegVal (op2Val - carry + one)), 
                vSum op1Val (actualNegVal (op2Val-carry + one))
            |"rsc" -> 
                actualSum op2Val (actualNegVal (op1Val - carry + one)), 
                cSum (uint64(op2Val)) (cNegVal (op1Val - carry + one)), 
                vSum op2Val (actualNegVal (op1Val - carry + one))
            |"cmp" -> 
                actualSum op1Val (actualNegVal op2Val), 
                cSum (uint64(op1Val)) (cNegVal (op2Val)), 
                vSum op1Val (actualNegVal op2Val)
            |"cmn" -> 
                actualSum op1Val op2Val, 
                cSum (uint64(op1Val)) (uint64(op2Val)), 
                vSum op1Val op2Val
            |_ -> failwithf "Not supposed to happen!"
        
        let createNewReg dest sum = 
            match dest with 
            | Some y -> 
                let curReg = data.Regs
                [(y, fst sum)]
                |> fun lst -> List.fold (fun prev x -> curReg.Add x) curReg lst //TODO:- increment PC count
            | None -> data.Regs
        
        let updateData dest op1Val op2Val = 
            let sum = createSum op1Val op2Val
            let fl = if (suf || operator = "cmn" || operator = "cmp") then setFlag sum else data.Fl
            let newReg = createNewReg dest sum
            Ok ({Fl = fl; Regs = newReg})
                          
        match op2 with
        | Some (Shifted (reg, shiftOp, sVal)) -> 
            let n = 
                match sVal with
                | Nms y -> y.R
                | Rg r -> int(data.Regs.[r])
            
            let op2Val = 
                match shiftOp with
                | LSL -> 
                    data.Regs.[reg] <<< n 
                | ASR -> 
                    let signBit = data.Regs.[reg] >>> 31
                    let shiftedBit = if signBit = 1u then uint32(0xFFFFFFFF) <<< (32-n) else 0u
                    (data.Regs.[reg] >>> n ||| shiftedBit)
                | LSR ->
                    data.Regs.[reg] >>> n
                | ROR -> 
                    let x = data.Regs.[reg] 
                    (x >>> n) ||| (x <<< (32 - n)) 
            
            updateData dest op1Val op2Val

        | Some (RRX reg) -> 
            let op2Val = (carry <<< 31 ||| data.Regs.[reg] >>> 1)

            updateData dest op1Val op2Val

        | Some (Ro reg) -> //if op2 is a register
            let op2Val = data.Regs.[reg]

            updateData dest op1Val op2Val

        | Some (Nm litNum) ->
            let op2Val = litNum.K
            updateData dest op1Val op2Val

        | None -> Error (sprintf "Not supposed to happen!")


    /// map of all possible opcodes recognised, cmps and cmns are not possible
    let opCodes = opCodeExpand dPSpec |> Map.remove "cmps" |> Map.remove "cmns"

    /// main function to parse a line of assembler
    /// ls contains the line input
    /// and other state needed to generate output
    /// the result is None if the opcode does not match
    /// otherwise it is Ok Parse or Error (parse error string)

    let parse (ls: LineData) : Result<Parse<Instr>,string> option = //Only parse the right instruction
        let parse' (instrC, ((root: string), suffix: string, pCond)) =
            // this does the real work of parsing
            let suf = 
                match suffix with
                |"" -> false  
                |"s" -> true
                |_ -> failwithf "Impossible, it will be filtered by Option.map"

            //split into array of strings
            let splitIntoWords (line: string) =
                line.Split( ([||] : char array), System.StringSplitOptions.RemoveEmptyEntries)
            
            //split into list of strings
            let splitIntoList (x: string) = 
                x.Split(',')
                |> List.ofArray 
                |> List.map splitIntoWords
                |> List.map List.ofArray

            let listOfWords = splitIntoList (ls.Operands.ToUpper())

            //Used to evaluate e.g #5+5*3 case
            let evalLit (lit: string) (rstOfLit: string list)= 
                let stringVal = lit.[1..String.length lit - 1] //Remove the # sign
                let rstOfVal = String.concat "" rstOfLit
                let litOp2 = stringVal + rstOfVal
                
                let opMap = 
                        Map.ofList [ 
                            '+', (+)  
                            '-', (-) 
                            '*', (*)
                            ]

                //create a list of symbols/literals                
                let symSeq = 
                    litOp2.Split([|'+' ; '-' ; '*' |], System.StringSplitOptions.RemoveEmptyEntries)
                    |> List.ofArray
                
                //create a list of operators
                let opSeq = 
                    litOp2 
                    |> fun str -> if (str.[0] = '-' || str.[0] = '+') then str else "+" + str
                    |> Seq.toList
                    |> List.collect (fun x -> if opMap.ContainsKey x then [x] else []) 
                
                //Use List.fold to evalute the expression
                let folder state litOrSym =
                    match state, litOrSym with
                    | (cummSum, hd::rst), y -> 
                        let intVal = 
                            match y, ls.SymTab with //if SymTab has the reference
                            | y, Some symTab when symTab.ContainsKey y -> 
                                int (symTab.[y])
                            | y, _ -> 
                                int(y)
                        let newSum = opMap.[hd] cummSum intVal
                        (newSum, rst)
                    | _ ->
                        (state)    

                List.fold folder (0, opSeq) symSeq
                |> fun (x, y) -> x


            //Case of ArithInstr
            let buildInstr (dest:string) (op1Op2: string list) = 
                let dest = 
                    match dest with
                    | x when regNames.ContainsKey x -> Some regNames.[dest]
                    | _ -> None //Not valid register

                let op1 = 
                    match op1Op2.[0] with
                    | x when regNames.ContainsKey x -> Some regNames.[op1Op2.[0]]
                    | _ -> None //Not valid register

                let op2OrExp = 
                    let op2' = op1Op2.[1..List.length op1Op2 - 1] 
                    match op2' with 
                    | str::rst -> 
                        match str, rst with                     
                        //Few possible cases:
                        //i) R1
                        //ii) R1, LSL #5
                        //iii) R1, LSL R0
                        //iv) #5
                        //v) #5+5*3
                        | x, _ when regNames.ContainsKey x -> 
                            match rst with 
                            | [] -> //Case i)
                                Some (Ro (regNames.[x]))
                            | rst'::rst'' -> //Case ii), iii)
                                match rst', rst'' with 
                                | r, [] when (r.ToLower()) = "rrx" -> //Applicable to RRX only
                                    Some (RRX (regNames.[x]))
                                | r, r'::r'' -> 
                                    match r', r'' with 
                                    |r', [] when regNames.ContainsKey r' -> 
                                        Some (Shifted (regNames.[x], shiftOpMap.[r], Rg regNames.[r']))
                                    |r', r'' when r'.StartsWith "#" -> //might want to include cases where it is valid to do #5+3+3
                                        let intNum = evalLit r' r''
                                        Some (Shifted (regNames.[x], shiftOpMap.[r], Nms ({K = uint32(intNum); R = intNum})))
                                    |_ -> None
                                | _ -> None

                        | x, y when x.StartsWith "#" -> //rst must be empty, op2 is a lit
                            let intNum = evalLit x y 
                            Some (Nm {K = uint32(intNum) ; R = intNum})
                        | _ -> //any other instructions are invalid
                            None
                    | [] -> None //Error case
                
                let instr = {Dest = dest; Op1 = op1; Op2 = op2OrExp; Suffix = suf; arithType = root.ToLower()}       

                match instr with
                | {Dest = None; Op1 = None; Op2 = None} -> Error (sprintf "Syntax error")
                | {Dest = None} -> 
                    match root with
                    | x when (x = "cmp") || (x = "cmn") -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u ; PCond = pCond }
                    | _ -> Error (sprintf "Error Dest")
                | {Op1 = None} -> Error (sprintf "Error Op1")
                | {Op2 = None} -> Error (sprintf "Error Op2")
                | _ -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u; PCond = pCond }

            match root with
            | x when (x = "cmp" || x = "cmn") -> 
                match listOfWords.[listOfWords.Length - 1] with
                | [] -> Error (sprintf "Syntax error")
                | _ -> 
                    let op1Op2 = 
                        listOfWords
                        |> fun lst -> List.fold (fun x lst' -> x @ lst') [] lst
                    buildInstr "" op1Op2 //CMP/CMN has no destination

            | _ -> 
                //Check whether the syntax is correct, i.e extra , at the end
                match listOfWords.[listOfWords.Length - 1] with
                | [] -> Error (sprintf "Syntax error")
                | _ -> 
                    let lst = 
                        listOfWords
                        |> fun lst -> List.fold (fun x lst' -> x @ lst') [] lst
                    let dest = lst.[0]
                    let op1Op2 = 
                        match lst with 
                        | hd::rst -> rst
                        | _ -> lst
                    buildInstr dest op1Op2
            
        Map.tryFind (ls.OpCode.ToLower()) opCodes
        |> Option.map parse'


    /// Parse Active Pattern used by top-level code
    let (|IMatch|_|) = parse