//////////////////////////////////////////////////////////////////////////////////////////
//                   Sample (skeleton) instruction implementation modules
//////////////////////////////////////////////////////////////////////////////////////////


module DP 

open EEExtensions
open CommonLex
open CommonData
open LS

// ----------------------------- Types ------------------------------------------------ //

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

type ArithType = 
    | ADD
    | SUB
    | RSB
    | RSC
    | ADC
    | SBC
    | CMP
    | CMN

type Suf = 
    | FSet
    | NoFSet

//type AriInstr = {Dest: RName option; Op1: RName option; Op2: Op2 option; Suffix: bool; arithType: ArithType}
type AriInstr = {Dest: RName option; Op1: RName option; Op2: Op2 option; Suffix: Suf; arithType: ArithType option;Cond:Condition}


//Might want to add different type of Instruction in the future
type Instr =  AriInstr 

/// parse error (dummy, but will do)
type ErrInstr = string

type Token = 
    | R of string*int // R0-R15, straight away use RName * int
    | Opr of string     // e.g LSL, straight away use Shift
    | Exp of string     // e.g #expression, use more D.U Type
    | END

type LexData = {Txt: string; Num: int}

type SumData = {Actual: uint32; CSum: uint64 ; VSum: int64}

// ---------------------------------- Map ------------------------------------------------ //


//  create a shift map
let shiftOpMap = 
    Map.ofList [
        "LSL", LSL
        "LSR", LSR
        "ASR", ASR
        "ROR", ROR
    ]

let arithTypeMap = 
    Map.ofList [
        "ADD", ADD
        "SUB", SUB
        "RSB", RSB
        "RSC", RSC
        "ADC", ADC
        "SBC", SBC
        "CMP", CMP
        "CMN", CMN
    ]

/// sample specification for set of instructions
let dPSpec = {
    InstrC = ARI
    Roots = ["add";"sub";"rsb";"adc";"sbc";"rsc";"cmp";"cmn"]
    Suffixes = [""; "s"]
}

/// map of all possible opcodes recognised, cmps and cmns are not possible
let opCodes = opCodeExpand dPSpec |> Map.remove "cmps" |> Map.remove "cmns"


// --------------------------------- Arithmetic Functions ---------------------------------------- //


//create a triplet of (sum, cSum, vSum)
let actualSum (op1Val: uint32) (op2Val: uint32) (carry: uint32) = uint32(int(op1Val) + int(op2Val) + int(carry))
let cSum (op1Val: uint64) (op2Val: uint64) (carry: uint64) = op1Val + op2Val + carry
let vSum (op1Val:uint32) (op2Val:uint32) (carry: uint32) = int64(int32(op1Val)) + int64(int32(op2Val)) + int64(int32(carry))

let doArith op1Val (op2Val: uint64) carry = 
    let actual = actualSum op1Val (uint32 op2Val) carry
    let cSum = cSum (uint64 op1Val) op2Val (uint64 carry)
    let vSum = vSum op1Val (uint32 op2Val) carry
    {Actual = actual; CSum = cSum; VSum = vSum}


/// Do arithmetic operation from instr 
let arith (instr: Instr) (data: DataPath<'INS>) : Result<DataPath<'INS>, string> = 
    let dest = instr.Dest
    let op1 = instr.Op1.Value
    let op2 = instr.Op2
    let suf = instr.Suffix
    let operator = instr.arithType

    let carry = if data.Fl.C then 1u else 0u
    let op1Val = data.Regs.[op1]

    // Restriction on the literal
    //let makeLiteral (literalData: uint32) = 
        //let ROR m = (literalData >>> m) ||| (literalData <<<(32 - m))
        //let resList = [0..15]
        //              |> List.map (fun m' -> ROR (2*m')) 
        //              |> List.collect (fun x -> if x >= 0u && x <= 255u then [x] else [])
        //if resList.IsEmpty then None else Some (literalData)


    //sum is a record of Actual, CSum and VSum
    let setFlag (sum: SumData) = 

        let cFlag = uint64(sum.Actual) <> sum.CSum // Can be just .. <> ..  //Think of the mathematical value, not operational
        let zFlag = sum.Actual = 0u 
        let nFlag = int(sum.Actual) < 0 
        let vFlag = int64(int32(sum.Actual)) <> sum.VSum 

        { N = nFlag; C = cFlag; Z = zFlag; V = vFlag}
    

    let createSum (op1Val: uint32) (op2Val: uint32) =

        let negVal (opVal: uint32) = uint64(~~~opVal) + 1UL

        match operator with
        |Some ADD -> doArith op1Val (uint64 op2Val) 0u
        |Some CMN -> doArith op1Val (uint64 op2Val) 0u
        |Some SUB -> doArith op1Val (negVal op2Val) 0u
        |Some CMP -> doArith op1Val (negVal op2Val) 0u
        |Some RSB -> doArith op2Val (negVal op1Val) 0u
        |Some ADC -> doArith op1Val (uint64 op2Val) carry
        |Some SBC -> doArith op1Val (negVal (op2Val+1u)) carry
        |Some RSC -> doArith op2Val (negVal (op1Val+1u)) carry
        |None -> failwithf "Not supposed to happen!"

    
    let createNewReg dest (sum: SumData) = 
        match dest with 
        | Some y -> 
            let curReg = data.Regs
            [(y, (sum.Actual))]
            |> fun lst -> List.fold (fun prev x -> curReg.Add x) curReg lst //TODO:- increment PC count
        | None -> data.Regs
    
    let updateData dest op1Val op2Val = 
        let sum = createSum op1Val op2Val
        let fl = if (suf = FSet || operator = Some CMN|| operator = Some CMP) then setFlag sum else data.Fl
        let newReg = createNewReg dest sum
        Ok (PCPlus4 {Fl = fl; Regs = newReg; MM = data.MM})
                      
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





// ----------------------------- Useful Functions ------------------------------------------------ //

// TODO:- Use a LexData instead of lit
let evalExp (st: SymbolTable option) (lit: string) = 
    let exp = lit.[1..] //Remove the # sign
    
    let opMap = 
            Map.ofList [ 
                '+', (+)  
                '-', (-) 
                '*', (*)
                ]
    
    //create a list of symbols/literals                
    let symSeq = 
        exp.Split([|'+' ; '-' ; '*' |], System.StringSplitOptions.RemoveEmptyEntries)
        |> List.ofArray
    
    //create a list of operators
    let opSeq = 
        exp 
        |> fun str -> if (str.[0] = '-' || str.[0] = '+') then str else "+" + str
        |> Seq.toList
        |> List.collect (fun x -> if opMap.ContainsKey x then [x] else []) 
    
    //Use List.fold to evalute the expression
    let folder state litOrSym =
        match state, litOrSym with
        | (cummSum, hd::rst), y -> 
            let createSum sum num = opMap.[hd] sum num

            match y, st with 
            | y, Some symTab when symTab.ContainsKey y -> 
                createSum cummSum (int(symTab.[y])) , rst
            | y, _ -> 
                createSum cummSum (int(y)) , rst
        | _ ->
            (state)    

    List.fold folder (0, opSeq) symSeq
    |> fun (x, y) -> x



// ------------------------------- Lex Functions -------------------------------------------------//


let (|LexMatchD|_|) debug regex (lexData: LexData) = 
    match String.regexMatch regex (lexData.Txt) with
    | None -> 
        if debug 
        then printfn "Match of '%s' with '%s' failed." lexData.Txt regex
        None
    | Some (mStr, rst) -> 
        let mChars = String.length mStr
        if mChars = 0 then
            failwithf "Unexpected 0 character match '%s' " regex
        if debug then
            printfn "Match of '%s' with '%s' OK: match is '%s'" lexData.Txt regex mStr
        let lexData' = {lexData with Txt = lexData.Txt.[mChars..]}
        Some (mStr, lexData')

let (|LexMatch|_|) = (|LexMatchD|_|) false

let nextToken ld = 
    let incr ld = {ld with Num = ld.Num + 1}
    match ld with
    | LexMatch @"^\," (_, ld') -> None, ld'
    | LexMatch @"^[ ]" (_, ld') -> None, ld'
    | LexMatch @"^#.*" (exp, ld') -> Some (Exp (exp)), ld'
    | LexMatch @"^[rR][0-9]+" (reg, ld') -> Some (R (reg, ld'.Num)), incr ld'
    | LexMatch @"^[lL][sS][lL]" (opr, ld') -> Some (Opr (opr)), ld'
    | LexMatch @"^[lL][sS][rR]" (opr, ld') -> Some(Opr(opr)), ld'
    | LexMatch @"^[aA][sS][rR]" (opr, ld') -> Some(Opr(opr)), ld'
    | LexMatch @"^[rR][rR][xX]" (opr, ld') -> Some(Opr(opr)), ld'
    | LexMatch @"^[rR][oO][rR]" (opr, ld') -> Some(Opr(opr)), ld'
    | _ -> failwithf "Matching failed in lexer" 

let tokenize (line: string): Token List = 
    let rec tokenize' ld = 
        match ld.Txt with
        | "" -> [END]
        | _ -> 
            let nt, st' = nextToken ld
            match nt with 
            | None -> tokenize' st'
            | Some tok -> tok :: tokenize' st'
    tokenize' {Txt = line; Num = 0}


// ---------------------- Parse line data ------------------------------------- //

let parseTok (symTb: SymbolTable option) (tok: Token List) = 

    let genR r = if regNames.ContainsKey r then Some regNames.[r] else None
    let genOp2 r = 
        match genR r with
        | Some op2' -> Some (Ro op2')
        | None -> None

    match tok with
    | [R (dest, 0) ; R (op1, 1) ; Exp(exp); END] -> 

        let destR = genR dest
        let op1R = genR op1
        let op2 = 
            //TODO: - let numberCheck = System.Text.RegularExpressions.Regex("^[0-9]+$")   
            match symTb with
            |Some symtab -> 
                let expRes = convExp2Lit exp.[1..] symtab
                match expRes with
                |Ok res -> 
                    Some (Nm {K = res ; R = int32(res)})
                |Error _ -> None    
            |None ->     
                Some (Nm {K = uint32(0) ; R = 0})

        destR, op1R, op2
    // TODO:- Expression case
    //| [R (dest, 0) ; R (op1, 1) ; R(op2, 2); Opr(opr) ; Exp(exp) ; END] -> 
        //let destR = genR dest
        //let op1R = genR op1 
        //let op2R = 
             //Shifted of Reg * Shift * SVal
       
    | [R (dest, 0) ; R (op1, 1) ; R(op2, 2); END] -> 
        let destR = genR dest
        let op1R = genR op1
        let op2R = genOp2 op2
        destR, op1R, op2R

    | [R (op1, 0) ; R (op2, 1); END] -> //CMP Case
        let op1R = genR op1
        let op2R = genOp2 op2

        None, op1R, op2R

    | [R (op1, 0) ; Exp(exp) ; END] -> //CMP Case
        let op1R = genR op1
        let op2 = 
            match symTb with
            |Some symtab -> 
                let expRes = convExp2Lit exp.[1..]  symtab
                match expRes with
                |Ok res -> 
                    Some (Nm {K = res ; R = int32(res)})
                |Error _ -> None    
            |None ->     
                Some (Nm {K = uint32(0) ; R = 0})
        
        None, op1R, op2

    | _ -> 
        None, None, None

let arithParse (ls: LineData): Result<Parse<Instr>, string> option = 
    let parse' (instrC, ((root: string), suffix: string, pCond)) = 
        let suf = 
            match suffix with
            |"" -> NoFSet  
            |"s" -> FSet
            |_ -> failwithf "Impossible, it will be filtered by Option.map"

        let tokenList = tokenize ls.Operands
        let dest, op1, op2 = parseTok ls.SymTab tokenList
        let arithType = 
            if arithTypeMap.ContainsKey (root.ToUpper()) 
                then Some (arithTypeMap.[root.ToUpper()])
                else None
        let instr = {Dest = dest; Op1 = op1; Op2 = op2; Suffix = suf; arithType = arithType;Cond = pCond}       

        match instr with
        | {Dest = None; Op1 = None; Op2 = None} -> Error (sprintf "Syntax error")
        | {Dest = None} -> 
            match root with
            | x when (x = "cmp") || (x = "cmn") -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u ; PCond = pCond }
            | _ -> Error (sprintf "Error Dest")
        | {Op1 = None} -> Error (sprintf "Error Op1")
        | {Op2 = None} -> Error (sprintf "Error Op2")
        | _ -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u; PCond = pCond }
       
    Map.tryFind (ls.OpCode.ToLower()) opCodes
    |> Option.map parse'

//let parse (ls: LineData) : Result<Parse<Instr>,string> option = //Only parse the right instruction
    //let parse' (instrC, ((root: string), suffix: string, pCond)) =
    //    // this does the real work of parsing
    //    let suf = 
    //        match suffix with
    //        |"" -> false  
    //        |"s" -> true
    //        |_ -> failwithf "Impossible, it will be filtered by Option.map"

    //    //split into array of strings
    //    let splitIntoWords (line: string) =
    //        line.Split( ([||] : char array), System.StringSplitOptions.RemoveEmptyEntries)
        
    //    //split into list of strings
    //    let splitIntoList (x: string) = 
    //        x.Split(',')
    //        |> List.ofArray 
    //        |> List.map splitIntoWords
    //        |> List.map List.ofArray

    //    let listOfWords = splitIntoList (ls.Operands.ToUpper())

    //    //Used to evaluate e.g #5+5*3 case
    //    let evalLit (lit: string) (rstOfLit: string list)= 
    //        let stringVal = lit.[1..String.length lit - 1] //Remove the # sign
    //        let rstOfVal = String.concat "" rstOfLit
    //        let litOp2 = stringVal + rstOfVal
            
    //        let opMap = 
    //                Map.ofList [ 
    //                    '+', (+)  
    //                    '-', (-) 
    //                    '*', (*)
    //                    ]

    //        //create a list of symbols/literals                
    //        let symSeq = 
    //            litOp2.Split([|'+' ; '-' ; '*' |], System.StringSplitOptions.RemoveEmptyEntries)
    //            |> List.ofArray
            
    //        //create a list of operators
    //        let opSeq = 
    //            litOp2 
    //            |> fun str -> if (str.[0] = '-' || str.[0] = '+') then str else "+" + str
    //            |> Seq.toList
    //            |> List.collect (fun x -> if opMap.ContainsKey x then [x] else []) 
            
    //        //Use List.fold to evalute the expression
    //        let folder state litOrSym =
    //            match state, litOrSym with
    //            | (cummSum, hd::rst), y -> 
    //                let intVal = 
    //                    match y, ls.SymTab with //if SymTab has the reference
    //                    | y, Some symTab when symTab.ContainsKey y -> 
    //                        int (symTab.[y])
    //                    | y, _ -> 
    //                        int(y)
    //                let newSum = opMap.[hd] cummSum intVal
    //                (newSum, rst)
    //            | _ ->
    //                (state)    

    //        List.fold folder (0, opSeq) symSeq
    //        |> fun (x, y) -> x


    //     //Case of ArithInstr
    //    let buildInstr (dest:string) (op1Op2: string list) = 
    //        let dest = 
    //            match dest with
    //            | x when regNames.ContainsKey x -> Some regNames.[dest]
    //            | _ -> None //Not valid register

    //        let op1 = 
    //            match op1Op2.[0] with
    //            | x when regNames.ContainsKey x -> Some regNames.[op1Op2.[0]]
    //            | _ -> None //Not valid register

    //        let op2OrExp = 
    //            let op2' = op1Op2.[1..List.length op1Op2 - 1] 
    //            match op2' with 
    //            | str::rst -> 
    //                match str, rst with                     
    //                //Few possible cases:
    //                //i) R1
    //                //ii) R1, LSL #5
    //                //iii) R1, LSL R0
    //                //iv) #5
    //                //v) #5+5*3
    //                | x, _ when regNames.ContainsKey x -> 
    //                    match rst with 
    //                    | [] -> //Case i)
    //                        Some (Ro (regNames.[x]))
    //                    | rst'::rst'' -> //Case ii), iii)
    //                        match rst', rst'' with 
    //                        | r, [] when (r.ToLower()) = "rrx" -> //Applicable to RRX only
    //                            Some (RRX (regNames.[x]))
    //                        | r, r'::r'' -> 
    //                            match r', r'' with 
    //                            |r', [] when regNames.ContainsKey r' -> 
    //                                Some (Shifted (regNames.[x], shiftOpMap.[r], Rg regNames.[r']))
    //                            |r', r'' when r'.StartsWith "#" -> //might want to include cases where it is valid to do #5+3+3
    //                                let intNum = evalLit r' r''
    //                                Some (Shifted (regNames.[x], shiftOpMap.[r], Nms ({K = uint32(intNum); R = intNum})))
    //                            |_ -> None
    //                        | _ -> None

    //                | x, y when x.StartsWith "#" -> //rst must be empty, op2 is a lit
    //                    let intNum = evalLit x y 
    //                    Some (Nm {K = uint32(intNum) ; R = intNum})
    //                | _ -> //any other instructions are invalid
    //                    None
    //            | [] -> None //Error case
            
    //        let instr = {Dest = dest; Op1 = op1; Op2 = op2OrExp; Suffix = suf; arithType = root.ToLower()}       

    //        match instr with
    //        | {Dest = None; Op1 = None; Op2 = None} -> Error (sprintf "Syntax error")
    //        | {Dest = None} -> 
    //            match root with
    //            | x when (x = "cmp") || (x = "cmn") -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u ; PCond = pCond }
    //            | _ -> Error (sprintf "Error Dest")
    //        | {Op1 = None} -> Error (sprintf "Error Op1")
    //        | {Op2 = None} -> Error (sprintf "Error Op2")
    //        | _ -> Ok { PInstr = instr ; PLabel = None ; PSize = 4u; PCond = pCond }

    //    match root with
    //    | x when (x = "cmp" || x = "cmn") -> 
    //        match listOfWords.[listOfWords.Length - 1] with
    //        | [] -> Error (sprintf "Syntax error")
    //        | _ -> 
    //            let op1Op2 = 
    //                listOfWords
    //                |> fun lst -> List.fold (fun x lst' -> x @ lst') [] lst
    //            buildInstr "" op1Op2 //CMP/CMN has no destination

    //    | _ -> 
    //        //Check whether the syntax is correct, i.e extra , at the end
    //        match listOfWords.[listOfWords.Length - 1] with
    //        | [] -> Error (sprintf "Syntax error")
    //        | _ -> 
    //            let lst = 
    //                listOfWords
    //                |> fun lst -> List.fold (fun x lst' -> x @ lst') [] lst
    //            let dest = lst.[0]
    //            let op1Op2 = 
    //                match lst with 
    //                | hd::rst -> rst
    //                | _ -> lst
    //            buildInstr dest op1Op2
        
    //Map.tryFind (ls.OpCode.ToLower()) opCodes
    //|> Option.map parse'


/// Parse Active Pattern used by top-level code
let (|IMatch|_|) = arithParse