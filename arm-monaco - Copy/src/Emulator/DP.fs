//////////////////////////////////////////////////////////////////////////////////////////
//                   Sample (skeleton) instruction implementation modules
//////////////////////////////////////////////////////////////////////////////////////////


module DP 


open CommonLex
open CommonData
open FlexOp2

// ----------------------------- Types ------------------------------------------------ //

// type Literal = {K: uint32; R: int}

// type Reg = RName

// type SVal = 
//     | Nms of Literal
//     | Rg of Reg

// type Shift = 
//     | LSL 
//     | LSR 
//     | ASR 
//     | ROR 

// type Op2 = 
//     | Nm of Literal
//     | Ro of Reg
//     | Shifted of Reg * Shift * SVal
//     | RRX of Reg

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
type AriInstr = {Dest: RName option; Op1: RName; Op2: Op2; Suffix: Suf; arithType: ArithType;Cond:Condition}


//Might want to add different type of Instruction in the future
type Instr =  AriInstr 

/// parse error (dummy, but will do)
type ErrInstr = string

// type Token = 
//     | R of string*int // R0-R15, straight away use RName * int
//     | Opr of string     // e.g LSL, straight away use Shift
//     | Exp of string     // e.g #expression, use more D.U Type
//     | END

// type LexData = {Txt: string; Num: int}

type SumData = {Actual: int64 ; CSum: int64 ; VSum: int64}

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
    Roots = ["ADD";"SUB";"RSB";"ADC";"SBC";"RSC";"CMP";"CMN"]
    Suffixes = [""; "S"]
}

/// map of all possible opcodes recognised, cmps and cmns are not possible
let opCodes = opCodeExpand dPSpec |> Map.remove "CMPS" |> Map.remove "CMNS"


// --------------------------------- Arithmetic Functions ---------------------------------------- //

let myUint32 (x: int64) = (~~~ ((int64 -1) >>> 32 |> fun x -> x <<< 32)) &&& x 

//create a triplet of (sum, cSum, vSum)
let actualSum (op1Val: uint32) (op2Val: uint32) (carry: uint32) = myUint32 (int64 op1Val + int64 op2Val + int64 carry)
let cSum (op1Val: uint32) (op2Val: uint32) (carry: uint32) =  myUint32 (int64 op1Val) + myUint32 (int64 op2Val) + myUint32 (int64 carry)
let vSum (op1Val:uint32) (op2Val:uint32) (carry: uint32) = int64(int32(op1Val)) + int64(int32(op2Val)) + int64(int32(carry))

let doArith (op1Val: uint32) (op2Val: uint32) (carry: uint32) = 
    let actual = actualSum op1Val op2Val carry
    let cSum = cSum op1Val op2Val carry
    let vSum = vSum op1Val op2Val carry
    {Actual = actual; CSum = cSum; VSum = vSum}


/// Do arithmetic operation from instr 
let arith (instr: Instr) (data: DataPath<'INS>) : Result<DataPath<'INS>, string> = 
    let dest = instr.Dest
    let op1 = instr.Op1
    let op2 = instr.Op2
    let suf = instr.Suffix
    let operator = instr.arithType

    let carry = if data.Fl.C then 1u else 0u
    let op1Val = data.Regs.[op1]

    //sum is a record of Actual, CSum and VSum
    let setFlag (sum: SumData) = 
        let cFlag = int32 (sum.CSum >>> 32) <> 0
            //int64(sum.Actual) <> int64(int32 sum.CSum) // Can be just .. <> ..  //Think of the mathematical value, not operational
        let zFlag = uint32 sum.Actual = 0u 
        let nFlag = uint32 (sum.Actual >>> 31) = 1u 
        let vFlag = int64(int32(sum.Actual)) <> sum.VSum 

        { N = nFlag; C = cFlag; Z = zFlag; V = vFlag}
    
    let createSum (op1Val: uint32) (op2Val: uint32) =
        match operator with
        |ADD -> doArith op1Val op2Val 0u
        |CMN -> doArith op1Val op2Val 0u
        |SUB -> doArith op1Val (uint32 (-int32 op2Val)) 0u
        |CMP -> doArith op1Val (uint32 (-int32 op2Val)) 0u
        |RSB -> doArith op2Val (uint32 (-int32 op1Val)) 0u
        |ADC -> doArith op1Val op2Val carry
        |SBC -> doArith op1Val (uint32 (-int32(op2Val+1u))) carry
        |RSC -> doArith op2Val (uint32 (-int32(op1Val+1u))) carry
           
    let createNewReg dest (sum: SumData) = 
        match dest with 
        | Some y -> 
            let curReg = data.Regs
            [(y, (uint32 sum.Actual))]
            |> fun lst -> List.fold (fun _ x -> curReg.Add x) curReg lst 
        | None -> data.Regs

    let updateData (dest:RName option) op1Val op2Val = 
        let sum = createSum op1Val op2Val
        let fl = if (suf = FSet || operator = CMN|| operator =  CMP) then setFlag sum else data.Fl
        let newReg = createNewReg dest sum
        Ok (PCPlus4 {Fl = fl; Regs = newReg; MM = data.MM})
                      
    match op2 with
    | (RegS (reg, shiftOp, sVal)) -> 
        let n = 
            match sVal with
            | Lit y -> int(y)
            | RShift r -> int(data.Regs.[r])
        
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
            | RRX -> failwithf "will not happen"
        
        updateData dest op1Val op2Val

    | (RegRRX reg) -> 
        let op2Val = (carry <<< 31 ||| data.Regs.[reg] >>> 1)

        updateData dest op1Val op2Val

    | (Reg reg) -> //if op2 is a register
        let op2Val = data.Regs.[reg]

        updateData dest op1Val op2Val

    | (Literal litNum) ->
        let op2Val = litNum
        updateData dest op1Val op2Val

  
/// Parse Active Pattern used by top-level code


let arithParse2 (ls: LineData): Result<Parse<AriInstr>, string> option =
    let parse' (instrC, ((root: string), suffix: string, pCond)) = 
        let (WA la) = ls.LoadAddr 
        let suf = 
            match suffix with
            |"" -> NoFSet  
            |"S" -> FSet
            |_ -> failwithf "Impossible, it will be filtered by Option.map"
        let arithType = (arithTypeMap.[root.ToUpper()])
            
        let opStrList = splitIntoWords ls.Operands [|','|]
        let destReg, op1Reg, op2 = 
            match opStrList, arithType with
            |op1::op2, CMP |op1::op2, CMN -> 
                let op1Reg = Map.tryFind op1 regNames
                let op2' = 
                    match ls.SymTab with
                    |None -> Some (Ok (Literal 0u))
                    |Some symtab -> Some (strList2Op2 op2 symtab)
                (None,op1Reg,op2')
            |dest::op1::op2,_ |dest::op1::op2,_  -> 
                
                let destReg = Map.tryFind dest regNames
                let op1Reg = Map.tryFind op1 regNames
                let op2' = 
                    match ls.SymTab with
                    |None -> Some (Ok (Literal 0u))
                    |Some symtab -> Some (strList2Op2 op2 symtab)
                (destReg,op1Reg,op2')
            
            |_ -> (None,None,None)

        let instr = 
            match destReg,op1Reg,op2 with
            |Some dest,Some op1, Some (Ok op2) -> Ok {Dest = Some dest; Op1 = op1; Op2 = op2; Suffix = suf; arithType = arithType;Cond = pCond} 
            |None,Some op1, Some (Ok op2) -> Ok {Dest = None; Op1 = op1; Op2 = op2; Suffix = suf; arithType = arithType;Cond = pCond} 
            |None,_,_ -> 
                
                Error "Dest Error"
            |_,None,_ -> Error "Op1 Error"
            |_,_,Some (Error k ) -> Error k
            |_ -> Error "unexpected Error in DP"

        ///output Parse Type
        match instr with 
        |Ok (instrLS' ) -> 
            Ok {
                PInstr = instrLS';      
                PLabel =  ls.Label |> Option.map (fun lab -> lab, la) ;
                PSize = 4u; 
                PCond = pCond 
                }
        |Error k -> Error k            

    Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
    |> Option.map parse' // if unknown keep none, if known parse it.

let (|IMatch|_|) = arithParse2