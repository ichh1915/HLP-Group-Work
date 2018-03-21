////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      FlexOp2                                                       //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Module contains the datatype Op2 and functions to generate them
///Module Dependencies: Common Data
module FlexOp2

open CommonData

///Type Definitions
///-----------------------------------------------------------------
[<Struct>]
type ShiftRotateOption = |LSL|ASR|LSR|ROR|RRX

type ShiftVal = |Lit of uint32 | RShift of RName

type Op2 = 
    |Literal of uint32
    |Reg of RName
    |RegS of RName*ShiftRotateOption*ShiftVal
    |RegRRX of RName

///Basic Shift/Rotate Functions
///-----------------------------------------------------------------
let rOR num (rBit:uint32) = num>>>(int rBit)|||num<<<(32-int rBit)
let lSR num sBit = (uint32(num))>>>int32(sBit)

let lSL num sBit = (uint32(num))<<<int32(sBit)

let rRX num cFlag= 
    let carryBit = 
        match cFlag with
        |true -> 1
        |false -> 0
    (uint32(num)>>> 1) + (uint32(carryBit)<<<31)

let aSR num sBit = 
    match (int32 (num)>=0) with 
        |true -> uint32(num)>>>int32(sBit)
        |false -> (uint32(num)>>> int32(sBit)) + (0xFFFFFFFFu<<<(32-int32(sBit)))
    
///Make Functions
///------------------------------------------------------------------------  
let makeShift (shiftType:ShiftRotateOption) (rOp2:RName) (rShift: RName option) (s: int option) = 
    match shiftType with
    |RRX -> Some (RegRRX rOp2)
    |_ -> 
        match (rShift,s) with
        |None,Some(lit) when (lit >=1 && lit <=31) -> Some (RegS (rOp2,shiftType,Lit (uint32 (lit))))
        |Some(reg),None -> Some (RegS (rOp2, shiftType, RShift reg))
        |None,None -> Some (RegS (rOp2, shiftType, Lit (1u)))
        |_,_ -> None

// let checkLitValidity lit = 
//     [0u..15u]
//     |> List.map (fun n -> rOR lit (2u*n)) 
//     |> List.exists (fun x -> ((x>=0u) && (x <256u)))
let checkLitValidity (literal:uint32) = 
    
    let checkOri = 
        [0..2..30]
        |>List.map (fun x -> (literal>>>x)|||(literal<<<(32-x)))  //all 16 ROR results
        |>List.exists(fun x-> ((x>=0u) && (x<=255u)))
    let checkInv = 
        let inv = ~~~literal
        [0..2..30]
        |>List.map (fun x -> (inv>>>x)|||(inv<<<(32-x)))  //all 16 ROR results
        |>List.exists(fun x-> ((x>=0u) && (x<=255u)))
    printf "checkOri is: %A" checkInv
    checkInv||checkOri

let makeLiteral (literalData:uint32) = 
    // printf "%A" ~~~literalData
    match literalData with
    | lit when checkLitValidity lit-> Some (Literal lit)
    | _ -> None


///flexOp2
///---------------------------------------------------------------------------
let flexOp2 (op2:Op2) (cpuData:DataPath<'INS>) = 
    let convertedOp2 = 
        match op2 with
        |RegS (r,s,RShift rName) -> RegS (r,s,Lit (cpuData.Regs.[rName]%32u))
        |others -> others
    match convertedOp2 with 
    | Literal lit -> lit
    | Reg reg -> cpuData.Regs.[reg]
    | RegS (rName,LSR,Lit lit) -> lSR cpuData.Regs.[rName] lit
    | RegS (rName,LSL,Lit lit) -> lSL cpuData.Regs.[rName] lit
    | RegS (rName,ASR,Lit lit) -> aSR cpuData.Regs.[rName] lit
    | RegS (rName,ROR,Lit lit) -> rOR cpuData.Regs.[rName] lit
    | RegRRX (rName) -> rRX cpuData.Regs.[rName] cpuData.Fl.C
    | _ -> failwithf("Should not happen")
