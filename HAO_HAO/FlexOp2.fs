module FlexOp2
/// ARM Status bits
type Flags = { N: bool; C:bool; Z: bool; V:bool}

/// ARM register names
[<Struct>]
type RName = R0 | R1 | R2 | R3 | R4 | R5 | R6 | R7 | R8 
             | R9 | R10 | R11 | R12 | R13 | R14 | R15

/// ARM state as values of all registers and status bits
type DataPath = {Fl: Flags; Regs:Map<RName,uint32>}

/// Map used to convert strings into RName values, 
/// includes register aliasses PC, LR, SP
let regNames = Map.ofList [ 
                     "R0",R0 ; "R1",R1 ; "R2",R2 ; "R3",R3 ; "R4",R4 ; 
                     "R5",R5 ; "R6",R6 ; "R7",R7 ; "R8",R8 ;
                     "R9", R9 ; "R10",R10 ; "R11",R11 ; "R12",R12 ; 
                     "R13",R13 ; "R14",R14 ; "R15",R15 ; 
                     "PC",R15 ; "LR",R14 ; "SP",R13 ]       ///("R0",R0) 


/// Inverse of regNames, used to convert RName values to strings
/// NB The string chosen will always be the register (not alias)
let regStrings = 
    regNames
    |> Map.toList
    |> List.map (fun (s,rn)-> (rn,s)) 
    |> List.filter (fun (_,s:string) -> s.StartsWith "R")
    |> Map.ofList


/// Map converts RName into register number (no aliasses)
let regNums = Map.map (fun _ (s:string) -> int (s.[1..])) regStrings


/// Map converts register number into RName (no aliasses)
let inverseRegNums = 
    regNums 
    |> Map.toList 
    |> List.map (fun (rn,n)->(n,rn)) 
    |> Map.ofList

/// Property on RName to return register number, for convenience
/// Aliasses not included, since they are not RNames
type RName with
    /// Return the number of a register as an integer
    member r.RegNum = regNums.[r]

/// Return a register name from an integer
let register n = if 0 <= n && n < 16 
                 then inverseRegNums.[n] 
                 else (failwithf "Register %d does not exist!" n)


type Shift = ASR | LSL | LSR | ROR | RRX
type Op2 =
    |Literal of uint32
    |Reg of RName
    |RegwithShift of RName*Shift*int
    |RegwithRegShift of RName*Shift*RName
 
      
let FlexOp2 (op2:Op2) (cpuData:DataPath) :uint32 =
    let mapRtoUint r= 
        cpuData.Regs.[r]
    let modulo32 r =
        int32 ((mapRtoUint r) % 32ul)
       
    match op2 with
    |Literal l->l
    |Reg r -> mapRtoUint r
    |RegwithShift (r,ASR,i)-> int32 (mapRtoUint r >>> i) |> uint32
    |RegwithShift (r,LSL,i)-> mapRtoUint r <<< i
    |RegwithShift (r,LSR,i)-> mapRtoUint r >>> i
    |RegwithShift (r,ROR,i)-> (mapRtoUint r <<< 32-i) ||| (mapRtoUint r >>> i)
    |RegwithShift (r,RRX,1)-> match cpuData.Fl.C with
                              |true -> (mapRtoUint r >>> 1) ||| (uint32 (1<<<31))
                              |false -> mapRtoUint r >>> 1
    
    |RegwithRegShift (r,ASR,r')-> int32 (mapRtoUint r >>> modulo32 r')|> uint32       
    |RegwithRegShift (r,LSL,r')-> mapRtoUint r <<< modulo32 r'
    |RegwithRegShift (r,LSR,r')-> mapRtoUint r >>> modulo32 r'
    |RegwithRegShift (r,ROR,r')-> (mapRtoUint r <<< 32-(modulo32 r')) ||| (mapRtoUint r >>> modulo32 r')

    | RegwithShift _ -> failwithf "Literal Op2 failed"
    | RegwithRegShift _ -> failwithf "Register Op2 failed"



let makeASR (rOp2:RName) (rShift:RName option) (s:int option) : Op2 option =
    match rShift,s with
    |Some r,None -> Some (RegwithRegShift (rOp2,ASR,r))
    |None,Some i-> Some (RegwithShift (rOp2,ASR,i))
    |_->None

let makeLSL (rOp2:RName) (rShift:RName option) (s:int option) : Op2 option =
    match rShift,s with
    |Some r,None -> Some (RegwithRegShift (rOp2,LSL,r))
    |None,Some i-> Some (RegwithShift (rOp2,LSL,i))
    |_->None

let makeLSR (rOp2:RName) (rShift:RName option) (s:int option) : Op2 option =
    match rShift,s with
    |Some r,None -> Some (RegwithRegShift (rOp2,LSR,r))
    |None,Some i-> Some (RegwithShift (rOp2,LSR,i))
    |_->None

let makeROR (rOp2:RName) (rShift:RName option) (s:int option) : Op2 option =
    match rShift,s with
    |Some r,None -> Some (RegwithRegShift (rOp2,ROR,r))
    |None,Some i-> Some (RegwithShift (rOp2,ROR,i))
    |_->None

let makeRRX (rOp2:RName) : Op2 option =
    Some (RegwithShift (rOp2,RRX,1))

let makeLiteral (literalData:uint32) :Op2 option = 
    let TestValid (data:uint32) :bool = 
        [0..2..30]
        |>List.map (fun x -> data>>>x|||data<<<(32-x))  //all 16 ROR results
        |>List.exists(fun x-> uint32 0<=x && x<=uint32 255)
    
    match TestValid literalData with
    |true->Some (Literal literalData)
    |false->None
          
