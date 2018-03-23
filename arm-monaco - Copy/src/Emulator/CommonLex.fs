
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      CommonTLex                                                    //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Common code for Instruction Definition and Parsing
///Changes made to original CommonLex:
///InstrClass=| ARI | LOG | LS | LSM | Branch
///Must be changed during group phase
///Module dependencies: CommonData
module CommonLex
open FlexOp2
open CommonData
open System.Text.RegularExpressions


/// ARM execution conditions
type Condition =

    | Ceq
    | Cne
    | Cmi
    | Cpl
    | Chi
    | Chs
    | Clo
    | Cls
    | Cge
    | Cgt
    | Cle
    | Clt
    | Cvs
    | Cvc
    | Cnv // the "never executed" condition NV - not often used!
    | Cal // the "always executed condition "AL". Used by default on no condition

/// classes of instructions (example, add/change this is needed)
type InstrClass = | ARI | BITC | LSC | LSM | Branch |MOVC |SFTC |TSTC |DCD |ADR |EQUC

/// specification of set of instructions
type OpSpec = {
    InstrC: InstrClass
    Roots: string list
    Suffixes: string list
}

type SymbolTable = Map<string,uint32>

/// result returned from instruction-specific module parsing
/// an instruction class. If symbol definitions are found in a 
/// symbol table then a complete parse will be output
/// otherwise some fields will be None
type Parse<'INS> = {
        /// value representing instruction. NB type varies with instruction class
        PInstr: 'INS 
        /// name and value of label defined on this line, if one is.
        PLabel: (string * uint32) option 
        /// number of bytes in memory taken up by this instruction
        PSize: uint32 
        /// execution condition for instruction
        PCond: Condition
    }

/// data given to instruction-specific parse function
type LineData = {
    /// memory address this instruction is loaded. Must be word address
    LoadAddr: WAddr 
    /// name of label defined on this line, if one exists
    Label: string option 
    /// table of symbols with defined values. 
    /// if this is given we are phase 2 and all symbols should be defined
    /// if this is not given we are phase 1 and no symbols are defined
    SymTab: SymbolTable option
    /// opcode string
    OpCode: string
    /// string of all the operands
    Operands: string
}

type Bkt = {
    PreBkt: string;
    InBkt : string;
    PostBkt: string;

}

/// Strings with corresponding execution condition
/// Note some conditions have multiple strings
/// Note "" is a valid condition string (always execute condition)
let condMap = [ "EQ",Ceq ; "NE",Cne ; "MI",Cmi ; "PL",Cpl ; "HI", Chi ; 
                "HS",Chs ; "LO",Clo ; "LS",Cls ; "GE",Cge ; "GT", Cgt ; 
                "LE", Cle ; "LT", Clt ; "VS",Cvs ;  "VC",Cvc ;
                "NV",Cnv ; "AL",Cal ; "",Cal; "",Cal] |> Map.ofList

/// list of all strings representing execution conditions
/// includes ""
let condStrings = 
    condMap
    |> Map.toList
    |> List.map fst
    |> List.distinct    

/// generate all possible opcode strings for given specification
/// each string is paired with info about instruction
/// and the three parts of the opcode
let opCodeExpand (spec: OpSpec) 
    //    opcode    class        root    suffix   instr cond
    : Map<string, InstrClass * (string * string * Condition)> =
    spec.Roots
    |> List.collect (fun r -> 
        spec.Suffixes
        |> List.collect (fun s -> 
            condStrings
            |> List.map (fun c -> r+s+c, (spec.InstrC,(r,s, condMap.[c])))))
            |> Map.ofList

/// function used to change PInstr field of a Result<Parse<'INS>,'E>
/// the output has this field mapped with fMap
/// or if Error has this value chnaged by fMapE
let pResultInstrMap fMap fMapE paRes =
    match paRes with
    | Ok ({PInstr=ins} as pr) -> 
        // Note subtle point. {pr with Pinst = ...} will not work here
        // That is because applying fmap changes the type of PInstr
        // and therefore the type of the record.
        Ok {
        PInstr = fMap ins 
        PLabel = pr.PLabel
        PCond = pr.PCond
        PSize = pr.PSize
        }
    | Error e -> Error (fMapE e)


//---------------------Useful functions for general parsing-------------------------//

///Split string into parts using the chars in array as separator. Converted to List, without empty elements, trimmed
let splitIntoWords ( line:string ) (charArray:char []) =
    line.Split (charArray, System.StringSplitOptions.None)
    |> Array.toList 
    |> List.map (fun k -> k.Trim ())

///Convert Char List to string
let convCharListToStr (list:char list) = System.String.Concat (Array.ofList(list))

///Maps Some to Ok, None to Error
let optToRes inp error = 
    match inp with
    |Some k -> Ok (k)
    |None -> Error error  

///remove empty string from list
let removeEmptyStr strList = 
    List.filter (fun k -> k<> "") strList

///return tuple of 2 elements (Result)
let checkTwoEl strList = 
    match strList with
    |a::[b] -> Ok (a,b)
    |[_] -> Error "Invalid syntax: Symbol not found.check symbols such as ',', '[', ']' etc"
    |_ -> Error "Invalid syntax:check symbols such as ',', '[', ']' etc"

let splitStrIntoList str = splitIntoWords str [|','|]

 
///parse string into prebracket, inbracket and postbracket        
let parseBktResGen lb rb str= 
    let leftBktSep inp= (splitIntoWords inp [|lb|]) |> checkTwoEl
    let rightBktSep inp = (splitIntoWords inp [|rb|]) |> checkTwoEl             
    match leftBktSep str with
    |Ok (prebkt,rest) -> 
        match rightBktSep rest with
        |Ok (inbkt,postbkt) -> Ok {PreBkt = prebkt; InBkt = inbkt; PostBkt = postbkt}
        |Error k -> Error k
    |Error k -> Error k

//---------------------------------Functions for Offset Parsing------------------------------//


///Arithmetic operations, used to convert expression into Literal
let arithOp op1 op2 sym = 
    match sym with
    |'+' -> op1+op2
    |'-' -> op1-op2
    |'*' -> op1*op2
    |_ -> failwithf "should not happen"

///maps shift operand string to DU, used for parsing string into Offset
let shiftMode = 
    Map.ofList [ 
        "LSL",LSL ; "LSR",LSR ; "ASR",ASR ; "ROR",ROR 
    ] 

///Converts Expression string to uint32
let convExp2LitNoCheck (str:string)  (symtab:SymbolTable) = 
    let charList = Seq.toList str

    let convStr2Lit symtab (str:string) = 
        match str with
        |str when str.StartsWith "0X" -> 
            let numWithout0x = str.[2..] 
            match Regex.IsMatch (numWithout0x,"^[0-9A-F][0-9A-F]*$") with
            |true->  System.Convert.ToInt32 (numWithout0x,16) |>uint32 |> Literal |> Some
            |false -> None

        |str when str.StartsWith "0B" -> 
            let numWithout0b = str.[2..] 
            
            match Regex.IsMatch (numWithout0b,"^[0-1][0-1]*$") with
            |true->  System.Convert.ToInt32 (numWithout0b,2)|>uint32 |> Literal |> Some
            |false -> None
        |str -> 
            match Regex.IsMatch (str,"^[0-9\-][0-9]*$") with
            |true->
                System.Int32.Parse str |>uint32 |> Literal |>Some
            |false -> Map.tryFind str symtab |> Option.map Literal

    let detectFirst list= 
        let firstOpIndex = List.tryFindIndex (fun k -> ((k = '+' )|| (k = '-') || (k = '*'))) list
        match firstOpIndex with
        |None -> 
            convCharListToStr list
            |>convStr2Lit symtab
            |>(fun k -> (k,[],'!')) //! used as a marker for operator symbols not found
        |Some index -> 
            let (first,rest) = List.splitAt index list
            let firstLitStr = convCharListToStr first
            match list.[index] with
            |'-' when index = 0-> (Some(Literal 0u),(List.tail rest),list.[index])
            |_ -> (convStr2Lit symtab firstLitStr,(List.tail rest),list.[index])
           
    let rec parseExp firstLit operator cList (symtab:SymbolTable) = 
        let (lit,rest,op) = detectFirst cList
        match op with 
        |'!' -> 
            match lit with
            |None -> Error (sprintf "Invalid Literal %A" lit)
            |Some (Literal lit') -> Ok (arithOp firstLit lit' operator)
            |_ -> Error "unexpected. should not happen."
        |_ -> 
            match lit with
            |None -> Error "Invalid Literal"
            |Some (Literal lit') ->
                let newFirstLit = arithOp firstLit lit' operator
                parseExp newFirstLit op rest symtab
            |_ -> Error "unexpected. should not happen."
    
    parseExp (0u) '+' charList symtab
    
let convExp2Lit  (str:string)  (symtab:SymbolTable) = 
    let checkFinalResult inp = 
        match checkLitValidity inp with
        |true->Ok inp
        |false -> Error (sprintf "Invalid Literal %A" inp)
    convExp2LitNoCheck str symtab
    |>Result.bind checkFinalResult


let checkOffsetValidity num = (num> -4096) && (num <4096) 

let convExp2Offset  (str:string)  (symtab:SymbolTable) = 
    let checkFinalResult inp = 
        match checkOffsetValidity (int inp) with
        |true->Ok inp
        |false -> Error (sprintf "Invalid Literal %A" inp)
    convExp2LitNoCheck str symtab
    |>Result.bind checkFinalResult
                   
    

///convert string into literal or register
let litOrReg (el: string) symtab offsetBool=
    match el with
    |lit when lit.StartsWith "#" -> 
        let litStr = lit.Substring 1
        match offsetBool with
        |true -> Result.map Literal (convExp2Offset litStr symtab)
        |false -> Result.map Literal (convExp2Lit litStr symtab)
    |reg -> 
        match Map.tryFind reg regNames with
        | Some reg' -> Ok ((Reg reg'))
        | None -> Error "invalid reg"
 

///parse string list, return op2 type, assumption: No empty string in list, all elements trimmed
let str2Offset  (symtab:SymbolTable) (strList:string list) offsetBool= 
    match strList with
    |[el1] -> litOrReg el1 symtab offsetBool//one element must be either literal or register
    |el1::[el2] -> //two element must be rrx
        match (el1,el2) with
        |(reg,"RRX") -> 
            let reg' = optToRes (Map.tryFind reg regNames) "Invalid RRX Reg"
            Result.bind (fun k -> optToRes (makeShift RRX k None None) "wont happen, RRX will always execute") reg'
        |_ -> Error "Op2 Invalid"
    |el1::el2::[el3] ->  //3 element offset must be in the form of reg,shifttype and reg/lit
        match Map.tryFind el1 regNames with
        |Some R15 -> Error "PC/R15 not allowed with shift"
        |Some reg' -> 
            match Map.tryFind el2 shiftMode with
            |Some shiftMode' -> 
                let el3' = litOrReg el3 symtab offsetBool
                match el3' with
                |Ok (Literal lit) -> optToRes (makeShift shiftMode' reg' None ( Some (int32 lit) )) "invalid lit shift operation" 
                |Ok (Reg reg) -> optToRes (makeShift shiftMode' reg' (Some reg) None) "invalid reg shift operation" 
                |Error k -> Error k
                |_ -> failwithf "Error at 3 el. will not happen"
            |None -> Error "Shift type error"
        | None -> 
            Error "invalid reg"
    |_ ->Error "Too many elements "     



//--------------------------------Functions for parsing line data ----------------------------------//



///square bracket parser
let parseSqBktRes = parseBktResGen '[' ']'

///convert string list to op2. 
let strList2Op2 strList symtab= 
            strList 
            |> List.collect ( fun a -> ((splitIntoWords a [|' '|])))
            |> removeEmptyStr
            |> fun k -> str2Offset symtab k false

let strList2Offset strList symtab= 
            strList 
            |> List.collect ( fun a -> ((splitIntoWords a [|' '|])))
            |> removeEmptyStr
            |> fun k -> str2Offset symtab k true

///update input record with provided update list
let updateRec (recIn) updateList = 
    updateList
    |>List.fold (fun old (key,newval) -> Map.add key newval old) recIn

///Return data path with PC+4
let PCPlus4 dataPath = 
    let initialPC = dataPath.Regs.[R15]
    {dataPath with Regs = dataPath.Regs.Add (R15,initialPC+4u)}