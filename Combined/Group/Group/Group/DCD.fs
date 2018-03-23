////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                       Branch                                                       //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

///Module contains B, BL Instruction Parse and Execution
///Module Depencences: CommonData,CommonLex,LS
///LS: 
///function splitIntoWords for general parsing
///function updateRec
///FlexOp2:
///function convExpToLit
module DCD

open CommonData
open CommonLex



//------------------------------Branch Instruction Type Definition--------------------//

type ByteOrWord = |BYTE|WORD

/// instruction type for branch 
type Instr =  {Ins:ByteOrWord ; Values:uint32 list;}

/// parse error, default used
type ErrInstr = string


//-----------------------------Identify Opcode----------------------------------//



/// map of all possible opcodes recognised

let opCodes = 
    [("DCD",(DCD,("DCD","",Cal)));("DCB",(DCD,("DCB","",Cal)))]
    |>Map.ofList

let opt2Val inp = 
    match inp with
    |Some k -> k
    |None -> failwithf "will not happen"
//----------------------------Branch Parsing----------------------------//
let parse (ls: LineData) : Result<Parse<Instr>,string> option =
    let parse' (instrC, (root,suffix,pCond)) =

        let (WA la) = ls.LoadAddr 
        let instruction = 
            match root with
            |"DCD" -> WORD
            |"DCB" -> BYTE
            |_ -> failwithf "will not happen"

        let convResToOpt res = 
            match res with
            |Ok k -> Some k
            |Error _ -> None //wont be used anyway
        let checkByteValues num=
            match num with
            |num when (num>=0u&&num<256u) -> Some num
            |num when ((int num<0) && (int num = -128)) -> Some (uint32(sbyte num))
            |_ -> None        
        let valListRes= 
            match ls.SymTab with
            |None -> []
            |Some symtab-> ls.Operands|>splitStrIntoList|>List.map ((fun k -> convExp2LitNoCheck k symtab)>>convResToOpt)   
        
        let valList = 
            match List.exists (fun k -> k=None ) valListRes with
            |true -> Error "invalid values"
            |false -> Ok (List.map opt2Val valListRes)
        
        let instr = 
            match instruction with
            |WORD -> Result.map (fun k -> {Ins = instruction; Values = k;}) valList
            |BYTE -> 
                let checkedByteRes vallist = 
                    vallist
                    |>List.map checkByteValues
                let checkContainNone list = 
                    match List.exists (fun k -> k=None ) list with
                    |true -> Error "Byte inputs must be between -128 and 255"
                    |false -> Ok (List.map opt2Val list)
            
                valList
                |>Result.map checkedByteRes
                |>Result.bind checkContainNone
                |>Result.map (fun k -> {Ins = instruction; Values = k;})
             

        match instr,ls.Label,pCond with 
        |Ok (instr' ), Some label,Cal -> 
            Ok {
                PInstr=instr'; 
                PLabel = Some (label,la) ;
                PSize = 
                    match root with
                    |"DCD" -> (uint32 valListRes.Length)
                    |"DCB" -> uint32 (valListRes.Length/4+1)
                    |_ -> failwithf "will not happen!" ; 
                PCond = Cal 
                }
        |Error k, _,_ -> Error k
        |_,None,_ -> Error "Name for DCD/DCB missing"  
        |_,_,_ -> Error "Condition not allowed"

        
        


    Map.tryFind ls.OpCode opCodes // lookup opcode to see if it is known
    |> Option.map parse' // if unknown keep none, if known parse it.


/// Parse Active Pattern used by top-level code
let (|IMatch|_|) = parse

let exec startAddr (instr:Instr) (dataPath:DataPath<'INS>)= 
    let convertBytesIntoWords (list:uint32 list) = 
        let numOfZerosNeeded = 
            match list.Length%4 with
            |0 -> 0
            |k -> 4-k //num of output words
        let listOfZero = List.init numOfZerosNeeded (fun _ -> 0u)
        let endList = list@listOfZero
        let rec convert (bytesList:uint32 list) wordList = 
            match bytesList with
            |el1::el2::el3::el4::rest -> 
                let word = el1+(el2<<<8)+(el3<<<16)+(el4<<<24)
                convert rest (word::wordList)
            |[] -> List.rev wordList
            |_ -> failwithf "will not happen"
        convert endList []

    let intList = 
        match instr.Ins with
        |WORD -> instr.Values
        |BYTE -> convertBytesIntoWords instr.Values
        
    [0..4..intList.Length*4-4]
    |>List.map2 (fun data addr -> (WA (startAddr+ uint32 addr),DataLoc data)) intList
    |>updateRec dataPath.MM
    |>fun k -> {dataPath with MM = k}



        



