////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                                      CommonTop                                                     //
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////



///Common Top provided
///Slightly modified
///For convenience in testing, allows feeding same string input into visual and function 
module CommonTop 

open CommonLex
open CommonData
open System.Text.RegularExpressions
open LS
open DCD
open EQUFILL


/// allows different modules to return different instruction types
type Instr =
    | ILSM of LSM.LSMInstr
    | ILS of LS.Instr
    | IB of Branch.BInstr
    | IDP of DP.Instr
    | ITST of TT2.Instr
    | IBIT of BT2.Instr
    | IMOV of MV2.Instr
    | ISFT of SF2.Instr
    | IDCD of DCD.Instr
    | IADR of ADR.Instr
    | IEQUFILL of EQUFILL.Instr
    | END
    
    

/// allows different modules to return different error info
/// by default all return string so this is not needed
type ErrInstr =
    | ERRILSM of LSM.ErrInstr
    | ERRILS of LS.ErrInstr
    | ERRIB of Branch.ErrInstr
    | ERRITST of TT2.ErrInstr
    | ERRIBIT of BT2.ErrInstr
    | ERRIMOV of MV2.ErrInstr
    | ERRISFT of SF2.ErrInstr
    | ERRIDP of DP.ErrInstr
    | ERRIDCD of DCD.ErrInstr
    | ERRIADR of ADR.ErrInstr
    | ERRIEQUFILL of EQUFILL.ErrInstr
    | ERRTOPLEVEL of string

/// Note that Instr in Mem and DP modules is NOT same as Instr in this module
/// Instr here is all possible isntruction values combines with a D.U.
/// that tags the Instruction class
/// Similarly ErrInstr
/// Similarly IMatch here is combination of module IMatches



let IMatch (ld: LineData) : Result<Parse<Instr>,ErrInstr> option =
    let pConv fr fe p = pResultInstrMap fr fe p |> Some
    match ld with
    | Branch.IMatch pa -> pConv IB ERRIB pa
    | LS.IMatch pa -> pConv ILS ERRILS pa
    | LSM.IMatch pa -> pConv ILSM ERRILSM pa
    | DP.IMatch pa -> pConv IDP ERRIDP pa
    | TT2.IMatch pa -> pConv ITST ERRITST pa
    | BT2.IMatch pa -> pConv IBIT ERRIBIT pa
    | MV2.IMatch pa -> pConv IMOV ERRIMOV pa
    | SF2.IMatch pa -> pConv ISFT ERRISFT pa
    | DCD.IMatch pa -> pConv IDCD ERRIDCD pa
    | ADR.IMatch pa -> pConv IADR ERRIADR pa
    | EQUFILL.IMatch pa -> pConv IEQUFILL ERRIEQUFILL pa
    | _ -> None


type CondInstr = Condition * Instr

let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string) =
    /// put parameters into a LineData record
    let makeLineData opcode operands = {
        OpCode=opcode
        Operands=String.concat " " operands
        Label=None
        LoadAddr = loadAddr
        SymTab = symtab
    }
    /// remove comments from string
    let removeComment (txt:string) =
        txt.Split(';')
        |> function 
            | [|x|] -> x 
            | [||] -> "" 
            | lineWithComment -> lineWithComment.[0]
    /// split line on whitespace into an array
    let splitIntoWords ( line:string ) =
        line.Split( ([||] : char array), 
            System.StringSplitOptions.RemoveEmptyEntries)
    /// try to parse 1st word, or 2nd word, as opcode
    /// If 2nd word is opcode 1st word must be label
    let matchLine words =
        let pNoLabel =
            match words with 
            | ["END"] -> {PInstr = END; PLabel =None; PSize = 4u; PCond = Cal}|>Ok |>Some
            | opc :: operands -> 
                makeLineData opc operands 
                |> IMatch
            | _ -> None
        match pNoLabel, words with
        | Some pa, _ -> pa
        | None, label::["END"] -> 
            let (WA addr) = loadAddr
            Ok {PInstr = END; PLabel = Some (label, addr); PSize = 4u; PCond = Cal}
    
        | None, label :: opc :: operands -> 
            match { makeLineData opc operands 
                    with Label=Some label} 
                  |> IMatch with
            | None -> 
                Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
            | Some pa -> pa
        | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
    asmLine.ToUpper()
    |> removeComment
    |> splitIntoWords
    |> Array.toList
    |> matchLine

let inputToLines (inp:string) = 
    
  
    Regex.Split(inp, "[\r\n]+")
    |>Array.toList
    |> List.map (fun k -> k.Trim ())
    |> List.filter (fun k -> k <>"" )
    |> List.filter (fun k -> (not ( k.StartsWith ";")))


let addSym (symtab:SymbolTable) parse= 
    match parse.PLabel with
    |Some label -> symtab.Add(label)
    |None -> symtab

let initialDataPath:DataPath<Instr> = 
    let regs = 
        [0u;0u;0u;0u;0u;0u;0u;0u;0u;0u;0u;0u;0u;0xFF000000u;0u;8u]
        |>List.map2 (fun a b -> (register a, b)) [0..15]
        |>Map.ofList
    let mm = Map.empty
    let flags  = { N=false; C=false; Z=false; V=false}
    {Regs = regs; MM = mm; Fl = flags}

let executeSpecialIns startAddr ins dp = 
    match ins with
    |IDCD ins'-> DCD.exec startAddr ins' dp 
    |IEQUFILL ins' when (ins'.Opcode = FILL)-> 
        EQUFILL.execFILL startAddr ins' dp
    |_ -> dp

let parseAll lines symtab dp= 
    let rec parseOneLine strlist addr parselist dp' startAddr = 
        match strlist with
        |[] -> Ok (List.rev parselist,dp',symtab)
        |a::b -> 
            let newparse = parseLine symtab (WA addr) a
            match newparse with
            |Ok parseres ->
                match parseres.PInstr with
                |IEQUFILL ins' when ins'.Opcode = EQU -> 
                    parseOneLine b addr (parseres::parselist) dp startAddr
                |IEQUFILL _| IDCD _ -> 
                    let newdp = executeSpecialIns startAddr parseres.PInstr dp'
                    let updatedParse = {parseres with PLabel =  Some (opt2Val parseres.PLabel|>fun (a,_)->a,startAddr) }
                    parseOneLine b addr (updatedParse::parselist) newdp (startAddr+parseres.PSize*4u)

                |_ ->
                    
                    let newAddr = addr+parseres.PSize
                    parseOneLine b newAddr (parseres::parselist) dp' startAddr
            |Error k -> Error k
        
    parseOneLine lines 0u [] dp dataMemAddrStart
    
let genSymTab parse1List = 
    let initSymTab = Map.empty
    let rec addSymbol parselist symtab= 
        match parselist with
        |[a] -> addSym symtab a
        |a::b -> addSymbol b (addSym symtab a)
        |[] -> symtab
    addSymbol parse1List initSymTab

let rec parseNTimes lines n symtab = 
    let parseres = parseAll lines symtab initialDataPath
    match n with 
    |1 -> 
        match parseres with
        |Ok (parselist,dp,_) -> 
            let symtab = parselist|> genSymTab 
            symtab
            |>Some
            |>(fun k -> parseAll lines k initialDataPath)
        |Error k -> Error k
            
    |_ ->
        match parseres with
        |Ok (parselist,dp,_) -> 
            let symtab' = parselist|> genSymTab 
            parseNTimes lines (n-1) (Some symtab')
        |Error k -> Error k
    




let rec storeIns addr  (datapath:DataPath<Instr>) (parses:Parse<Instr> list) = 
    match parses with
    // |[a] -> 
    //     {datapath with MM = datapath.MM.Add (WA addr,Code a.PInstr)}
        
    |a::b -> 
        match a.PInstr with
        |IDCD _ |IEQUFILL _-> 
            storeIns addr datapath b
        |_ -> storeIns (addr+a.PSize) {datapath with MM = datapath.MM.Add (WA addr,Code a.PInstr)} b
    |[] -> datapath
//Add instruction execution of equ, dcb, dcd, fill



let genParsedDPSymtab lines= 
    parseNTimes lines 10 None
    |>Result.map (fun (parselist,dp,symtab) -> (storeIns  0u dp parselist),symtab)

let runErrorMap ins res= 
    match res with
    |Ok k -> Ok k
    |Error k -> 
        match ins with
        | ILSM _ -> Error (ERRILSM k )
        | ILS _ -> Error (ERRILSM k )
        | IB _ -> Error (ERRILSM k )
        | END -> Error (ERRTOPLEVEL k)
        | IDP _ -> Error (ERRIDP k)
        | IBIT _ -> Error (ERRIBIT k)
        | IMOV _ -> Error (ERRIMOV k)
        | ITST _ -> Error (ERRITST k)
        | ISFT _ -> Error (ERRISFT k)
        | IDCD _ -> Error (ERRIDCD k)
        | IADR _ -> Error (ERRIADR k)
        | IEQUFILL _ -> Error (ERRIEQUFILL k)

let CheckCond (cpuData:DataPath<'INS>) (cond:Condition): bool = 
    match cond with
    |Ceq when cpuData.Fl.Z-> true
    |Cne when not cpuData.Fl.Z-> true
    |Cmi when cpuData.Fl.N-> true
    |Cpl when not cpuData.Fl.N-> true

    |Cvs when cpuData.Fl.V-> true
    |Cvc when not cpuData.Fl.V-> true
    |Chs when cpuData.Fl.C-> true
    |Clo when not cpuData.Fl.C-> true

    |Chi when cpuData.Fl.C && not cpuData.Fl.Z-> true
    |Cls when (not cpuData.Fl.C) || cpuData.Fl.Z-> true
    |Cge when cpuData.Fl.N = cpuData.Fl.V-> true
    |Clt when cpuData.Fl.N <> cpuData.Fl.V-> true

    |Cgt when (not cpuData.Fl.Z) && cpuData.Fl.N = cpuData.Fl.V-> true
    |Cle when cpuData.Fl.Z || cpuData.Fl.N <> cpuData.Fl.V-> true
    |Cnv-> false
    |Cal-> true

    |_-> false    

let execIfTrue func bool obj = 
    match bool with
    |true -> func obj
    |false -> obj


let executeAnyInstr (ins:Instr) (dp:DataPath<Instr>) : Result<DataPath<Instr>, ErrInstr> = 
    let execute dp= 
        match ins with
        | ILSM ins' when CheckCond dp ins'.Cond-> LSM.execLSM ins' dp |> runErrorMap ins
        | ILS (LS ins') when CheckCond dp ins'.Cond-> LS.execLS (LS ins') dp |> runErrorMap ins
        | ILS (LDRP ins') when CheckCond dp ins'.Cond-> LS.execLS (LDRP ins') dp |> runErrorMap ins
        | IB ins' when CheckCond dp ins'.Cond-> Branch.execB ins' dp |>runErrorMap ins
        | IDP ins' when CheckCond dp ins'.Cond-> DP.arith ins' dp |> runErrorMap ins
        | IBIT ins' when CheckCond dp ins'.Cond-> BT2.BitwiseExecute dp ins' |>Ok
        | IMOV ins' when CheckCond dp ins'.Cond-> MV2.MovsExecute dp ins' |> Ok
        | ITST ins' when CheckCond dp ins'.Cond-> TT2.TestExecute dp ins' |> Ok
        | ISFT ins' when CheckCond dp ins'.Cond-> SF2.ShiftExecute dp ins' |> Ok
        | IADR ins' when CheckCond dp ins'.Cond-> ADR.exec ins' dp |> runErrorMap ins
        |_ -> Ok (PCPlus4 dp)
    
    let condMet = CheckCond dp 
    execute dp    

let rec simulate addr (dp:DataPath<Instr>) = 
    let memContent = dp.MM.TryFind(WA addr)
    match memContent with
    |Some (Code END) -> Ok dp
    |Some (Code ins) -> 
        let newDP = executeAnyInstr ins dp
        Result.bind (fun k -> simulate (k.Regs.[R15]-8u) k) newDP
    |(Some (DataLoc _)) |_ when addr >= 0x100u -> Error (ERRTOPLEVEL "Error: data memory accessed when fetching instruction")
    |None -> Ok {dp with Regs = dp.Regs.Add (R15,dp.Regs.[R15]-4u)}
    |_ -> failwithf "unexpected error"
