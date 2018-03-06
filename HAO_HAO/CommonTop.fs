////////////////////////////////////////////////////////////////////////////////////
//      Code defined at top level after the instruction processing modules
////////////////////////////////////////////////////////////////////////////////////
module CommonTop 

    open CommonData
    open CommonLex
    open SF
    open MV


    /// allows different modules to return different instruction types
    type UInstr =
        | IMV of MV.Instr
        | ISF of SF.Instr
    
    /// allows different modules to return different error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMV of MV.ErrInstr
        | ERRISF of SF.ErrInstr
        | ERRTOPLEVEL of string

 
    /// Note that Instr in Mem and DP modules is NOT same as Instr in this module
    /// Instr here is all possible isntruction values combines with a D.U.
    /// that tags the Instruction class
    /// Similarly ErrInstr
    /// Similarly IMatch here is combination of module IMatches

    let IMatch (ld: LineData) : Result<Parse<UInstr>,ErrInstr> option =
        let pConv fr fe p = pResultInstrMap fr fe p |> Some
        match ld with
        | MV.IMatch pa -> pConv IMV ERRIMV pa
        | SF.IMatch pa -> pConv ISF ERRISF pa
        | _ -> None
    
    
    type CondInstr = Condition * Instr


    let parseLine (symtab: SymbolTable option) (loadAddr: WAddr) (asmLine:string):Result<Parse<UInstr>,ErrInstr> =
        /// put parameters into a LineData record
        let makeLineData opcode operands = {
            OpCode=opcode
            Operands=String.concat "" operands
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
                | opc :: operands -> 
                    makeLineData opc operands 
                    |> IMatch
                | _ -> None
            match pNoLabel, words with
            | Some pa, _ -> pa
            | None, label :: opc :: operands -> 
                match { makeLineData opc operands 
                        with Label=Some label} 
                      |> IMatch with
                | None -> 
                    Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %s" opc))
                | Some pa -> pa
            | _ -> Error (ERRTOPLEVEL (sprintf "Unimplemented instruction %A" words))
        asmLine
        |> removeComment
        |> splitIntoWords
        |> Array.toList
        |> matchLine
    

    let executeAnyInstr (ins: UInstr) (d: DataPath): Result<DataPath, string> =
        let execute =
            match ins with
            | IMEM ins ->Memory.execute |> runErrorMap
            | ISF ins -> SF.execute |> runErrorMap
        execute d


    /// must include all changeable machine state, 
    /// including writeable memory, registers, flags

    /// allows different modules to return different instruction types
    type Instr =
        | IMEM of Memory.Instr
        | ISF of SF.Instr

    /// allows different modules to return different parse error info
    /// by default all return string so this is not needed
    type ErrInstr =
        | ERRIMEM of Memory.ErrInstr
        | ERRIDP of DP.ErrInstr
        | ERRTOPLEVEL of string

    /// allows different modules to return different parse error info
    /// by default all return string so this is not needed
    /// NB - these are not yet implemented in sample code

        type ErrRun =
        | RUNERRMEM of Memory.ErrRun
        | RUNERRSF of SF.ErrRun
        | RUNERRTOPLEVEL of string

       


    open CommonData
    /// test the initProjectLexer code
    let test = parseLine None (WA 0u)

