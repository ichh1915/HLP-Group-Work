///////////////////////////////////////////////////////////////////////////////////////////
//                     Common code for Instruction Definition and Parsing
///////////////////////////////////////////////////////////////////////////////////////////
module CommonLex 

    open CommonData
    
    /// ARM execution conditions
    type Condition =
        | Ceq //Z=1   
        | Cne //Z=0
        | Cmi //N=1
        | Cpl //N=0

        | Cvs //V=1
        | Cvc //V=0
        | Chs //C=1
        | Clo //C=0

        | Chi //C=1 && Z=0
        | Cls //C=0 || Z=1
        | Cge //N=V
        | Clt //N!=V

        | Cgt //Z=0 && N=V
        | Cle //Z=1 || N!=V
        | Cnv // the "never executed" condition NV - not often used!
        | Cal // the "always executed condition "AL". Used by default on no condition

    /// classes of instructions 
    type InstrClass = | MOVC  //MOV MVN
                      | BITC  //EOR AND ORR BIC
                      | SFTC  //LSL LSR ASR ROR RRX
                      | TSTC  //TST TEQ


    /// specification of set of instructions  class+root+suffixes
    ///                                       SF...  ADD...  S...EQ...
    type OpSpec = {
        InstrC: InstrClass
        Roots: string list
        Suffixes: string list
    }

    type SymbolTable = Map<string,uint32>

    /// RETUEN from instruction-specific module parsing
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
    
    ///INPUT to instruction-specific parse function
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
        OpCode: string       // "LSL{s}{cond}"
        /// string of all the operands
        Operands: string     // "R1, R2, R3, LSL, #0xF"
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
     
                                   //    opcode    class        root    suffix   instr cond
    let opCodeExpand (spec: OpSpec): Map<string, InstrClass * (string * string * Condition)> =
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