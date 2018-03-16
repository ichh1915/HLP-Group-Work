module TokenizeOperandsV2

open CommonData
open FlexOp2
open LS
open CommonLex


// type Token = |Reg of string*int     //R0-R12
//              |Com of int            //,
//              |Opr of string         //LSL 
//              |Lit of string         //#1   #0xF
//              |END

// type Op2Shift = |ASR |LSL |LSR |ROR |RRX
let op2codeMap = Map.ofList [ "LSL",LSL; "LSR",LSR; "ASR",ASR; "ROR", ROR; "RRX", RRX;]



// type FlexOp2 =
//      |Literal of uint32
//      |FReg of RName
//      |RegwithShift of RName*Op2Shift*int
//      |RegwithRegShift of RName*Op2Shift*RName


///Returned type of the parsed shift operands   
type SHIFTOps = {Dest:RName ; 
                  Op1:RName;
                  Op2:Op2 option;
                  } 
///Returned type of the parsed mov/mvn operands  
type MOVOps = {Dest:RName;  
               Op2:Op2;
               } 
///Returned type of the parsed Bitwise operands    
type BITOps = {Dest:RName; 
                Op1:RName; 
                Op2:Op2;
               } 
///Returned type of the parsed tst/teq operands    
type TSTOps = {Op1:RName; 
               Op2:Op2;
               } 

type LexerState = Normal | InComment 
type LexData = {Txt: string; State: LexerState; Numb: int}

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                              Modulo256 /Modulo32 /Test valid immediate operand                                      
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//The least significant Byte of the register content is used (0-255)
let modulo256R (r:uint32) =
    int32 (((r)<<<24)>>>24)

//The least significant 5 bits of the literal is used (0-31)
let modulo32L (l:uint32) = 
    int32 (((l)<<<27)>>>27)

// //Test if the immediate oprand is valid
// let TestValid (literal:uint32) = 
//     [0..2..30]
//     |>List.map (fun x -> literal>>>x|||literal<<<(32-x))  //all 16 ROR results
//     |>List.exists(fun x-> uint32 0<=x && x<=uint32 255)
//     |>function|true -> literal |false -> failwithf "Invalid immediate operand value"

// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// //                                    Obtain uint32 values of FlexOp2                                  
// ////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// let FlexOp2 (cpuData:DataPath<'INS>) (op2:FlexOp2):uint32 =
//     let mapRtoUint r= 
//         cpuData.Regs.[r]
                            
//     match op2 with
//     |Literal l->TestValid l 
//     |FReg r -> mapRtoUint r
//     |RegwithShift (r,ASR,i)-> int32 (mapRtoUint r >>> (i|>uint32|>TestValid|>modulo32L)) |> uint32
//     |RegwithShift (r,LSL,i)-> mapRtoUint r <<< (i|>uint32|>TestValid|>modulo32L)
//     |RegwithShift (r,LSR,i)-> mapRtoUint r >>> (i|>uint32|>TestValid|>modulo32L)
//     |RegwithShift (r,ROR,i)-> (mapRtoUint r <<< 32-(i|>uint32|>TestValid|>modulo32L)) ||| (mapRtoUint r >>> (i|>uint32|>TestValid|>modulo32L))
//     |RegwithShift (r,RRX,1)-> match cpuData.Fl.C with
//                               |true -> (mapRtoUint r >>> 1) ||| (uint32 (1<<<31))
//                               |false -> mapRtoUint r >>> 1
    
//     |RegwithRegShift (r,ASR,r')-> if modulo256R (mapRtoUint r') > 31 then 0u
//                                   else int32 (mapRtoUint r >>> modulo256R (mapRtoUint r'))|> uint32       
//     |RegwithRegShift (r,LSL,r')-> if modulo256R (mapRtoUint r') > 31 then 0u
//                                   else mapRtoUint r <<< modulo256R (mapRtoUint r')
//     |RegwithRegShift (r,LSR,r')-> if modulo256R (mapRtoUint r') > 31 then 0u
//                                   else (mapRtoUint r) >>> (int (mapRtoUint r'))
//     |RegwithRegShift (r,ROR,r')-> (mapRtoUint r <<< 32-(modulo256R (mapRtoUint r'))) ||| (mapRtoUint r >>> modulo256R (mapRtoUint r'))

//     |RegwithShift _ -> failwithf "Literal Op2 failed"
//     |RegwithRegShift _ -> failwithf "Register Op2 failed"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Check if Op2 set the C Flag                                  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let Op2SetCFlag (cpuData:DataPath<'INS>) (op2:Op2):bool option =
    let mapRtoUint r= 
        cpuData.Regs.[r]
       
    match op2 with
    |Literal _->None
    |Reg _ -> None
    |RegS (r,ASR,Lit k)-> 
        let i = int k
        if i>32 && int32 cpuData.Regs.[r] < 0 then Some true 
        else if int32 i>32 && int32 cpuData.Regs.[r] >= 0 then Some false
        else if i = 0 then None
        else if int32 (cpuData.Regs.[r]>>>(int32 i-1)) &&& 1 = 1 then Some true else Some false

    |RegS (r,LSL,Lit k)-> 
        let i = int k
        if i>32 then Some false
        else if int32 i = 0 then None
        else if int32 (cpuData.Regs.[r]<<<(int32 i-1)) < 0  then Some true else Some false
    
    |RegS (r,LSR,Lit k)->
        let i = int k 
        if i>32 then Some false 
        else if i = 0 then None
        else if int32 (cpuData.Regs.[r]>>>(int32 i-1)) &&& 1 = 1 then Some true else Some false

    |RegS (r,ROR,Lit k)-> 
        let i = int k
        let i = i % 32
        if i = 0 then None
        else if int32 (cpuData.Regs.[r]>>>(int32 i-1)) &&& 1 = 1 then Some true else Some false //note here shift is in the range 0-31
    
    |RegRRX r-> if (int32 cpuData.Regs.[r]) &&& 1 = 1 then Some true else Some false
                             
    
    |RegS (r,ASR,RShift r')-> if modulo256R cpuData.Regs.[r']>32 && modulo256R cpuData.Regs.[r] < 0 then Some true 
                                  else if modulo256R cpuData.Regs.[r']>32 && modulo256R cpuData.Regs.[r] >= 0 then Some false
                                  else if modulo256R cpuData.Regs.[r'] = 0 then None
                                  else if modulo256R (cpuData.Regs.[r]>>>(modulo256R cpuData.Regs.[r']-1)) &&& 1 = 1 then Some true else Some false
       
    |RegS (r,LSL,RShift r')-> if modulo256R cpuData.Regs.[r']>32 then Some false
                                  else if int32 cpuData.Regs.[r'] = 0 then None
                                  else if int32 (cpuData.Regs.[r]<<<(int32 cpuData.Regs.[r']-1)) < 0  then Some true else Some false

    |RegS (r,LSR,RShift r')-> if modulo256R cpuData.Regs.[r']>32 then Some false 
                                  else if int32 cpuData.Regs.[r'] = 0 then None
                                  else if int32 (cpuData.Regs.[r]>>>(int32 cpuData.Regs.[r']-1)) &&& 1 = 1 then Some true else Some false

    |RegS (r,ROR,RShift r')->     let op2r = cpuData.Regs.[r'] % 32u
                                  if op2r = 0u then None
                                  else if int32 (cpuData.Regs.[r]>>>int32 (op2r-1u)) &&& 1 = 1 then Some true else Some false

    |_->None

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Tokenise a string of all oprands                                                
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
// let (|LexMatchD|_|) debug regex state =
//     match String.regexMatch regex state.Txt with
//     | None -> if debug 
//               then printfn "Match of '%s' with '%s' failed." state.Txt regex; 
//               None
//     | Some (mStr, _) -> 
//         let mChars = String.length mStr
//         if mChars = 0 then 
//             failwithf "Unexpected 0 character match in LexMatch '%s'" regex
//         if debug then
//             printfn "Match of '%s' with '%s' OK: match is '%s" state.Txt regex mStr; 
//         let state' = {state with Txt = state.Txt.[mChars..]}
//         Some (mStr,state')
// let (|LexMatch|_|) = (|LexMatchD|_|) false

// let nextToken lData =
//     let incr st = {st with Numb = st.Numb+1}
//     let retTag tag ld = Some(tag ld.Numb), ld
//     match lData.State with
//     | InComment ->
//         match lData with
//         | _ -> None, {lData with Txt=""}
//     | Normal ->
//         match lData with 
//         | LexMatch "//" _ -> None, {lData with State=InComment}
//         | LexMatch @"^\," (_,sta) -> None, sta
//         | LexMatch @"^[ ]" (_, sta) -> None, sta
//         | LexMatch @"^#" (_, sta) -> None, sta
//         | LexMatch @"^[rR][0-9]+" (reg,sta) -> Some(Reg ( reg, sta.Numb) ), incr sta
//         | LexMatch @"^[lL][sS][lL]" (opr,sta) -> Some(Opr(opr)), sta
//         | LexMatch @"^[lL][sS][rR]" (opr,sta) -> Some(Opr(opr)), sta
//         | LexMatch @"^[aA][sS][rR]" (opr,sta) -> Some(Opr(opr)), sta
//         | LexMatch @"^[rR][rR][xX]" (opr,sta) -> Some(Opr(opr)), sta
//         | LexMatch @"^[rR][oO][rR]" (opr,sta) -> Some(Opr(opr)), sta
//         | LexMatch @"^[0-9]*[x]*[0-9a-fA-F]+" (lit,sta) -> Some(Lit(lit)), sta
//         | _ -> failwithf "Matching failed in lexer at: '%s'" lData.Txt

// let tokenize (asmline:string):Token List =
//     let rec tokenize' st =
//         match st.Txt with
//         | "" -> [END]
//         | _ -> let nt,st' = nextToken st
//                match nt with
//                | None -> tokenize' st'
//                | Some tok -> tok :: tokenize' st'
//     tokenize' {Txt=asmline;State=Normal;Numb=0}

let splitStrIntoList str = splitIntoWords str [|','|]


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Parse Token List to SHIFTOps                                  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
let ParseSHIFTOps  root (ls:LineData)(tok:string list): Result<SHIFTOps,string> =
    let (dest,op1,op2) = 
        match tok,root with
        |dest::[op1], "RRX" -> 
            let destReg = Map.tryFind dest regNames
            let op1' = 
                Map.tryFind op1 regNames
            (destReg,op1',None)
        |dest::op1::[op2],_ ->
            let destReg = Map.tryFind dest regNames
            let op1Reg = Map.tryFind op1 regNames
            let op2' = 
                match ls.SymTab with
                |None -> (Literal 0u)|>Ok |>Some
                |Some symtab -> Some (litOrReg op2 symtab)
            (destReg,op1Reg,op2')
        |_ -> (None,None,None)
       
    match (dest,op1,op2),root with
    |(Some dest',Some op1, None), "RRX" -> Ok {Dest = dest'; Op1 = op1; Op2 = None}
    |(Some dest',Some op1, Some (Ok op2)), _ ->Ok {Dest = dest'; Op1 = op1; Op2 = Some op2}
    |(None,_,_),_ -> Error "Dest error"
    |(_,None,_),_ -> Error "Op1 Error"
    |(_,_,None),_ -> Error "Op2 Error"
    |(_,_,Some (Error k)),_ -> Error k




// let ParseSHIFTOps (tok:Token list): SHIFTOps =

//     match tok with
// ///dest Op1 Op2
//     |[Reg(dest,0); Reg(op1,1); Lit(litStr); END]->
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2= Literal (litStr|>uint32|>TestValid|>modulo32L|>uint32);                                               
//                                            }                                                                             
//     |[Reg(dest,0); Reg(op1,1); Reg(op2,2); END]->
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2=FReg (regNames.[op2]); 
//                                            }                                         
// ///RRX dest Op1
//     |[Reg(dest,0); Reg(op1,1); END]->
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2=Literal (uint32 0);
//                                            }
//     |_->failwithf "Syntax Error!!"

////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Parse Token List to MOVTOps                                  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let ParseMOVOps  (ls:LineData)(tok:string list): Result<MOVOps,string> =
    let (dest,op2) = 
        match tok with
        |dest::op2 -> 
            let destReg = Map.tryFind dest regNames
            let op2' = 
                match ls.SymTab with
                |None -> (Literal 0u)|>Ok |>Some
                |Some symtab -> Some (strList2Offset op2 symtab)
            (destReg,op2')
        |_ -> (None,None)
       
    match (dest,op2) with
    |(Some dest', Some (Ok op2')) -> Ok {Dest = dest';  Op2 = op2'}
    |(None,_) -> Error "Dest error"
    |(_,None) -> Error "Op1 Error"
    |(_,Some (Error k)) -> Error k


// let ParseMOVOps (tok:Token List): MOVOps =
//     match tok with
// ///dest FlexOp2
//     |[Reg(dest,0); Lit(litStr); END]->    
//                                           {Dest=regNames.[dest]; 
//                                            Op2=Literal (uint32 litStr); 
//                                           }       
//     |[Reg(dest,0); Reg(op2,1); END]-> 
//                                           {Dest=regNames.[dest]; 
//                                            Op2=FReg (regNames.[op2]);                                              
//                                           }                                          
//     |[Reg(dest,0); Reg(op2,1); Opr(opcodeStr); Lit(litStr);END]->                                                                                                                                                                            
//                                           {Dest=regNames.[dest];                                            
//                                            Op2=RegwithShift (regNames.[op2], op2codeMap.[opcodeStr],(litStr|>int));
//                                           }
//     |[Reg(dest,0); Reg(op2,1); Opr(opcodeStr); Reg(op3,2); END] -> 
//                                           {Dest=regNames.[dest]; 
//                                            Op2=RegwithRegShift (regNames.[op2], op2codeMap.[opcodeStr],regNames.[op3])
//                                           }                                                                                                                             
    
//     |_->failwithf "Syntax Error!!"
                                    
  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Parse Token List to BITOps                                  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let ParseBITOps (ls:LineData)(tok:string list) : Result<BITOps,string> =
    let (dest,op1,op2) = 
        match tok with
        |dest::op1::op2 -> 
            let destReg = Map.tryFind dest regNames
            let op1' = Map.tryFind op1 regNames
            let op2' = 
                match ls.SymTab with
                |None -> (Literal 0u)|>Ok |>Some
                |Some symtab -> Some (strList2Offset op2 symtab)
            (destReg,op1',op2')
        |_ -> (None,None,None)
       
    match (dest,op1,op2) with
    |(Some dest',Some op1, Some (Ok op2)) ->Ok {Dest = dest'; Op1 = op1; Op2 = op2}
    |(None,_,_) -> Error "Dest error"
    |(_,None,_) -> Error "Op1 Error"
    |(_,_,None) -> Error "Op2 Error"
    |(_,_,Some (Error k)) -> Error k


// let ParseBITOps (tok:Token List): BITOps =
//     match tok with
// ///dest Op1 FlexOp2
//     |[Reg(dest,0); Reg(op1,1); Lit(litStr); END]->
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2=Literal (uint32 litStr); 
//                                            }                                                                             
//     |[Reg(dest,0); Reg(op1,1); Reg(op2,2); END]->
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2=FReg (regNames.[op2]);
//                                            }                                         
//     |[Reg(dest,0); Reg(op1,1); Reg(op2,2); Opr(opcodeStr); Lit(litStr);END] -> 
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1]; 
//                                            Op2=RegwithShift (regNames.[op2], op2codeMap.[opcodeStr],(litStr|>int) );
//                                            }
//     |[Reg(dest,0); Reg(op1,1); Reg(op2,2); Opr(opcodeStr); Reg(op3,3); END] -> 
//                                            {Dest=regNames.[dest]; 
//                                            Op1=regNames.[op1];  
//                                            Op2=RegwithRegShift (regNames.[op2], op2codeMap.[opcodeStr],regNames.[op3]);
//                                            }
//     |_->failwithf "Syntax Error!!"


////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////
//                                    Parse Token List to TSTOps                                  
////////////////////////////////////////////////////////////////////////////////////////////////////////////////////////

let ParseTSTOps  (ls:LineData)(tok:string list): Result<TSTOps,string> =
    let (op1,op2) = 
        match tok with
        |op1::op2 -> 
            let destReg = Map.tryFind op1 regNames
            let op2' = 
                match ls.SymTab with
                |None -> (Literal 0u)|>Ok |>Some
                |Some symtab -> Some (strList2Offset op2 symtab)
            (destReg,op2')
        |_ -> (None,None)
       
    match (op1,op2) with
    |(Some op1', Some (Ok op2')) -> Ok {Op1 = op1';  Op2 = op2'}
    |(None,_) -> Error "op1 error"
    |(_,None) -> Error "op2 Error"
    |(_,Some (Error k)) -> Error k



// let ParseTSTOps (tok:Token List): TSTOps =
//     match tok with
// ///Op1 FlexOp2
//     |[Reg(op1,0); Lit(litStr); END]->
//                                            {Op1=regNames.[op1]; 
//                                            Op2=Literal (uint32 litStr);
//                                            }                                                                             
//     |[Reg(op1,0); Reg(op2,1); END]->
//                                            {Op1=regNames.[op1]; 
//                                            Op2=FReg (regNames.[op2]);
//                                            }                                         
//     |[Reg(op1,0); Reg(op2,1); Opr(opcodeStr); Lit(litStr);END] -> 
//                                            {Op1=regNames.[op1]; 
//                                            Op2=RegwithShift (regNames.[op2], op2codeMap.[opcodeStr],(litStr|>int));
//                                            }
//     |[Reg(op1,0); Reg(op2,1); Opr(opcodeStr); Reg(op3,2); END] -> 
//                                            {Op1=regNames.[op1];  
//                                            Op2=RegwithRegShift (regNames.[op2], op2codeMap.[opcodeStr],regNames.[op3]);
//                                            }
//     |_->failwithf "Syntax Error!!"


