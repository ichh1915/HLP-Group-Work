# Specifications 

## Parse
---
### Parsing expressions

Working. Allows arithmetic operators '+', '-' and '*'. Allows symbol. Allows hexadecimal (0x) and binary (0b) input. Whitespace not allowed within an expression. Literals that is larger than 32 bit will overflow and result will be wrong. VisUAL throws an exception. Tested with unit tests. 

### Parsing flexible op2

Working. Allows symbol. Tested with unit tests.

### Conditions

Working. Instructions EQU, FILL, DCD, DCB, END does not allow conditions. Tested manually at frontend. 

### Parsing comments

Working. Any string after the character ';' will be treated as comment for each line. Will not be read as part of the assembly code. Tested manually at frontend. 

### Symbol Table
Working. Labels will be stored in symbol table, maps to the instruction address. Exceptions: EQU, DCD, DCB, FILL. Forward references allowed (up to 10 'layers'). Tested manually at frontend. 

### Case sensitivity
Case insensitive. All characters converted to uppercase before parse. There is no difference between a symbol 'apple' and 'APPLE'. Tested with unit tests. 

## Special Instructions
---
### EQU
Summary: Declare constant 

Syntax: name equ expression

Comments: Working. Allows forward reference up to 10 'layers'. Tested manually at frontend. 

### DCD
Summary: Declare Word(s) in Memory 

Syntax: name DCD value_1, value_2, ... value_N

Comments: Working. name stored in symbol table that maps to the memory address of the first element. Tested with property tests. 


### DCB
Summary: Declare Byte(s) in Memory 

Syntax: name DCB value_1, value_2, ... value_N

Comments: Working. name stored in symbol table that maps to the memory address of the first element. Value has to be within range of a byte. Tested manually at frontend. 

### FILL
Summary: Declare Empty Word(s) in Memory

Syntax: name FILL N      (N must be a multiple of 4)

Comments: Working. name stored in symbol table that maps to the memory address of the first element. N must be a multiple of 4. Tested with property test.

## Instructions

### ADR
Summary: Address Load

Syntax: ADR{cond} dest, expression

Comments: working. Difference between PC and the input value must be creatable by rotating an 8-bit number within a 32-bit word. Doesn't agree with VisUAL. For example, ADR R0,271 works on VisUAL, but returns error in our simulator. 

### AND, EOR, BIC, ORR
Summary:Bitwise instructions

Syntax:op{S}{cond} Rd, FlexOperand2

Comments:Can be updated the C flag when calculating FlexOperand2.

Test Status: Tested with property test during individual stage. Working. 

### Branch: B, BL
Summary: Branch {with link}

Syntax: B{L}{cond} target

Comments: working. Tested with property test during individual phase. 

### Arithmetic instructions: ADD, ADC, SUB, SBC, RSB, RSC
Summary: Arithmetic instruction

Syntax: op{cond}{S} Rd, Rn, Operand2

Comments: Accept expression and literal for Operand 2. 

Issue: Able to set Rd as R13, R14 and R15 unconditionally. 

Test Status: Tested manually at top level.

### Compare: CMP, CMN
Summary: Compare and Compare Negative Instruction

Syntax: CMP{cond} Rn, Operand2
        CMN{cond} Rn, Operand2

Comments: Accept expression and literal.

Test Status: Tested manually at top level.

### LDR= 
Summary: LDR Psuedo-Instruction 

Syntax: LDR{cond} dest, =expression

Comments: Accept any values. Expression and literal treated equally. 

Test Status: Tested manually at top level.

### LDR
Summary: Load Register 

Syntax: 
1. LDR{B}{cond} dest, [source {, OFFSET}] Offset addressing
2. LDR{B}{cond} dest, [source, OFFSET]! Pre-indexed addressing
3. LDR{B}{cond} dest, [source], OFFSET Post-indexed addressing 

Comments: Working. All three indexing mode supported. 
- Offset must be within the range of -4095 to 4095. 
- Source must be within data memory. Returns Error when instruction memory is read. defined to be 0x100 by default. 
- Word mode: address must be word aligned
- Byte mode: stores the smallest 8 bit if number greater than 8 bit is used
- Returns 0 for uninitialised memory
- R15 as source allowed for word mode, deprecated for byte mode.
- Destination must be different from source register when pre or post index addressing used. 

Test Status: Tested with property test during individual stage. Working. 

### STR
Summary: Store Register 

Syntax: 
1. STR{B}{cond} dest, [source {, OFFSET}] Offset addressing
2. STR{B}{cond} dest, [source, OFFSET]! Pre-indexed addressing
3. STR{B}{cond} dest, [source], OFFSET Post-indexed addressing 

Comments: Working. All three indexing mode supported. 
- Offset must be within the range of -4095 to 4095. 
- Source must be within data memory. Returns Error when instruction memory is read. defined to be 0x100 by default. 
- Word mode: address must be word aligned
- Byte mode: stores the smallest 8 bit if number greater than 8 bit is used
- Returns 0 for uninitialised memory
- R15 not allowed as destination
- Destination must be different from source register when pre or post index addressing used. 

Test Status: Tested with property test during individual stage. Working. 


### LDM
Summary: Load Multiple Registers

Syntax: LDM[dir]{cond} source, {list of registers}

Comments: 
- dir allowed: "FA", "FD", "EA", "ED", "IA", "DB", "IB", "DA", ""
- Address must be word aligned
- Address must be in data memory. Default = 0x100
- Stack pointer (source) must be from R0-R14
- List of Registers cannot contain R13 and R15
- Stack pointer (source) cannot be in list of registers
- Duplicate registers in register list allowed

Test Status: Tested with property test during individual stage. Working. 

### STM
Summary: Store Multiple Registers

Syntax: STM[dir]{cond} source, {list of registers}

Comments: 
- dir allowed: "FA", "FD", "EA", "ED", "IA", "DB", "IB", "DA", ""
- Address must be word aligned
- Address must be in data memory. Default = 0x100
- Stack pointer (source) must be from R0-R14
- List of Registers cannot contain R13 and R15
- Stack pointer (source) cannot be in list of registers
- Duplicate registers in register list allowed

Test Status: Tested with property test during individual stage. Working. 

### MOV MVN
Summary:Move FlexOperand2 in Rd

Syntax:op{S}{cond} Rd,FlexOperand2
       
Test Status: Tested with property test during individual stage. Working. 


### LSL, LSR, ASR, ROR, RRX
Summary:Shift operations 

Syntax:
1. op{S}{cond} Rd,Rn,Rs  
2. op{S}{cond} Rd,Rm,expression 
3. RRX{S}{cond} Rd,Rm
       
Comment:
1. Only the least significant byte is used and can be in the range of 0-255
2. C flag is updated to the last bit shifted out. N and Z are updated according to the result
3. Test Status: Tested with property test during individual stage. Working. 


### TST, TEQ
Summary:Test instruction

Syntax:op{cond} Rn,FlexOperand2
       
Test Status: Tested with property test during individual stage. Working. 




