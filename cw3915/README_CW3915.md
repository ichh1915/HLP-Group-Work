# Contribution to the Group Deliverable
My code includes all arithmetic instructions which includes: "ADD, SUB, RSB, ADC, SBC, RSC, CMP, CMN". The code works for almost all instructions allowed in ViSUAL, which includes adding suffix, expression such as #3+3-5 and #symtab. However, conditional execution, label, limitation on the arithmetic operation on Register R13, and updating PC by count of 4 is not yet implemented in the code. Detailed specification will be mentioned in section Specification.


## Interface
To interface with the code, in the VProgram.fs, parse your arithmetic instruction to the function test. Function test returns a result of parse, which is matched by the function arithInstr to return the updated DataPath. The sample codes are commented in VProgram, you can change it to all possible allowable combination of ADD(S), SUB(S), RSB(S), ADC(S), SBC(S), RSC(S), CMP, CMN. Uppercase or lowercase of the instruction will both work in the program. 


## Files
The files that I added the code are:

* `DP.fs` Data processing module, which contains parse and arithmetic operation from the parse result. 
* `MyVTest.fs` Tests of all possible instructions allowed by the code, i,e random literal tests, random register tests, specified test and property test on ADDS, SUBS, RSBS. 
* `VProgram.fs` Top-level code which includes sample code.


## Specification
The following arithmetic operations work just like ViSUAL:
* Able to parse all arithmetic operations aforementioned, which includes:
	* Use of suffix
	* Op2 can take the form of expression evaluated from literal or symbol with constant (with operator '+', '-', '* ' only). Note, however, the symbol creation is not implemented in this code, i.e symb1 equ 100 instruction will not be recognised by the code yet, but will be implemented by another group member. 
	* Op2 can take shift operator including LSL, LSR, ASR, ROR, RRX with expression of value stored in register or literal, e.g ADDS R0, R1, R2, LSL #3+3+sym is allowed. 
	* The restriction that Op2 literal cannot be shifted is also imposed, just like ViSUAL, e.g ADDS R0, R1, #5, LSL #1 is not allowed. 
	* Flags will be set correctly wihen suffix S is declared, and will be carried down the instruction until the next S is requested.
	* DataPath will be updated correctly, except for R15 (PC). 
	* CMP and CMN can only take Op1 and Op2 which no destination, e.g CMP R0, R1 is allowed, but not CMP R0, R1, R2, and will set the flag correctly.
	* Upper case or lower case will both work in the code. 
	* Literal can take the form decimal, and hex with prefix '0x', and binary with prefix '0b', e.g ADDS R0, R1, #0x55. 
* Aforementioned specifications are implemented in DP.fs on functions arith and parse. The error instruction will be generated on parse, refer to the comments in DP.fs.

The following are NOT yet implemented:
* Restriction on literal being creatble by rotating an 8-bit number right by an even number of bits within a 32-bit word is not implemented in the code, the function is however written, it will be implemented in the top level, but not within the arith function. This restriction on literal is imposed on the random numbers generated under random literal tests under MyVTest. 
* The PC count is not incremented by 4 after each line. This is because incrementing it will make the tests more tricky. This will be taken care of in the group work. 
* Restriction on R13 (SP) is not imposed, for it will make the arithmetic test more tricky as well. The restriction will be better understood and implemented when working together in the group. 
* Label is defaulted to None, and is not used/updated in this code. 
* Brackets "(",")" in expression is not allowed, and will cause the program to crash. 


## Tests
The tests mainly consist of rndLitTstLst, rndRegTstLst, definedTstLst, and some property tests.

* rndLitTstLst 
This test generates a random number between 0 - 255, and is passed to function makeLiteral to make sure it is a valid literal for ViSUAL. This is then combined to form instruction in the following format: {Arith}, R2, R0, #{Literal}; {Arith} is a list of allowable arithmetic instructions aforementioned, and {Literal} is the random literal generated. 10 random literals are generated.  

* rndRegTstLst 
This test generates a random registers and combine them arbitarily. This takes the form of {Arith}, {dest}, {op1}, {op2}, which {dest}, {op1}, {op2} are randomised registers fed into a list of {Arith} instructions. 

* definedTstLst
Contains some edge cases or special cases that need to be tested, such as evaluation of literals, and Shift operations on Op2. 

* Property Tests
The property tests are done on instructions ADDS, SUBS and RSBS only. The property tested being ADDS R0, R1, #Literal will be the same as SUBS R0, R1, #-Literal, except for the case where Literal = 0, and SUBS R0, R1, R2 is equal to RSBS R0, R2, R1. Three tests have been done to make sure the properties imposed are correct. 

Al tests above passed. 