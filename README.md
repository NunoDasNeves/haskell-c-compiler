# Haskell C Compiler

##About

Writing a (very simple) C compiler in Haskell based on these tutorials and other resources:
- http://learnyouahaskell.com
- https://norasandler.com/2017/11/29/Write-a-Compiler.html
- http://www.stephendiehl.com/llvm/
- https://www.schoolofhaskell.com/school/starting-with-haskell/basics-of-haskell/8\_Parser
- https://www.tutorialspoint.com/compiler\_design/compiler\_design\_overview.htm

First time writing Haskell or a compiler.

##(Planned) Features

* Lexer -> Parser -> Semantic Analyzer -> x86 assembly generator
    * Assembly will be fed into GCC to produce machine code
* Operators: =, ==, >=, <=, >, <, !, +, -, \*, /
    * Maybe some bitwise operators too
* Control flow: if, while
    * Wanna keep this simple, might add else as well
* Types: int, int\*, int[], char, char\*, char[]
    * Probably will support & for getting variable addresses too
* Basic inline asm support, so we can do syscalls and such
    * Might just do a syscall() keyword instead..hmm

##(Planned) Limitations

* No global variables planned. A program is just a list of functions
    * Might change my mind on this
    * Might support function declaration ahead of definition
* No operator precedence planned...everything is currently right-associative. Use ()!
* No preprocessor planned (no #defines, #includes etc
* No program arguments (no char\*\*)
* No comments! These would be easy to add (only requires changes to lexer) but it's just more time
* No function pointers
* No structs
* No casts
* No array {} initialization
