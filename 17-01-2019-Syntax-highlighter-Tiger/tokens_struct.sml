(*================Structure of tokens of tiger syntax====================*)
structure Tokens_struct : TOKENS_SIG =
struct
    type linenum         = int (*variable to store line no.*)
    type token           = string*int*int*string (*token for tiger syntax*)
    fun NEWLINE(s,i,j)   =(s,i,j,"white")
    fun WHITESPACE(s,i,j)=(s,i,j,"white")
    fun KEYWORD(s,i,j)   =(s,i,j,"red")
    fun SYMBOL(s,i,j)    =(s,i,j,"blue")
    fun DIGIT(s,i,j)     =(s,i,j,"green")
    fun ID(s,i,j)        =(s,i,j,"yellow")
    fun OTHER(s,i,j)     =(s,i,j,"blue")
    fun EOF()            =("EOF",0,0," ") (*since end of file so i,j is not needed so just (0,0)*)
end