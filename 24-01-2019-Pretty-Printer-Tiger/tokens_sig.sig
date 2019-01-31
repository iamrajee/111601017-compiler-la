(*================Signature defination of tokens====================*)

signature TOKENS_SIG =
sig
    type linenum   = int   (*for variable to store line no.*)
    
    type token     = string*int*int*string (*for token for tiger syntax*)
    val NEWLINE:     string*linenum * linenum -> token
    val WHITESPACE:  string*linenum * linenum -> token
    val KEYWORD:     string*linenum * linenum -> token
    val SYMBOL:      string*linenum * linenum -> token
    val DIGIT:       string*linenum * linenum -> token
    val ID:          string*linenum * linenum -> token
    val OTHER:       string*linenum * linenum -> token
    val COMMENT:       string*linenum * linenum -> token

    val EOF:         unit-> token  (* for end of file*)
end