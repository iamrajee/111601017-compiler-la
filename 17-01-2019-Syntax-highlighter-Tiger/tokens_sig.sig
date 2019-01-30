signature TOKENS_SIG =
sig
    type linenum   = int
    
    type token     = string*int*int*string
    val NEWLINE:     string*linenum * linenum -> token
    val WHITESPACE:  string*linenum * linenum -> token
    val KEYWORD:     string*linenum * linenum -> token
    val SYMBOL:      string*linenum * linenum -> token
    val DIGIT:       string*linenum * linenum -> token
    val ID:          string*linenum * linenum -> token
    val OTHER:       string*linenum * linenum -> token

    val EOF:         unit-> token
end