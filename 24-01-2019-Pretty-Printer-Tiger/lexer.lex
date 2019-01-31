type lexresult     = Tokens_struct.token
fun eof   ()      = Tokens_struct.EOF ()   (*end of file function*)
val  lineNum  = ref 0 (*variable to store line no.*)
val  nestedloopNum  = ref 0
val prevLineCol = ref 0 (*variable to store previous line column no.*)
(*
<COMMENT> "*/"         => (nestedloopNum := !nestedloopNum-1;if (!nestedloopNum=0) then (YYBEGIN INITIAL); Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));
*)
(*
<COMMENT> "*/"         => (YYBEGIN INITIAL; nestedloopNum := !nestedloopNum-1; Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));
*)
%%
%s COMMENT;
whitespace=[\ \r\t];
digit=[0-9];
keyword="array"|"if"|"then"|"else"|"while"|"for"|"to"|"do"|"let"|"in"|"end"|"of"|"break"|"nil"|"function"|"var"|"type"|"import"|"primitive";
symbol=","|":"|";"|"("|")"|"["|"]"|"{"|"}"|"."|"+"|"-"|"*"|"/"|"="|"<>"|"<"|"<="|">"|">="|"&"|"|"|":=";
alphabet = [a-zA-Z];
%%


\n					=> (lineNum := !lineNum+1;prevLineCol := yypos;Tokens_struct.NEWLINE(yytext,!lineNum,yypos-(!prevLineCol)));				
<INITIAL> {whitespace}+       => (Tokens_struct.WHITESPACE(yytext,!lineNum,yypos-(!prevLineCol)));
<INITIAL> {digit}+    		=> (Tokens_struct.DIGIT(yytext,!lineNum,yypos-(!prevLineCol)));
<INITIAL> {keyword}	  		=> (Tokens_struct.KEYWORD(yytext,!lineNum,yypos-(!prevLineCol)));
<INITIAL> {alphabet}*         => (Tokens_struct.ID(yytext,!lineNum,yypos-(!prevLineCol)));

<INITIAL> "/*"         => (nestedloopNum := !nestedloopNum+1; YYBEGIN COMMENT; Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));
<COMMENT> "/*"         => (nestedloopNum := !nestedloopNum+1; Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));
<COMMENT> "*/"         => (nestedloopNum := !nestedloopNum-1;if (!nestedloopNum=0) then (YYBEGIN INITIAL;Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol))) else Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));

<COMMENT> [^"*/"]         => (Tokens_struct.COMMENT(yytext,!lineNum,yypos-(!prevLineCol)));

<INITIAL> {symbol}		    => (Tokens_struct.SYMBOL(yytext,!lineNum,yypos-(!prevLineCol)));
.					=> (continue());