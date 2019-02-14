type lexresult             = Machine.Inst option
fun eof ()                 = NONE
val  lineNum  = ref 0 (*variable to store line no.*)
val  nestedloopNum  = ref 0
val prevLineCol = ref 0 (*variable to store previous line column no.*)
fun charsToInt m (x :: xs) = charsToInt (10 * m + ord x - ord #"0") xs
  | charsToInt m []        = m

fun toSigned (#"-" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"~" :: xs) = ~ (charsToInt 0 xs)
  | toSigned (#"+" :: xs) =   charsToInt 0 xs
  | toSigned xs           =   charsToInt 0 xs

val toInt  = toSigned o String.explode

%%
%structure RPLex
whitespace=[\ \r\t\n];
digit=[0-9];
keywords= "array" | "if";
%%
{whitespace}+ => (SOME (Machine.Printincolor(yytext, Machine.white)));
"#".*\n       => (SOME (Machine.Printincolor(yytext, Machine.yellow)));
[-+*]	        => (SOME (Machine.Printincolor(yytext,Machine.green)));
{keywords}	  => (SOME (Machine.Printincolor(yytext,Machine.red)));
[a-zA-Z]*	    => (SOME (Machine.Printincolor(yytext,Machine.red)));
{digit}*	    => (SOME (Machine.Printincolor(yytext,Machine.white)));

%%
%structure RPLex
%s COMMENT;
whitespace=[\ \r\t\n];
digit=[0-9];
keyword="array"|"if"|"then"|"else"|"while"|"for"|"to"|"do"|"let"|"in"|"end"|"of"|"break"|"nil"|"function"|"var"|"type"|"import"|"primitive";
symbol=","|":"|";"|"("|")"|"["|"]"|"{"|"}"|"."|"+"|"-"|"*"|"/"|"="|"<>"|"<"|"<="|">"|">="|"&"|"|"|":=";
alphabet = [a-zA-Z];
%%

\n					=> (lineNum := !lineNum+1;prevLineCol := yypos;Machine.Printincolor(yytext, Machine.white);				
<INITIAL> {whitespace}+       => (SOME (Machine.Printincolor(yytext, Machine.white);
<INITIAL> {digit}+    		=> (SOME (Machine.Printincolor(yytext, Machine.white);

<INITIAL> "then"   		=> (SOME (Machine.Printincolor(yytext ^ "\n"^ "\t", Machine.white);
<INITIAL> "else"   		=> (SOME (Machine.Printincolor("\n" ^ yytext ^ "\n"^ "\t", Machine.white);

<INITIAL> {keyword}	  		=> (SOME (Machine.Printincolor(yytext, Machine.white);
<INITIAL> {alphabet}*         => (SOME (Machine.Printincolor(yytext, Machine.white);

<INITIAL> "/*"         => (nestedloopNum := !nestedloopNum+1; YYBEGIN COMMENT; Machine.Printincolor(yytext, Machine.white);
<COMMENT> "/*"         => (nestedloopNum := !nestedloopNum+1; Machine.Printincolor(yytext, Machine.white);
<COMMENT> "*/"         => (nestedloopNum := !nestedloopNum-1;if (!nestedloopNum=0) then (YYBEGIN INITIAL;Machine.Printincolor(yytext, Machine.white) else Machine.Printincolor(yytext, Machine.white);
<COMMENT> [^"*/"]         => (SOME (Machine.Printincolor(yytext, Machine.white);

<INITIAL> {symbol}		    => (SOME (Machine.Printincolor(yytext, Machine.white);
.					=> (continue());