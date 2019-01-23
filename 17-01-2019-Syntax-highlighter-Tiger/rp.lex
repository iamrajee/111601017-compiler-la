type lexresult             = Machine.Inst option
fun eof ()                 = NONE
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
[a-zA-Z]*	    => (SOME (Machine.Printincolor(yytext,Machine.white)));
{digit}*	    => (SOME (Machine.Printincolor(yytext,Machine.white)));

