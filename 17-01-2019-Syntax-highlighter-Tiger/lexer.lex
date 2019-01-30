type lexresult     = Tokens_struct.token
fun eof   ()      = Tokens_struct.EOF ()   (*end of file function*)
val  lineNum  = ref 0 (*variable to store line no.*)
val prevLineCol = ref 0 (*variable to store previous line column no.*)

%%
whitespace=[\ \r\t];
digit=[0-9];
keyword="array"|"if"|"then"|"else"|"while"|"for"|"to"|"do"|"let"|"in"|"end"|"of"|"break"|"nil"|"function"|"var"|"type"|"import"|"primitive";
symbol=","|":"|";"|"("|")"|"["|"]"|"{"|"}"|"."|"+"|"-"|"*"|"/"|"="|"<>"|"<"|"<="|">"|">="|"&"|"|"|":=";
alphabet = [a-zA-Z];
%%


\n					=> (lineNum := !lineNum+1;prevLineCol := yypos;Tokens_struct.NEWLINE(yytext,!lineNum,yypos-(!prevLineCol)));				
{whitespace}+       => (Tokens_struct.WHITESPACE(yytext,!lineNum,yypos-(!prevLineCol)));
{digit}+    		=> (Tokens_struct.DIGIT(yytext,!lineNum,yypos-(!prevLineCol)));
{symbol}		    => (Tokens_struct.SYMBOL(yytext,!lineNum,yypos-(!prevLineCol)));
{keyword}	  		=> (Tokens_struct.KEYWORD(yytext,!lineNum,yypos-(!prevLineCol)));
{alphabet}*         => (Tokens_struct.ID(yytext,!lineNum,yypos-(!prevLineCol)));
.					=> (continue());