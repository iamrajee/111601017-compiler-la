type pos = int
(* type lexresult = Tiger.token *)

type svalue        = Tokens.svalue
type ('a,'b) token = ('a,'b) Tokens.token
type lexresult     = (svalue,pos) token

val nested_commenting = ref 0;

fun eof() = Tokens.EOF(0,0)
fun IntFromString str = let
                            val x = Int.fromString str
                        in
                            case x of
                                    SOME n => n
                                |   NONE => 0
                        end


%%
%header (functor GrammarLexFun (structure Tokens : GRAMMAR_TOKENS));
%s COMMENT;
digit = [0-9] ;
eol = ("\n\r"|"\r\n"|"\r"|"\n") ;
whitespace = (" "|\t)  ;
letter = [a-zA-Z]   ;
esc = ("\a"|"\b"|"\f"|"\n"|"\r"|"\t"|"\v");
%%

<INITIAL> "/*"    => (
            nested_commenting := !nested_commenting + 1;
            YYBEGIN COMMENT; continue());

<COMMENT> "/*"    => (
                      nested_commenting := !nested_commenting + 1;
                      continue());

<COMMENT> "*/"    => (
                      nested_commenting := !nested_commenting - 1;
                      if (!nested_commenting = 0) then (YYBEGIN INITIAL; continue())
                      else continue());
                
<COMMENT> . | [\n]       => (continue());

<INITIAL> {eol} =>            (continue());

<INITIAL> {whitespace} => (continue());
<INITIAL> "->"         => (Tokens.PRODUCTION (yypos, yypos+2));
<INITIAL> ":="         => (Tokens.ASSIGN (yypos, yypos+2));
<INITIAL> ","          => (Tokens.COMMA (yypos, yypos+1));
<INITIAL> ";"          => (Tokens.SEMICOLON (yypos, yypos+1));
<INITIAL> "|"          => (Tokens.OR(yypos, yypos + 1));

<INITIAL> "symbols"    => (Tokens.SYMBOLS(yypos, yypos + 7));
<INITIAL> "tokens"     => (Tokens.TOKENS(yypos, yypos + 6));

<INITIAL> [a-z]        => (Tokens.TOK(yytext, yypos, yypos + 1));
<INITIAL> [A-Z]        => (Tokens.SYM(yytext, yypos, yypos + 1));

.           =>  (continue());
