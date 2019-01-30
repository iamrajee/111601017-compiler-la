structure Parser_struct =
struct 
  	fun parse_file filename =
		let val file       = TextIO.openIn filename (*variable to store filename*)
		fun get _          = TextIO.input file (*function to get input*)
		val lexer          = Mlex.makeLexer get (*passing input to makeLexer function in lexer.lex.sml*)

		(*================================== FUNCTION DEFINATION======================================*)
		fun print_red    x = print ("\027[31m"^x^"\027[0m") (*functions to print colored text*)
		fun print_white  x = print ("\027[37m"^x^"\027[0m")
		fun print_green  x = print ("\027[32m"^x^"\027[0m")
		fun print_yellow x = print ("\027[33m"^x^"\027[0m")
		fun print_blue   x = print ("\027[34m"^x^"\027[0m")
		fun printToken (x,c) = (case c of                   (*main functions to print colored token*)
								"red" => (print_red x)
							|  "green"  => (print_green x)
							|  "white"  => (print_white x)
							|  "blue"   => (print_blue x)
							|  "yellow" => (print_yellow x)
							|   _       => (print ("")))


	  	fun call_lexer_fun() =
	      	let val (x,i,j,c) = lexer() (*Calling lexer to return token for new char*)
	       	in printToken (x,c);        (*Printing token*)
			if x="EOF" andalso j=0 then	(*If end of file then newline and break*)
				print ("\n") 
			else                         (*else Call function recursivily*)
				call_lexer_fun()
	    end
		(*========================================================================*)

		in call_lexer_fun();
			TextIO.closeIn file
    end

end

