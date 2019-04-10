structure Translate =
struct

val indent = ref 0

val bktab = "\b\b\b\b";

fun get_tabs n = if n = 0 then "" else "    " ^ get_tabs (n-1)

fun new_line n = ("\n" ^ get_tabs(n))

fun print_expression (Ast.INT x) = print (Int.toString (x))
|   print_expression (Ast.BINOP (x , bop , y)) =
            (   print_expression x ;
                print ("\027[1m" ^ " " ^ (Ast.binopDenote bop) ^ " "^"\027[0m") ;
                print_expression y
								(* print_expression y; *)
								(* print (";") *)
                )
| 	print_expression (Ast.LET (x, y)) =
			(	
				(* indent := !indent + 1;	 *)
				(* print ("\027[31m"^"let"^"\027[0m" ^ new_line(!indent)); *)
				print_decs (x);
				(* print ("\027[31m"^"in"^"\027[0m" ^ new_line (!indent));	 *)
				print_exps (y)
				(* indent := !indent - 1; *)
				(* print ("\027[31m"^"end"^"\027[0m") *)

			)
| 	print_expression (Ast.ID x) =
		(print ("\027[1;37m" ^ x ^ "\027[0m")
		)
| 	print_expression (Ast.FUNC(a, b)) = 
	(
		if String.compare(a,"print") = EQUAL then
			print ("\027[32m"^"document.write"^"\027[0m" ^ " (")
			(* print_expcomm (b)
			print ("')") *)
		else
			print ("\027[32m"^a^"\027[0m" ^ " (");

			
		print_expcomm (b);
		print (")")
	)
| 	print_expression (Ast.IF(a, b)) = 
	(
		print ("\027[31m"^"if"^"\027[0m"^" (");
		print_expression (a);
		print (" ) {");
		indent := !indent + 1;
		print (new_line(!indent));
		print_expression (b);
		indent := !indent - 1;
		print (new_line(!indent) ^ "}")
	)
| 	print_expression (Ast.IFELSE(a, b, c)) = 
	(
		print ("\027[3;31m"^"if"^"\027[0m"^" (");
		print_expression (a);
		print (" ) {");
		indent := !indent + 1;
		print (new_line(!indent));
		print_expression (b);
		indent := !indent - 1;
		print (new_line(!indent));
		print ("} "^"\027[3;31m"^"else"^"\027[0m"^" {");
		indent := !indent + 1;
		print (new_line(!indent));
		print_expression (c);
		indent := !indent - 1;
		print (new_line(!indent));
		print ("}")
	)
and

print_expcomm (x :: xs) = 
	(
		print_expression (x);
		if (null xs) then () else print (", ");
		print_expcomm (xs)
	)
| print_expcomm ([]) = ()

and

print_exps (x::exp_lst)   =
    (   print_expression (x);
        if (null exp_lst) then () else print ("\027[1;33m"^""^"\027[0m");
        print (new_line(!indent));
        print_exps(exp_lst))
|   print_exps []   = (print (bktab))

and 

print_decs (x :: y)		=
	(	print_dec(x);
		if (null y) then print ("") else print ("\027[1;33m"^""^"\027[0m");
		print (new_line(!indent));
		print_decs(y)
	)
| print_decs [] =	(print (bktab))

and 

print_dec (Ast.VARDEC(x, y)) = 
	(	print ("\027[34m"^"var "^"\027[0m" ^ x ^ " = ");
	 	print_expression(y)
	)

fun writeFile filename content =
    let val fd = TextIO.openOut filename
        val _ = TextIO.output (fd, content) handle e => (TextIO.closeOut fd; raise e)
        val _ = TextIO.closeOut fd
    in () end
		
fun compile []        = ()
  | compile (x :: xs) = (print_expression x ; print ("\027[1;33m"^""^"\027[0m" ^ new_line (!indent)) ; compile xs)

end



(* writeFile "/home/rajendra/111601017-compiler-lab/project/write.txt" "hi" *)
