(*======================================= RHS ==================================================*)
type RHS = Atom.atom list(*list of terminals/non-terminals in RHS of production*)


(* ==================================== RHS_KEY =============================================*)
structure RHS_KEY : ORD_KEY = struct (*Structure of RHS type*)
    type ord_key = RHS 
    fun compare ([], []) = EQUAL (* function for camparing list*)
        | compare ([], _) = LESS    
        | compare (_, []) = GREATER
        | compare (x :: xs, y :: ys) = 
            let 
                val temp = Atom.lexCompare(x, y)
            in
                case temp of
                    EQUAL   => compare(xs , ys)
                |   GREATER => GREATER
                |   LESS    => LESS
            end
end

(*======================================= RHSSet ==================================================*)
structure RHSSet = RedBlackSetFn (RHS_KEY)    (* Structure for set of RHS_KEY *)

(*================================ PRODUCTION, RULES, GRAMMER ================================*)
type Productions = RHSSet.set
type Rules = Productions AtomMap.map
type Grammar    = { symbols : AtomSet.set, tokens : AtomSet.set, rules : Rules }


(*------------------------------------ GRAMMER DESCRIPTION -------------------------------------*)
(*

S -> E$
E -> E + T | T
T -> T * F | F
F -> id | num | ( E )

*)

(* =========================== SYMBOLS ==================== *)
val sym = ref AtomSet.empty;  (*Set of atom, non-terminal*)
(* sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "E", Atom.atom "T", Atom.atom "F"]);  Adding SYMBOLS *)
sym := AtomSet.addList (!sym, [Atom.atom "S", Atom.atom "A", Atom.atom "B", Atom.atom "C"]);


(* =========================== TOKENS ========================== *)
val tok = ref AtomSet.empty; (*Set of atom, terminal*)
(* tok := AtomSet.addList (!tok, [Atom.atom "num", Atom.atom "id", Atom.atom "(", Atom.atom ")", Atom.atom "*", Atom.atom "+", Atom.atom "$"]); *)
tok := AtomSet.addList (!tok, [Atom.atom "d"]);


(* ========================== PRODUCTIONS ======================================== *)
val S_ = ref RHSSet.empty;
(* S_ := RHSSet.add (!S_, [Atom.atom "E", Atom.atom "$"]); *)
S_ := RHSSet.add (!S_, [Atom.atom "A"]);

(* val E_ = ref RHSSet.empty;
E_ := RHSSet.add (!E_, [Atom.atom "E", Atom.atom "+", Atom.atom "T"]);
E_ := RHSSet.add (!E_, [Atom.atom "T"]); *)
val A_ = ref RHSSet.empty;
A_ := RHSSet.add (!A_, [Atom.atom "B"]);

(* val T_ = ref RHSSet.empty;
T_ := RHSSet.add (!T_, [Atom.atom "T", Atom.atom "*", Atom.atom "F"]);
T_ := RHSSet.add (!T_, [Atom.atom "F"]); *)
val B_ = ref RHSSet.empty;
B_ := RHSSet.add (!B_, [Atom.atom "C"]);

(* val F_ = ref RHSSet.empty;
F_ := RHSSet.add (!F_, [Atom.atom "id"]);
F_ := RHSSet.add (!F_, [Atom.atom "num"]);
F_ := RHSSet.add (!F_, [Atom.atom "(", Atom.atom "E", Atom.atom ")"]); *)
val C_ = ref RHSSet.empty;
C_ := RHSSet.add (!C_, [Atom.atom "d"]);


(* ============================ RULES ======================================= *)
val rule : Rules ref = ref AtomMap.empty; (*Rules for each symbol*)
rule := AtomMap.insert (!rule, Atom.atom "S", !S_);
(* rule := AtomMap.insert (!rule, Atom.atom "E", !E_);
rule := AtomMap.insert (!rule, Atom.atom "T", !T_);
rule := AtomMap.insert (!rule, Atom.atom "F", !F_); *)
rule := AtomMap.insert (!rule, Atom.atom "A", !A_);
rule := AtomMap.insert (!rule, Atom.atom "B", !B_);
rule := AtomMap.insert (!rule, Atom.atom "C", !C_);

(* ============================== GRAMMER ==================================== *)
val Grm : Grammar = { symbols = !sym, tokens = !tok, rules = !rule };