type quad =
		| ADD of int * int * int
		| SUB of int * int * int
		| MUL of int * int * int
		| DIV of int * int * int
		| NAND of int * int * int
		| SET of int * int
		| SETI of int * int
		| GOTO of int
		| GOTO_EQ of int * int * int
		| GOTO_NE of int * int * int
		| GOTO_LT of int * int * int
		| GOTO_LE of int * int * int
		| GOTO_GT of int * int * int
		| GOTO_GE of int * int * int
		| CALL of int
		| RETURN
		| INVOKE of int * int * int
		| STOP


(** Convertit un quadruplet en chaîne de caractère.
	@param q	Quadruplet à convertir.
	@return		Chaîne de caractère. *)
let to_string q =
	match q with
	| ADD (d, a, b) 	-> Printf.sprintf "add(v%d, v%d, v%d)" d a b
	| SUB (d, a, b) 	-> Printf.sprintf "sub(v%d, v%d, v%d)" d a b
	| MUL (d, a, b) 	-> Printf.sprintf "mul(v%d, v%d, v%d)" d a b
	| DIV (d, a, b) 	-> Printf.sprintf "div(v%d, v%d, v%d)" d a b
	| NAND (d, a, b)	-> Printf.sprintf "nand(v%d, v%d, v%d)" d a b
	| SET (d, a)		-> Printf.sprintf "set(v%d, v%d)" d a
	| SETI (d, a) 		-> Printf.sprintf "seti(v%d, %d)" d a
	| GOTO d 			-> Printf.sprintf "goto(%d)" d
	| GOTO_EQ (d, a, b) -> Printf.sprintf "goto_eq(%d, v%d, v%d)" d a b
	| GOTO_NE (d, a, b) -> Printf.sprintf "goto_ne(%d, v%d, v%d)" d a b
	| GOTO_LT (d, a, b) -> Printf.sprintf "goto_lt(%d, v%d, v%d)" d a b
	| GOTO_LE (d, a, b) -> Printf.sprintf "goto_le(%d, v%d, v%d)" d a b
	| GOTO_GT (d, a, b) -> Printf.sprintf "goto_gt(%d, v%d, v%d)" d a b
	| GOTO_GE (d, a, b) -> Printf.sprintf "goto_ge(%d, v%d, v%d)" d a b
	| CALL d			-> Printf.sprintf "call(%d)" d
	| RETURN			-> Printf.sprintf "return"
	| INVOKE (d, a, b)	-> Printf.sprintf "invoke(%d, %d, %d)" d a b
	| STOP				-> Printf.sprintf "stop"


(** Affiche un quadruplet sur la sortie standard.
	@param q	Quadruplet à afficher. *)
let print q =
	print_string (to_string q)

let print_prog prog =

	let rec process n =
		if n >= (Array.length prog) then
			()
		else
			begin
				Printf.printf "%4d\t" n;
				print prog.(n);
				print_string "\n";
				process (n + 1)
			end in

	process 0
