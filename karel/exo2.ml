open Common
open Quad

(** Tapez votre programme ici. *)
let prog = [
	(* adresse 00 *)INVOKE (Karel.turn_left, 0, 0);
	(* adresse 01 *)INVOKE (Karel.turn_left, 0, 0);
	(* adresse 02 *)SETI(0, 0); (*i = 0*)
	(* adresse 03 *)SETI(1, 4); (*a = 4*)
	(* adresse 04 *)SETI(2, 1); (*increment pour i*) 
	(* adresse 05 *)GOTO_EQ(9, 0, 1);
	(* adresse 06 *)INVOKE(Karel.move, 0, 0);
	(* adresse 07 *)ADD(0, 0, 2);
	(* adresse 08 *)GOTO(5);
	(* adresse 09 *)STOP
	(* ... *)
]

(*** traitement des options et interprétation ***)

(* configuration des options *)
let opts = [
]
let  doc = "Karel Game Emulator"

(* exécution du programme *)
let process (world: Karel.world) =
	let prog = Array.of_list prog in

	let rec exec vs =
	if Vm.ended vs
	then print_string "Execution ended!\n"
	else
		begin
			let pc = Vm.get_pc vs in
			Printf.printf "%4d: %s\n" pc (Quad.to_string prog.(pc));
			let ns = Vm.step vs prog in
			let _ = Vm.invoke ns Karel.display 0 0 in
			exec ns
		end in

	let kstate = Karel.init_state world in
	let vstate = Vm.new_state Karel.invoke kstate in
	let _ = Vm.invoke vstate Karel.display 0 0 in
	exec vstate
	

(* analyse des paramètres *)
let scan args =

	let load_world path =
		let file = open_in path in
		let lexbuf = Lexing.from_channel file in
		try
			Wparser.top Wlexer.scan lexbuf;
			!Karel.world
		with
		|	Parsing.Parse_error		-> print_fatal lexbuf path "syntax error in world file"
		|	Common.LexerError msg 	-> print_fatal lexbuf path msg in
	
	match args with
	| []		-> process Karel.empty_world
	| [ world]	-> process (load_world world)
	| _			-> Arg.usage opts "ERROR: syntax: game program [map]"

(* lancement du programme *)
let _ = 
	let free_args = ref [] in
	Arg.parse opts (fun arg -> free_args := arg :: !free_args) doc;
	scan !free_args

