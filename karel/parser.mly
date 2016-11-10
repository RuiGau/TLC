%{

open Quad
open Common
open Comp
open Karel


%}

%token BEGIN_PROG
%token BEGIN_EXEC
%token END_EXEC
%token END_PROG

%token MOVE
%token TURN_LEFT
%token TURN_OFF

%token SEMI
%token BEGIN
%token END

%token ITERATE
%token TIMES

%token WHILE
%token DO

%token IF
%token THEN

%token PICK_BEEPER
%token PUT_BEEPER
%token NEXT_TO_A_BEEPER

%token <int> INT

%token FRONT_IS_CLEAR
%token FRONT_IS_BLOCKED
%token LEFT_IS_CLEAR
%token LEFT_IS_BLOCKED
%token RIGHT_IS_CLEAR
%token RIGHT_IS_BLOCKED
%token NOT_NEXT_TO_A_BEEPER
%token FACING_NORTH
%token NOT_FACING_NORTH
%token FACING_EAST
%token NOT_FACING_EAST
%token FACING_SOUTH
%token NOT_FACING_SOUTH
%token FACING_WEST
%token NOT_FACING_WEST
%token ANY_BEEPERS_IN_BEEPER_BAG
%token NO_BEEPERS_IN_BEEPER_BAG

%type <unit> prog
%start prog

%%

prog:	BEGIN_PROG BEGIN_EXEC stmts_opt END_EXEC END_PROG
			{ () }
;



stmts_opt:	/* empty */		{ () }
|			stmts			{ () }
;

stmts:		stmt			{ () }
|			stmts SEMI stmt	{ () }
|			stmts SEMI		{ () }
;

stmt:		simple_stmt
				{ () }
| 			ITERATE INT TIMES BEGIN stmts END
				{ print_string("ITERATE MULTI") }
| 			ITERATE INT TIMES stmt
				{ print_string("ITERATE") }
| 			WHILE test DO BEGIN stmts END
				{ print_string("WHILE MULTI	") }
| 			WHILE test DO stmt
				{ print_string("WHILE") }
| 			IF test THEN stmt
				{ print_string("IF") }
| 			IF test THEN BEGIN stmts END
				{ print_string("IF MULTI") }
;




simple_stmt: TURN_LEFT
				{ gen (INVOKE (turn_left, 0, 0)) }
|			TURN_OFF
				{ gen STOP  }
|			MOVE
				{ gen (INVOKE (move, 0, 0)) }
|			PICK_BEEPER
				{ gen (INVOKE (pick_beeper, 0, 0)) }
|			PUT_BEEPER
				{ gen (INVOKE (put_beeper, 0, 0)) }
|			NEXT_TO_A_BEEPER
				{ print_string("NEXT_TO_A_BEEPER") }
;

test: FRONT_IS_CLEAR
		{ let d = new_temp() in gen (INVOKE (Karel.is_clear, Karel.front, d)); d }
| 	FRONT_IS_BLOCKED
		{ let d = new_temp() in gen (INVOKE (Karel.is_blocked, Karel.front, d)); d }
| 	LEFT_IS_CLEAR
		{ let d = new_temp() in gen (INVOKE (Karel.is_clear, Karel.left, d)); d }
| 	LEFT_IS_BLOCKED
		{ let d = new_temp() in gen (INVOKE (Karel.is_blocked, Karel.left, d)); d }
| 	RIGHT_IS_CLEAR
		{ let d = new_temp() in gen (INVOKE (Karel.is_clear, Karel.right, d)); d }
| 	RIGHT_IS_BLOCKED
		{ let d = new_temp() in gen (INVOKE (Karel.is_blocked, Karel.right, d)); d }
| 	NOT_NEXT_TO_A_BEEPER
		{ let d = new_temp() in gen (INVOKE (Karel.no_next_beeper, d, 0)); d }
| 	NEXT_TO_A_BEEPER
		{ let d = new_temp() in gen (INVOKE (Karel.next_beeper, d, 0)); d }
| 	FACING_NORTH
		{ let d = new_temp() in gen (INVOKE (Karel.facing, Karel.north, d)); d }
| 	NOT_FACING_NORTH
		{ let d = new_temp() in gen (INVOKE (Karel.not_facing, Karel.north, d)); d }
| 	FACING_EAST
		{ let d = new_temp() in gen (INVOKE (Karel.facing, Karel.east, d)); d }
| 	NOT_FACING_EAST
		{ let d = new_temp() in gen (INVOKE (Karel.not_facing, Karel.east, d)); d }
| 	FACING_SOUTH
		{ let d = new_temp() in gen (INVOKE (Karel.facing, Karel.south, d)); d }
| 	NOT_FACING_SOUTH
		{ let d = new_temp() in gen (INVOKE (Karel.not_facing, Karel.south, d)); d }
| 	FACING_WEST
		{ let d = new_temp() in gen (INVOKE (Karel.facing, Karel.west, d)); d }
| 	NOT_FACING_WEST
		{ let d = new_temp() in gen (INVOKE (Karel.not_facing, Karel.west, d)); d }
| 	ANY_BEEPERS_IN_BEEPER_BAG
		{ let d = new_temp() in gen (INVOKE (Karel.any_beeper, d, 0)); d }
| 	NO_BEEPERS_IN_BEEPER_BAG
		{ let d = new_temp() in gen (INVOKE (Karel.no_beeper, d, 0)); d }

;

