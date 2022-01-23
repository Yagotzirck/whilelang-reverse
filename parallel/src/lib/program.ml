(** Implementation of the functions acting on {!type:Ast_ann.program_ann}.*)

open Ast_ann;;


exception No_previous_statements;;
exception No_next_statements;;

exception If_start_not_found;;
exception While_start_not_found;;

(** Given a program, adds [stmt_ann] as the program's first statement/instruction. *)
let prepend_stmt ~stmt_ann = function
  | Program_ann (prev_stmts, curr_stmt, next_stmt) -> Program_ann (prev_stmts @ [stmt_ann], curr_stmt, next_stmt);;

(** Given a program, adds [stmt_ann] as the program's last statement/instruction. *)
let append_stmt stmt_ann = function
  | Program_ann (prev_stmts, curr_stmt, next_stmt) -> Program_ann (prev_stmts, curr_stmt, next_stmt @ [stmt_ann]);;

(** Given a program [prg], returns:
    - A new program where [prg]'s previous statement becomes the current statement, or
    - [prg], if [prg]'s current statement is the first one (there are no previous statements.)
*)
  let prev_stmt = function
  | Program_ann ([], curr, next) -> Program_ann ([], curr, next)
  | Program_ann (prev_h :: prev_t, curr, next) -> Program_ann (prev_t, prev_h, curr :: next);;

(** Given a program [prg], returns:
    - A new program where [prg]'s next statement becomes the current statement, or
    - [prg], if [prg]'s current statement is the last one (there are no next statements.)
*)
let next_stmt = function
  | Program_ann (prev, curr, []) -> Program_ann (prev, curr, [])
  | Program_ann (prev, curr, next_h :: next_t) -> Program_ann (curr:: prev, next_h, next_t);;

(** Given a program [prg], returns a new program where [prg]'s last statement becomes the current statement. *)
let rec last_stmt = function
  | Program_ann (prev, curr, []) -> Program_ann (prev, curr, [])
  | Program_ann (prev, curr, next_h :: next_t) -> last_stmt (Program_ann (curr :: prev, next_h, next_t));;

(** Returns the statement preceding the current statement inside the program.
@raise  No_previous_statements if the previous statements' list is empty
        (e.g. the given program's current statement is the first one.)
*)
let get_prev_stmt = function
  | Program_ann ([], _, _) -> raise No_previous_statements
  | Program_ann (h::_, _, _) -> h;;


(** Returns the current statement inside the program. *)
let get_curr_stmt = function
  | Program_ann (_, curr, _) -> curr;;