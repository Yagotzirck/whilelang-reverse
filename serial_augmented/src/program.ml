(** Implementation of the functions acting on {!type:Ast_aug.program_aug}.*)

open Ast_aug;;


exception No_previous_statements;;
exception No_next_statements;;

exception If_start_not_found;;
exception While_start_not_found;;

(** Given a program, adds [stmt_aug] as the program's first statement/instruction. *)
let prepend_stmt ~stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts @ [stmt_aug], curr_stmt, next_stmt);;

(** Given a program, adds [stmt_aug] as the program's last statement/instruction. *)
let append_stmt stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts, curr_stmt, next_stmt @ [stmt_aug]);;

(** Given a program [prg], returns:
    - A new program where [prg]'s previous statement becomes the current statement, or
    - [prg], if [prg]'s current statement is the first one (there are no previous statements.)
*)
  let prev_stmt = function
  | Program_aug ([], curr, next) -> Program_aug ([], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> Program_aug (prev_t, prev_h, curr :: next);;

(** Given a program [prg], returns:
    - A new program where [prg]'s next statement becomes the current statement, or
    - [prg], if [prg]'s current statement is the last one (there are no next statements.)
*)
let next_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> Program_aug (curr:: prev, next_h, next_t);;

(** Given a program [prg], returns a new program where [prg]'s last statement becomes the current statement. *)
let rec last_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> last_stmt (Program_aug (curr :: prev, next_h, next_t));;

(** "Rewinds" a program associated to an [Ifthenelse]'s "then" or "else" branch back to the
    first statement (that is, the statement coming after [If_start].)
@raise  If_start_not_found if [If_start] wasn't found as the program's first statement
        (e.g. the function is applied to an [Ifthenelse] program block that wasn't augmented, or
        wasn't applied to an [Ifthenelse] block at all.)
*)
let rec if_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise If_start_not_found
  | Program_aug (If_start outer_if :: [], curr, next) -> Program_aug (If_start outer_if :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> if_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

  
(** "Rewinds" a program associated to a [While]'s block back to the
    first statement (that is, the statement coming after [While_start].)
@raise  While_start_not_found if [While_start] wasn't found as the program's first statement
        (e.g. the function is applied to a [While] program block that wasn't augmented, or
        wasn't applied to a [While] block at all.)
*)
let rec while_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise While_start_not_found
  | Program_aug (While_start (outer_while) :: [], curr, next) -> Program_aug (While_start (outer_while) :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> while_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

(** Returns the statement preceding the current statement inside the program.
@raise  No_previous_statements if the previous statements' list is empty
        (e.g. the given program's current statement is the first one.)
*)
let get_prev_stmt = function
  | Program_aug ([], _, _) -> raise No_previous_statements
  | Program_aug (h::_, _, _) -> h;;


(** Returns the current statement inside the program. *)
let get_curr_stmt = function
  | Program_aug (_, curr, _) -> curr;;