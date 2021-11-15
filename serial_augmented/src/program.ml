open Ast_aug;;


exception No_previous_statements;;
exception No_next_statements;;

exception If_start_not_found;;
exception While_start_not_found;;


(* program-related functions *)
let prepend_stmt stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts @ [stmt_aug], curr_stmt, next_stmt);;

let append_stmt stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts, curr_stmt, next_stmt @ [stmt_aug]);;

  let prev_stmt = function
  | Program_aug ([], curr, next) -> Program_aug ([], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> Program_aug (prev_t, prev_h, curr :: next);;
let next_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> Program_aug (curr:: prev, next_h, next_t);;

let rec if_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise If_start_not_found
  | Program_aug (If_start outer_if :: [], curr, next) -> Program_aug (If_start outer_if :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> if_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

let rec while_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise While_start_not_found
  | Program_aug (While_start (outer_while) :: [], curr, next) -> Program_aug (While_start (outer_while) :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> while_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

let rec last_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> last_stmt (Program_aug (curr :: prev, next_h, next_t));;

let get_prev_stmt = function
  | Program_aug ([], _, _) -> raise No_previous_statements
  | Program_aug (h::_, _, _) -> h;;

let get_curr_stmt = function
  | Program_aug (_, curr, _) -> curr;;