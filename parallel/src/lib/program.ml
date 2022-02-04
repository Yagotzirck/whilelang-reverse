(** Implementation of the functions acting on {!type:Ast_ann.program_ann}.*)

open Ast_ann;;


exception No_previous_statements;;
exception No_next_statements;;

exception Statement_without_stack of string;;
exception Statement_empty_stack of string;;

exception Root_thread_par_update;;
exception Parent_curr_stmt_not_Par;;

(** type used to discern whether the thread contains a program
    contained in the first (Left) or in the second (Right) element of the [Par] statement
    of the parent thread that created it.
*)
type child_branch = Left | Right | Root;;

(** Each thread can have max 2 running child threads at a time, since
    a [Par] statement creates two child threads and the parent thread
    waits for both children to complete before proceeding its execution.
*)
let max_num_chld_threads = 2;;

(** Given a program, adds [stmt_ann] as the program's first statement/instruction. *)
let prepend_stmt ~stmt_ann = function
  | Program_ann (prev_stmts, curr_stmt, next_stmt) -> Program_ann (prev_stmts @ [stmt_ann], curr_stmt, next_stmt);;

(** Given a program, adds [stmt_ann] as the program's last statement/instruction. *)
let append_stmt ~stmt_ann = function
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


(** Given an int value [stmt_counter] representing the current statement counter and a program [prg],
    pushes [stmt_counter] onto the source code stack associated to [prg]'s current instruction.
@raises Statement_without_stack if the current instruction doesn't have an associated source code stack.
*)
let push_stmt_counter stmt_counter prg =
  let push_inner = function
    | Program_start -> raise (Statement_without_stack "Program_start")
    | Program_end -> raise (Statement_without_stack "Program_end")
    | Par_prg_start -> raise (Statement_without_stack "Par_prg_start")
    | Par_prg_end -> raise (Statement_without_stack "Par_prg_end")
    | Par (_, _, _) -> raise (Statement_without_stack "Par")

    | Skip stmt_stk -> Skip (stmt_counter :: stmt_stk)
    | Assign (e1, e2, stmt_stk) -> Assign (e1, e2, stmt_counter :: stmt_stk)

    | Cadd (e1, e2, stmt_stk) -> Cadd (e1, e2, stmt_counter :: stmt_stk)
    | Csub (e1, e2, stmt_stk) -> Csub (e1, e2, stmt_counter :: stmt_stk)

  in
  match prg with
    | Program_ann (prev_stmts, curr_stmt, next_stmts) -> Program_ann (prev_stmts, push_inner curr_stmt, next_stmts);;

  
  let top_prev_stmt_counter prg =
    let top_inner = function
    | Program_start -> raise (Statement_without_stack "Program_start")
    | Program_end -> raise (Statement_without_stack "Program_end")
    | Par_prg_start -> raise (Statement_without_stack "Par_prg_start")
    | Par_prg_end -> raise (Statement_without_stack "Par_prg_end")
    | Par (_, _, _) -> raise (Statement_without_stack "Par")


    | Skip [] -> raise (Statement_empty_stack "Skip")
    | Skip (h :: _) -> h

    | Assign (_, _, []) -> raise (Statement_empty_stack "Assign")
    | Assign (_, _, h :: _) -> h

    | Cadd (_, _, []) -> raise (Statement_empty_stack "Cadd")
    | Cadd (_, _, h :: _) -> h

    | Csub (_, _, []) -> raise (Statement_empty_stack "Csub")
    | Csub (_, _, h :: _) -> h

  in
  match prg with
    | Program_ann ([], _, _) -> raise No_previous_statements
    | Program_ann ( h_prev :: _, _, _) -> top_inner h_prev;;

  
    let pop_prev_stmt_counter prg =
      let pop_inner = function
      | Program_start -> raise (Statement_without_stack "Program_start")
      | Program_end -> raise (Statement_without_stack "Program_end")
      | Par_prg_start -> raise (Statement_without_stack "Par_prg_start")
      | Par_prg_end -> raise (Statement_without_stack "Par_prg_end")
      | Par (_, _, _) -> raise (Statement_without_stack "Par")
  
  
      | Skip [] -> raise (Statement_empty_stack "Skip")
      | Skip (_ :: t) -> Skip t
  
      | Assign (_, _, []) -> raise (Statement_empty_stack "Assign")
      | Assign (e1, e2, _ :: t) -> Assign (e1, e2, t)
  
      | Cadd (_, _, []) -> raise (Statement_empty_stack "Cadd")
      | Cadd (e1, e2, _ :: t) -> Cadd (e1, e2, t)
  
      | Csub (_, _, []) -> raise (Statement_empty_stack "Csub")
      | Csub (e1, e2, _ :: t) -> Csub (e1, e2, t)
  
    in
    match prg with
      | Program_ann ([], _, _) -> raise No_previous_statements
      | Program_ann ( h_prev :: t_prev, curr, next) -> Program_ann ( (pop_inner h_prev) :: t_prev, curr, next);;

    
(** Given a program, returns [true] if the current statement is [Par_prg_end], [false] otherwise. *)
let is_par_prg_at_end = function
  | Program_ann (_, Par_prg_end, _) -> true
  | _ -> false;;

(** Handles the forward execution of statement {!val:Program_ann.Par_prg_end}
    (that is, a child thread terminated its program execution) at program level.

@param  chld_branch The child thread's branch ([Left] or [Right]) to determine whether the program contained in the child thread belongs
        to the first or the second program inside the {!val:Program_ann.Par} statement of the parent thread.
        If it's [Root], then an exception is raised;
@param  t_parent The parent thread whose current statement is the [Par] statement to be updated with the finished child thread's program.

@return The updated parent program.

@raise  Root_thread_par_update if [chld_branch] is [Root].
@raise  Parent_curr_stmt_not_Par if the parent program's current statement is not [Par].

*)
let update_par_prg_fwd ~chld_branch ~chld_prg ~parent_prg =
  match (chld_branch, parent_prg) with
    (* The child thread is the root thread; unlikely to happen, but at least OCaml doesn't complain about inexhaustive pattern matching *)
    | (Root, Program_ann _) -> raise Root_thread_par_update 

    (* Update the first program in the parent thread program's Par() statement *)
    | (Left, Program_ann (prev_stmts, Par(_, prg2, num_chld_done), next_stmts ) ) ->
      Program_ann (prev_stmts, Par (chld_prg, prg2, num_chld_done + 1), next_stmts)

    (* Update the second program in the parent thread program's Par() statement *)
    | (Right, Program_ann (prev_stmts, Par(prg1, _, num_chld_done), next_stmts ) ) ->
      Program_ann (prev_stmts, Par (prg1, chld_prg, num_chld_done + 1), next_stmts)
    
    (* The current statement in the parent thread program is not Par(); raise an exception *)
    | (_, Program_ann _) -> raise Parent_curr_stmt_not_Par;;


(** Given a waiting thread's program, returns a boolean value indicating whether its children
    are done with their execution ([true]) or not ([false]).

@raise  Parent_curr_stmt_not_Par if the parent program's current statement is not [Par]
        (and therefore we're most likely not dealing with a waiting thread).
*)
  let is_children_execution_done = function
      | Program_ann (_, Par (_, _, num_chld_done), _) -> num_chld_done = max_num_chld_threads
      | Program_ann _ -> raise Parent_curr_stmt_not_Par;;