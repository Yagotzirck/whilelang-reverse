(** Interpreter for the parallel version of the while language. *)


open Ast
open Ast_ann
(* open Annotate *)

(* Exceptions' definitions *)
exception Assignment_to_non_var of string;;
exception Illegal_statement_fwd_execution of string;;
exception Illegal_statement_rev_execution of string;;



(** Integer semantics' definition.
@param expr The integer expression to evaluate.
@param s The sigma store (used in case the expression contains variables whose value must be retrieved.)
@return The integer value resulting from the expression's evaluation.
*)
let rec sem_int ~expr:(expr : int_expr) ~s:(s : Sigma.sigma) : int =
  match expr with
    | Val (i) -> Sigma.get_value s i
    | Eint (x) -> x
    | Add (e1, e2) -> (sem_int e1 s) + (sem_int e2 s)
    | Sub (e1, e2) -> (sem_int e1 s) - (sem_int e2 s);;


(* Auxiliary functions for sem_stmt and sem_stmt_rev *)

(** Performs forward execution's destructive assignment:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Assigns [e2]'s evaluation to the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.dassign_fwd} for details such as pushing the previous value
    in the delta store before performing the assignment.)
@param e1 The expression containing the variable to which we must assign the new value.
@param e2 The expression containing the value to assign.
@param state The state where the assignment must be performed.
@return The new state where the assignment has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let assign_var_fwd ~e1 ~e2 ~state =
  match e1 with
    | Val (i) ->
        let value = sem_int e2 (State.get_sigma state) in
        State.dassign_fwd i value state
        
    | _ -> raise (Assignment_to_non_var "Assign");;

  
(** Performs reverse execution's destructive assignment:

  + Evaluates [e1], making sure it's a variable [ide];
  + Assigns [ide]'s previous value to [ide] and returns the state
    with the above changes applied
    (see {!val:State.dassign_rev} for details such as popping [ide]'s previous value from
    the delta store.)
@param e1 The expression containing the variable to which we must assign the previous value.
@param state The state where the assignment must be performed.
@return The new state where the assignment has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let assign_var_rev ~e1 ~state =
  match e1 with
    | Val (i) -> State.dassign_rev i state

    | _ -> raise (Assignment_to_non_var "Assign (reverse execution)");;


(** Performs constructive sum:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Sums [e2]'s evaluation to the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.cadd} for details.)
@param e1 The expression containing the variable to which we must add [e2]'s evaluation
@param e2 The expression containing the value to add to the variable specified by [e1].
@param state The state where the sum must be performed.
@return The new state where the sum has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let cadd ~e1 ~e2 ~state =
  match e1 with
  | Val (i) ->
      let value = sem_int e2 (State.get_sigma state) in
      State.cadd i value state 
    
  | _ -> raise (Assignment_to_non_var "Cadd, or Csub if execution was reversed");;

  
(** Performs constructive subtraction:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Subtracts [e2]'s evaluation from the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.csub} for details.)
@param e1 The expression containing the variable from which we must subtract [e2]'s evaluation
@param e2 The expression containing the value to subtract from the variable specified by [e1].
@param state The state where the subtraction must be performed.
@return The new state where the subtraction has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let csub ~e1 ~e2 ~state =
  match e1 with
  | Val (i) ->
        let value = sem_int e2 (State.get_sigma state) in
        State.csub i value state

  | _ -> raise (Assignment_to_non_var "Csub, or Cadd if execution was reversed");;





let exec_par_fwd prg1 prg2 ptid state =
  State.move_to_next_stmt ptid state |> State.set_thread_as_waiting ptid |> State.new_running_thread prg2 ptid Program.Right |> State.new_running_thread prg1 ptid Program.Left;;

let exec_par_rev prg ptid state =
  let adjusted_state = State.pop_prev_stmt_counter ptid state |> State.set_thread_as_waiting ptid |> State.dec_prev_par_finished_children ptid in
  let num_last_stmt = (State.get_stmt_counter state) - 1 in
  
  match Program.get_last_executed_par_prg num_last_stmt prg with
    (* If the program is found, create a thread containing the given program in the list of running threads *)
    | Some (prg, branch) -> State.new_running_thread prg ptid branch adjusted_state

    (* If the program wasn't found in the programs contained in Par program, raise an exception *)
    | None -> raise State.Last_executed_stmt_not_found;;



(** Given a thread ID and a state, performs a single evaluation step in forward execution mode
    in the program associated to the given thread ID, and returns the resulting state.
*)
let sem_stmt_fwd (tid : int) (curr_state : State.state) : State.state =
  let expr = State.get_curr_stmt tid curr_state in
  match expr with
    | Program_end -> curr_state
    | Skip _ ->  State.move_to_next_stmt tid curr_state
    | Assign (e1, e2, _) -> (assign_var_fwd e1 e2 curr_state) |> State.move_to_next_stmt tid
    | Cadd (e1, e2, _) -> (cadd e1 e2 curr_state) |> State.move_to_next_stmt tid
    | Csub (e1, e2, _) -> (csub e1 e2 curr_state) |> State.move_to_next_stmt tid

    | Par (prg1, prg2, _, _) -> exec_par_fwd prg1 prg2 tid curr_state
    | Par_prg_end -> State.handle_finished_par_thread_fwd tid curr_state

    (* The following statement expressions aren't supposed to be encountered during forward execution *)
    | Program_start -> raise (Illegal_statement_fwd_execution "Program_start")
    | Par_prg_start -> raise (Illegal_statement_fwd_execution "Par_prg_start")

  

(** Given a state, performs a single evaluation step in reverse execution mode
    by retrieving the last statement previously executed in forward execution mode,
    and returns the resulting state.
*)
let sem_stmt_rev (curr_state : State.state) : State.state =
  let adj_state = State.adjust_state_for_rev_semantics curr_state in
  let last_executed_thread = State.get_last_executed_thread adj_state in
  let tid = Thread.get_tid last_executed_thread in
  let expr = State.get_prev_stmt tid adj_state in
  match expr with
    | Program_start -> adj_state
    | Skip _ -> State.move_to_prev_stmt tid adj_state
    | Assign (e1, _, _) -> (assign_var_rev e1 adj_state) |> State.move_to_prev_stmt tid
    | Cadd (e1, e2, _) -> (csub e1 e2 adj_state) |> State.move_to_prev_stmt tid
    | Csub (e1, e2, _) -> (cadd e1 e2 adj_state) |> State.move_to_prev_stmt tid

    | Par _ -> exec_par_rev (Thread.get_program last_executed_thread) tid adj_state
    
    (* The following statement expressions aren't supposed to be encountered during reverse execution *)
    | Program_end -> raise (Illegal_statement_rev_execution "Program_end")

    | Par_prg_start -> raise (Illegal_statement_rev_execution "Par_prg_start")
    | Par_prg_end -> raise (Illegal_statement_rev_execution "Par_prg_end");;
    

(*

(** Given a state, evaluates all statements until the end of the program in forward execution mode and returns the resulting state.*)
let rec sem_prg_fwd curr_state =
  if not (State.is_prg_at_end curr_state) then
    sem_prg_fwd (sem_stmt_fwd curr_state)
  else
    curr_state;;

(** Given a state, evaluates all statements until the beginning of the program in reverse execution mode and returns the resulting state.*)
let rec sem_prg_rev curr_state =
  if not (State.is_prg_at_start curr_state) then
    sem_prg_rev (sem_stmt_rev curr_state)
  else
    curr_state;;

(** Given a state [curr_state] and a specified [num_steps] integer, let [remaining_stmts] be the number of statements 
    between the current statement in [curr_state]'s program and the [Program_end] boundary statement; then the function
    performs min([remaining_stmts], [num_steps]) statement evaluations in forward execution mode and returns the resulting state.

    If [num_steps] <= 0, [curr_state] is returned unaltered.
*)
let rec sem_prg_fwd_steps curr_state num_steps =
  if not (State.is_prg_at_end curr_state) && num_steps > 0 then
    sem_prg_fwd_steps (sem_stmt_fwd curr_state) (num_steps - 1)
  else
    curr_state;;

(** Given a state [curr_state] and a specified [num_steps] integer, let [remaining_stmts] be the number of statements 
    between the current statement in [curr_state]'s program and the [Program_start] boundary statement; then the function
    performs min([remaining_stmts], [num_steps]) statement evaluations in reverse execution mode and returns the resulting state.

    If [num_steps] <= 0, [curr_state] is returned unaltered.
*)
    let rec sem_prg_rev_steps curr_state num_steps =
  if not (State.is_prg_at_start curr_state) && num_steps > 0 then
    sem_prg_rev_steps (sem_stmt_rev curr_state) (num_steps -1)
  else
    curr_state;;


(** Given a state [curr_state] and a specified [num_steps] integer, it acts as an interface allowing both forward and reverse
    execution by calling
      - {!val:sem_prg_fwd_steps} (forward execution), if [num_steps] >= 0;
      - {!val:sem_prg_rev_steps} (reverse execution), if [num_steps] < 0 (in this case, [num_steps]'s sign gets inverted to obtain the
        positive amount of steps to execute to pass to {!val:sem_prg_rev_steps} as parameter.)
*)
let sem_prg_steps curr_state num_steps =
  if num_steps >= 0 then
    sem_prg_fwd_steps curr_state num_steps
  else
    sem_prg_rev_steps curr_state (-num_steps);;


*)