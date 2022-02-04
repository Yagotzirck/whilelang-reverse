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


(** Given a thread ID and a state, performs the following operations:

  + Pushes the statement counter onto the current statement's source code stack;
  + Increments the state's statement counter by 1;
  + Moves the current instruction of the program contained in the given thread ID to the next instruction;
  + Returns the updated state.

  This is an auxiliary function for {!val:sem_stmt_fwd}, which groups boilerplate code
  to perform once the statement has been interpreted and executed into a single function.
*)
let move_to_next_stmt tid state =
  State.push_stmt_counter tid state |> State.inc_num_stmts |> State.next_stmt tid;;


let exec_par_fwd prg1 prg2 ptid state =
  State.set_thread_as_waiting ptid state |> State.new_running_thread prg2 ptid Right |> State.new_running_thread prg1 ptid Left;;


(** Given a thread ID and a state, performs a single evaluation step in forward execution mode
    in the program associated to the given thread ID, and returns the resulting state.*)
let sem_stmt_fwd (tid : int) (curr_state : State.state) : State.state =
  let expr = State.get_curr_stmt tid curr_state in
  match expr with
    | Program_end -> curr_state
    | Skip _ ->  move_to_next_stmt tid curr_state
    | Assign (e1, e2, _) -> (assign_var_fwd e1 e2 curr_state) |> move_to_next_stmt tid
    | Cadd (e1, e2, _) -> (cadd e1 e2 curr_state) |> move_to_next_stmt tid
    | Csub (e1, e2, _) -> (csub e1 e2 curr_state) |> move_to_next_stmt tid

    | Par (prg1, prg2, _) -> exec_par_fwd prg1 prg2 tid curr_state
    | Par_prg_end -> State.handle_finished_par_thread tid curr_state

    (* The following statement expressions aren't supposed to be encountered during forward execution *)
    | Program_start -> raise (Illegal_statement_fwd_execution "Program_start")
    | Par_prg_start -> raise (Illegal_statement_fwd_execution "Par_prg_start")

  

(** Given a state, performs a single evaluation step in reverse execution mode and returns the resulting state.*)
(*
let sem_stmt_rev (curr_state : State.state) : State.state =
  let expr = State.get_prev_stmt curr_state in
  match expr with
    | Program_start -> curr_state
    | Skip _ -> curr_state |> State.prev_stmt
    | Assign (e1, _, _) -> (assign_var_rev e1 curr_state) |> State.prev_stmt
    | Cadd (e1, e2, _) -> (csub e1 e2 curr_state) |> State.prev_stmt
    | Csub (e1, e2, _) -> (cadd e1 e2 curr_state) |> State.prev_stmt

    (* The following statement expressions aren't supposed to be encountered during reverse execution *)
    | Program_end -> raise (Illegal_statement_rev_execution "Program_end")
    | Par_prg_end -> raise (Illegal_statement_rev_execution "Par_prg_end");;
    


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