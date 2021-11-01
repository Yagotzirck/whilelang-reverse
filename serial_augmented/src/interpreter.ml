(* Augmented interpreter for the serial version of the while language. *)


open Ast
open Ast_aug
open State
open Augment

(* Exceptions' definitions *)
exception Assignment_to_non_var of string;;
exception Illegal_statement_fwd_execution of string;;
exception Illegal_statement_rev_execution of string;;




let rec sem_int (expr : int_expr) (s : sigma) : int =
  match expr with
    | Val (i) -> get_value s i
    | Eint (x) -> x
    | Add (e1, e2) -> (sem_int e1 s) + (sem_int e2 s)
    | Sub (e1, e2) -> (sem_int e1 s) - (sem_int e2 s);;

let rec sem_bool (expr : bool_expr) (s : sigma) : bool =
  match expr with
    | Ebool (b) -> b
    | Not (b) -> not (sem_bool b s)
    | Eq (e1, e2) -> (sem_int e1 s) = (sem_int e2 s)
    | Gt (e1, e2) -> (sem_int e1 s) > (sem_int e2 s)
    | And (b1, b2) -> (sem_bool b1 s) && (sem_bool b2 s);;


(* Auxiliary functions for sem_stmt *)
let update_var (id : ide) exp_value operation p_state =
  let curr_sigma = state_get_sigma p_state in
        let value = sem_int exp_value curr_sigma in
        let new_sigma = operation curr_sigma id value in
        state_set_sigma new_sigma p_state;;


let assign_var e1 e2 p_state =
  match e1 with
    | Val (i) ->
        let new_state = state_push_ide i p_state in
        update_var i e2 dassign new_state
    | _ -> raise (Assignment_to_non_var "Assign");;

let assign_var_rev e1 p_state =
  match e1 with
    | Val (i) ->
        let value = state_top_ide i p_state in
        let new_state = state_pop_ide i p_state in
        state_dassign i value new_state

    | _ -> raise (Assignment_to_non_var "Assign (reverse execution)");;

let inc_var e1 e2 p_state =
  match e1 with
  | Val (i) -> update_var i e2 increment p_state
  | _ -> raise (Assignment_to_non_var "Cadd, or Csub if execution was reversed");;

let dec_var e1 e2 p_state =
  match e1 with
  | Val (i) -> update_var i e2 decrement p_state
  | _ -> raise (Assignment_to_non_var "Csub, or Cadd if execution was reversed");;



let if_eval_fwd b prg1 prg2 p_state =
  let b_eval = sem_bool b (state_get_sigma p_state) in
  let prg = state_get_program p_state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval if_prg_block prg  in
  state_set_program aug_if_prg_block p_state;;

let if_eval_rev prg1 prg2 p_state =
  let b_eval = state_top_if p_state in
  let new_state = state_pop_if p_state |> state_prev_stmt in
  let prg = state_get_program new_state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval (prg_last_stmt if_prg_block) prg |> prg_last_stmt in
  state_set_program aug_if_prg_block new_state;;


let while_eval b_expr b_eval while_prg p_state next_prg_when_b_false =
  let prg = state_get_program p_state in
  
  let prg_to_execute =
    if b_eval then
    aug_while_block b_expr while_prg prg
  else
    next_prg_when_b_false

  in
  state_set_program prg_to_execute p_state;;

let while_eval_fwd b_expr while_prg p_state =
  let b_eval = sem_bool b_expr (state_get_sigma p_state) in
  let new_state = state_push_while false p_state in
  let next_prg_when_b_false = state_get_program new_state |> prg_next_stmt in
  while_eval b_expr b_eval while_prg new_state next_prg_when_b_false;;

let while_eval_rev b_expr while_prg p_state =
  let b_eval = state_top_while p_state in
  let new_state = state_pop_while p_state |> state_prev_stmt in
  
  let next_prg_when_b_false = state_get_program new_state |> prg_prev_stmt in
  while_eval b_expr b_eval while_prg new_state next_prg_when_b_false |> state_last_stmt;;

let while_start_eval while_stmt_in_outer_prg p_state =
  let b_eval = state_top_while p_state in
  let new_state = state_pop_while p_state in
  let prg_to_execute =
    if b_eval then
      state_get_program new_state |> prg_last_stmt
    else
      while_stmt_in_outer_prg
  in
  state_set_program prg_to_execute new_state;;  

let while_end_eval b_expr while_next_stmt_in_outer_prg p_state =
  let b_eval = sem_bool b_expr (state_get_sigma p_state) in
  let new_state = state_push_while true p_state in
  let prg_to_execute =
    if b_eval then
      state_get_program new_state |> prg_while_block_1st_stmt
    else
      while_next_stmt_in_outer_prg
  in
  state_set_program prg_to_execute new_state;; 

let sem_stmt_fwd (p_state : prg_state) : prg_state =
  let expr = state_get_curr_stmt p_state in
  match expr with
    | Program_end -> p_state
    | Skip -> state_next_stmt p_state
    | Assign (e1, e2) -> state_next_stmt (assign_var e1 e2 p_state)
    | Cadd (e1, e2) -> state_next_stmt (inc_var e1 e2 p_state)
    | Csub (e1, e2) -> state_next_stmt (dec_var e1 e2 p_state)
    | Ifthenelse (b_expr, prg1, prg2) -> if_eval_fwd b_expr prg1 prg2 p_state
    | If_end (prg, b_eval) -> state_set_program prg (state_push_if b_eval p_state)
    | While (b_expr, prg) -> while_eval_fwd b_expr prg p_state
    | While_end (b_expr, while_in_outer_prg) -> while_end_eval b_expr while_in_outer_prg p_state

    (* The following statement expressions aren't supposed to be encountered during forward execution *)
    | Program_start -> raise (Illegal_statement_fwd_execution "Program_start")
    | If_start _ -> raise (Illegal_statement_fwd_execution "If_start")
    | While_start _ -> raise (Illegal_statement_fwd_execution "While_start");;
  

let sem_stmt_rev (p_state : prg_state) : prg_state =
  let expr = state_get_prev_stmt p_state in
  match expr with
    | Program_start -> p_state
    | Skip -> state_prev_stmt p_state
    | Assign (e1, _) -> state_prev_stmt (assign_var_rev e1 p_state)
    | Cadd (e1, e2) -> state_prev_stmt (dec_var e1 e2 p_state)
    | Csub (e1, e2) -> state_prev_stmt (inc_var e1 e2 p_state)
    | Ifthenelse (_, prg1, prg2) -> if_eval_rev prg1 prg2 p_state
    | If_start (prg_prev) -> state_set_program prg_prev p_state
    | While (b_expr, prg) -> while_eval_rev b_expr prg p_state
    | While_start (while_in_outer_prg) -> while_start_eval while_in_outer_prg p_state

    (* The following statement expressions aren't supposed to be encountered during reverse execution *)
    | Program_end -> raise (Illegal_statement_rev_execution "Program_end")
    | If_end _ -> raise (Illegal_statement_rev_execution "If_end")
    | While_end (_, _) -> raise (Illegal_statement_rev_execution "While_end");;


let rec sem_prg_fwd p_state =
  if not (state_is_prg_at_end p_state) then
    sem_prg_fwd (sem_stmt_fwd p_state)
  else
    p_state;;

let rec sem_prg_rev p_state =
  if not (state_is_prg_at_start p_state) then
    sem_prg_rev (sem_stmt_rev p_state)
  else
    p_state;;


let rec sem_prg_fwd_steps p_state num_steps =
  if not (state_is_prg_at_end p_state) && num_steps > 0 then
    sem_prg_fwd_steps (sem_stmt_fwd p_state) (num_steps - 1)
  else
    p_state;;

let rec sem_prg_rev_steps p_state num_steps =
  if not (state_is_prg_at_start p_state) && num_steps > 0 then
    sem_prg_rev_steps (sem_stmt_rev p_state) (num_steps -1)
  else
    p_state;;

let sem_prg_steps p_state num_steps =
  if num_steps >= 0 then
    sem_prg_fwd_steps p_state num_steps
  else
    sem_prg_rev_steps p_state (-num_steps);;

  