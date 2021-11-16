(** Augmented interpreter for the serial version of the while language. *)


open Ast
open Ast_aug
open Augment

(* Exceptions' definitions *)
exception Assignment_to_non_var of string;;
exception Illegal_statement_fwd_execution of string;;
exception Illegal_statement_rev_execution of string;;




let rec sem_int (expr : int_expr) (s : Sigma.sigma) : int =
  match expr with
    | Val (i) -> Sigma.get_value s i
    | Eint (x) -> x
    | Add (e1, e2) -> (sem_int e1 s) + (sem_int e2 s)
    | Sub (e1, e2) -> (sem_int e1 s) - (sem_int e2 s);;

let rec sem_bool (expr : bool_expr) (s : Sigma.sigma) : bool =
  match expr with
    | Ebool (b) -> b
    | Not (b) -> not (sem_bool b s)
    | Eq (e1, e2) -> (sem_int e1 s) = (sem_int e2 s)
    | Gt (e1, e2) -> (sem_int e1 s) > (sem_int e2 s)
    | And (b1, b2) -> (sem_bool b1 s) && (sem_bool b2 s);;


(* Auxiliary functions for sem_stmt and sem_stmt_rev *)

let assign_var_fwd e1 e2 state =
  match e1 with
    | Val (i) ->
        let curr_sigma = State.get_sigma state in
        let value = sem_int e2 curr_sigma in
        State.dassign_fwd i value state
        
    | _ -> raise (Assignment_to_non_var "Assign");;

let assign_var_rev e1 state =
  match e1 with
    | Val (i) -> State.dassign_rev i state

    | _ -> raise (Assignment_to_non_var "Assign (reverse execution)");;

let cadd e1 e2 state =
  match e1 with
  | Val (i) ->
      let curr_sigma = State.get_sigma state in
      let value = sem_int e2 curr_sigma in
      State.cadd i value state 
    
  | _ -> raise (Assignment_to_non_var "Cadd, or Csub if execution was reversed");;

let csub e1 e2 state =
  match e1 with
  | Val (i) ->
        let curr_sigma = State.get_sigma state in
        let value = sem_int e2 curr_sigma in
        State.csub i value state

  | _ -> raise (Assignment_to_non_var "Csub, or Cadd if execution was reversed");;



let if_eval_fwd b prg1 prg2 state =
  let b_eval = sem_bool b (State.get_sigma state) in
  let prg = State.get_program state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval if_prg_block prg  in
  State.set_program aug_if_prg_block state;;

let if_eval_rev prg1 prg2 state =
  let b_eval = State.top_if state in
  let new_state = State.pop_if state |> State.prev_stmt in
  let prg = State.get_program new_state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval (Program.last_stmt if_prg_block) prg |> Program.last_stmt in
  State.set_program aug_if_prg_block new_state;;


let while_eval b_expr b_eval while_prg state next_prg_when_b_false =
  let prg = State.get_program state in
  
  let prg_to_execute =
    if b_eval then
    aug_while_block b_expr while_prg prg
  else
    next_prg_when_b_false

  in
  State.set_program prg_to_execute state;;

let while_eval_fwd b_expr while_prg state =
  let b_eval = sem_bool b_expr (State.get_sigma state) in
  let new_state = State.push_while false state in
  let next_prg_when_b_false = State.get_program new_state |> Program.next_stmt in
  while_eval b_expr b_eval while_prg new_state next_prg_when_b_false;;

let while_eval_rev b_expr while_prg state =
  let b_eval = State.top_while state in
  let new_state = State.pop_while state |> State.prev_stmt in
  
  let next_prg_when_b_false = State.get_program new_state |> Program.prev_stmt in
  while_eval b_expr b_eval while_prg new_state next_prg_when_b_false |> State.last_stmt;;

let while_start_eval while_stmt_in_outer_prg state =
  let b_eval = State.top_while state in
  let new_state = State.pop_while state in
  let prg_to_execute =
    if b_eval then
      State.get_program new_state |> Program.last_stmt
    else
      while_stmt_in_outer_prg
  in
  State.set_program prg_to_execute new_state;;  

let while_end_eval b_expr while_next_stmt_in_outer_prg state =
  let b_eval = sem_bool b_expr (State.get_sigma state) in
  let new_state = State.push_while true state in
  let prg_to_execute =
    if b_eval then
      State.get_program new_state |> Program.while_block_1st_stmt
    else
      while_next_stmt_in_outer_prg
  in
  State.set_program prg_to_execute new_state;; 

let sem_stmt_fwd (curr_state : State.state) : State.state =
  let expr = State.get_curr_stmt curr_state in
  match expr with
    | Program_end -> curr_state
    | Skip -> State.next_stmt curr_state
    | Assign (e1, e2) -> State.next_stmt (assign_var_fwd e1 e2 curr_state)
    | Cadd (e1, e2) -> State.next_stmt (cadd e1 e2 curr_state)
    | Csub (e1, e2) -> State.next_stmt (csub e1 e2 curr_state)
    | Ifthenelse (b_expr, prg1, prg2) -> if_eval_fwd b_expr prg1 prg2 curr_state
    | If_end (prg, b_eval) -> State.set_program prg (State.push_if b_eval curr_state)
    | While (b_expr, prg) -> while_eval_fwd b_expr prg curr_state
    | While_end (b_expr, while_in_outer_prg) -> while_end_eval b_expr while_in_outer_prg curr_state

    (* The following statement expressions aren't supposed to be encountered during forward execution *)
    | Program_start -> raise (Illegal_statement_fwd_execution "Program_start")
    | If_start _ -> raise (Illegal_statement_fwd_execution "If_start")
    | While_start _ -> raise (Illegal_statement_fwd_execution "While_start");;
  

let sem_stmt_rev (curr_state : State.state) : State.state =
  let expr = State.get_prev_stmt curr_state in
  match expr with
    | Program_start -> curr_state
    | Skip -> State.prev_stmt curr_state
    | Assign (e1, _) -> State.prev_stmt (assign_var_rev e1 curr_state)
    | Cadd (e1, e2) -> State.prev_stmt (csub e1 e2 curr_state)
    | Csub (e1, e2) -> State.prev_stmt (cadd e1 e2 curr_state)
    | Ifthenelse (_, prg1, prg2) -> if_eval_rev prg1 prg2 curr_state
    | If_start (prg_prev) -> State.set_program prg_prev curr_state
    | While (b_expr, prg) -> while_eval_rev b_expr prg curr_state
    | While_start (while_in_outer_prg) -> while_start_eval while_in_outer_prg curr_state

    (* The following statement expressions aren't supposed to be encountered during reverse execution *)
    | Program_end -> raise (Illegal_statement_rev_execution "Program_end")
    | If_end _ -> raise (Illegal_statement_rev_execution "If_end")
    | While_end (_, _) -> raise (Illegal_statement_rev_execution "While_end");;


let rec sem_prg_fwd curr_state =
  if not (State.is_prg_at_end curr_state) then
    sem_prg_fwd (sem_stmt_fwd curr_state)
  else
    curr_state;;

let rec sem_prg_rev curr_state =
  if not (State.is_prg_at_start curr_state) then
    sem_prg_rev (sem_stmt_rev curr_state)
  else
    curr_state;;


let rec sem_prg_fwd_steps curr_state num_steps =
  if not (State.is_prg_at_end curr_state) && num_steps > 0 then
    sem_prg_fwd_steps (sem_stmt_fwd curr_state) (num_steps - 1)
  else
    curr_state;;

let rec sem_prg_rev_steps curr_state num_steps =
  if not (State.is_prg_at_start curr_state) && num_steps > 0 then
    sem_prg_rev_steps (sem_stmt_rev curr_state) (num_steps -1)
  else
    curr_state;;

let sem_prg_steps curr_state num_steps =
  if num_steps >= 0 then
    sem_prg_fwd_steps curr_state num_steps
  else
    sem_prg_rev_steps curr_state (-num_steps);;