(* Interpreter for the serial version of the while language. *)

open Ast
open State

(* Exceptions' definitions *)
exception Assignment_to_non_var of string;;







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
  let curr_sigma = get_sigma p_state in
        let value = sem_int exp_value curr_sigma in
        let new_sigma = operation curr_sigma id value in
        set_sigma new_sigma p_state;;


let assign_var e1 e2 p_state =
  match e1 with
    | Val (i) -> update_var i e2 dassign p_state
    | _ -> raise (Assignment_to_non_var "Assign");;

let inc_var e1 e2 p_state =
  match e1 with
  | Val (i) -> update_var i e2 increment p_state
  | _ -> raise (Assignment_to_non_var "Cadd");;

let dec_var e1 e2 p_state =
  match e1 with
  | Val (i) -> update_var i e2 decrement p_state
  | _ -> raise (Assignment_to_non_var "Csub");;

let rec if_eval b prg1 prg2 p_state =
  if sem_bool b (get_sigma p_state) then
    sem_prg prg1 p_state
  else
    sem_prg prg2 p_state

and

while_eval b prg p_state =
    if sem_bool b (get_sigma p_state) then
      while_eval b prg (sem_prg prg p_state)
    else
      p_state
and

sem_stmt (expr : stmt) (p_state : prg_state) : prg_state =
  match expr with
    | Skip -> p_state
    | Assign (e1, e2) -> assign_var e1 e2 p_state
    | Cadd (e1, e2) -> inc_var e1 e2 p_state
    | Csub (e1, e2) -> dec_var e1 e2 p_state
    | Ifthenelse (b, prg1, prg2) -> if_eval b prg1 prg2 p_state
    | While (b, prg) -> while_eval b prg p_state

and

sem_prg (prg : program) (p_state : prg_state) : prg_state =
  match prg with
    | Program_empty -> p_state
    | Program (expr, prg_tail) -> sem_prg prg_tail (sem_stmt expr p_state);;