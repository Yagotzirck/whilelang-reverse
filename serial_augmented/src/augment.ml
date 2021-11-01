(* A set of auxiliary functions to augment the original abstract syntax tree.
**
** More specifically:
** program -> program_aug (it now acts as a doubly-linked list (zipper), in order to allow
**     forwards as well as backwards execution);
**
** stmt -> stmt_aug : Ifthenelse and While program blocks need to be augmented as well
*)

open Ast
open Ast_aug
open State

exception Empty_if_while_block;;
exception Empty_prg_block;;

let aug_if_block if_bool_value if_prg prg  =
  prg_prepend_stmt (If_start prg) (prg_append_stmt (If_end ((prg_next_stmt prg), if_bool_value)) if_prg);;
  
let aug_while_block while_bool_expr while_prg while_in_outer_prg =
  let stmt_to_prepend = While_start (while_in_outer_prg) in
  let stmt_to_append = While_end (while_bool_expr, while_in_outer_prg |> prg_next_stmt) in
  prg_prepend_stmt stmt_to_prepend (prg_append_stmt stmt_to_append while_prg);;


let rec aug_if_while_in_list (prg : program) : program_aug = 
  let rec create_list = function
    | Program_empty -> []
    | Program(stmt, prg_n) -> (aug_stmt stmt) :: create_list prg_n
  in
  let create_prg = function
  | [] -> raise Empty_if_while_block
  | h :: t -> Program_aug ([], h, t)
  in
  create_list prg |> create_prg

and

aug_stmt (s : stmt) : stmt_aug = 
  match s with
    | Skip -> Skip
    | Assign (e1, e2) -> Assign (e1, e2)
    | Cadd (e1, e2) -> Cadd (e1, e2)
    | Csub (e1, e2) -> Csub (e1, e2)

    | Ifthenelse (b, prg1, prg2) -> Ifthenelse (b, aug_if_while_in_list prg1, aug_if_while_in_list prg2)
    | While (b, prg) -> While (b, aug_if_while_in_list prg)

and

aug_prg (prg: program) : program_aug =
  let rec create_list = function
    | Program_empty -> Program_end :: []
    | Program(stmt, prg_n) -> (aug_stmt stmt) :: create_list prg_n
  in
  let create_prg = function
  | [] -> raise Empty_prg_block
  | h :: t -> Program_aug (Program_start :: [], h, t)
  in
  create_list prg |> create_prg;;