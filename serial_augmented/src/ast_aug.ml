(* Abstract syntax tree for the augmented serial version of the while language. *)

open Ast

(* In the augmented version we must allow forwards as well as backwards execution,
** therefore we must implement a doubly-linked list (or rather, its functional
** equivalent: a zipper) in order to allow movement in both directions.
*)
type program_aug =
  | Program_aug of stmt_aug list * stmt_aug * stmt_aug list

and

(* Statements must be augmented as well with the following additions:
**
** - Program_start and Program_end, in order to set boundaries for the whole program;
**
** - If_start and If_end, in order to set boundaries for programs inside the "then" and "else" branches.
** 
** - While_start and While_end, in order to set boundaries for programs constituting While blocks.
*)
stmt_aug =
  | Program_start
  | Program_end
  | Skip
  | Assign of int_expr * int_expr (* Destructive assignment; 1st int_expr must be a Val *)

  (* Cop (constructive assignments on variables: "+=" and "-=") *)
  | Cadd of int_expr * int_expr (* Must check about 1st int_expr being a Val *)
  | Csub of int_expr * int_expr (* Same as above *)

  | Ifthenelse of bool_expr * program_aug * program_aug

  (* If_start: (prg_prev), where prg_prev is the program having the outer If containing this program block as the current statement *)
  | If_start of program_aug     

  (* If_end: (prg_next, if_b_eval), where:
  **  
  **  - prg_next is the program having the statement that follows the outer While containing this program block as the current statement;
  **  
  **  - if_b_eval is the bool value resulting from the evaluation of the containing Ifthenelse's bool expression.
  *)
  | If_end of program_aug * bool

  | While of bool_expr * program_aug
  | While_start of program_aug              (* (while_in_outer_prg *)
  | While_end of bool_expr * program_aug;;  (* (bool_expr, while_in_outer_prg) *)


  