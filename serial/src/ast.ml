(* Abstract syntax tree for the serial version of the while language. *)

type ide = string;;

type int_expr =
  | Val of ide
  | Eint of int

  (* Op (arithmetic operators "+" and "-" used inside integer expressions) *)
  | Add of int_expr * int_expr
  | Sub of int_expr * int_expr;;

type bool_expr =
  | Ebool of bool
  | Not of bool_expr
  | Eq of int_expr * int_expr
  | Gt of int_expr * int_expr
  | And of bool_expr * bool_expr;;


type program = Program of stmt list (* Should add state and enhanced state as well *)

and

stmt =
  | Skip
  | Assign of int_expr * int_expr (* Destructive assignment; 1st int_expr must be a Val *)

  (* Cop (constructive assignments on variables: "+=" and "-=") *)
  | Cadd of int_expr * int_expr (* Must check about 1st int_expr being a Val *)
  | Csub of int_expr * int_expr (* Same as above *)

  | Ifthenelse of bool_expr * program * program
  | While of bool_expr * program;;

  