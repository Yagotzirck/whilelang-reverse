(** Abstract syntax tree for the parallel version of the while language. *)

type ide = string;;

type int_expr =
  | Val of ide
  | Eint of int

  (* Op (arithmetic operators "+" and "-" used inside integer expressions) *)
  | Add of int_expr * int_expr
  | Sub of int_expr * int_expr;;


type program =
  | Program_empty
  | Program of stmt * program

and

stmt =
  | Skip
  | Assign of int_expr * int_expr (** Destructive assignment ("="); 1st int_expr must be a Val *)

  (* Cop (constructive assignments on variables: "+=" and "-=") *)
  | Cadd of int_expr * int_expr (** Constructive assignment ("+="); must check about 1st int_expr being a Val *)
  | Csub of int_expr * int_expr (** Constructive assignment ("-="); must check about 1st int_expr being a Val *)

  | Par of program * program (** Parallel execution of two programs *)
;;