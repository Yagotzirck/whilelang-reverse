(** Abstract syntax tree for the augmented serial version of the while language. *)

open Ast

(** In the augmented version we must allow forwards as well as backwards execution,
    therefore we must implement a doubly-linked list (or rather, its functional
    equivalent: a zipper) in order to allow movement in both directions.

    In order to explain how a zipper works, let's start from this simple integer list:

    [[1; 2; 3; 4; 5; 6]]

    Its zipper representation would consist in a tuple containing:
      - A list containing the items to the left of the current element ([prev_items]);
      - The current item ([curr_item]);
      - A list containing the elements to the right of the current element ([next_items]).

    Therefore, the previous list converted to a zipper would be:

    [( [], 1, [2; 3; 4; 5; 6] )]

    A [next_item] operation
      - moves [curr_item] to the head of [prev_items] list;
      - moves the head of [next_items] to [curr_item].

    Applying [next_item] to the previous zipper yields

    [( 1 :: [], 2, [3; 4; 5; 6] )]

    If we apply [next_item] two more times, we get

    [( [3; 2; 1]), 4, [5; 6] )]

    A [prev_item] operation
      - moves [curr_item] to the head of [next_items] list;
      - moves the head of [prev_items] to [curr_item].

    Applying [prev_item] to the last zipper we obtained above yields

    [( [2; 1]), 3, [4; 5; 6] )]

    It should be clear at this point that the zipper is the ideal structure that achieves a
    doubly linked list-like behavior using the functional programming paradigm, allowing us to
    move in both directions starting from a given list of items (in our case, a list of
    statements/instructions).
*)
type program_aug =
  | Program_aug of stmt_aug list * stmt_aug * stmt_aug list

(** Statements must be augmented as well with the following additions:
    - [Program_start] and [Program_end], in order to set boundaries for the whole program;
    - [If_start] and [If_end], in order to set boundaries for programs inside the "then" and "else" branches;
    - [While_start] and [While_end], in order to set boundaries for programs constituting While blocks.
*)
and stmt_aug =
  | Program_start
  | Program_end
  | Skip
  | Assign of int_expr * int_expr (** Destructive assignment; 1st [int_expr] must be {!constructor:Ast.int_expr.Val} *)

  (* Cop (constructive assignments on variables: "+=" and "-=") *)
  | Cadd of int_expr * int_expr (** Constructive assignment ("+="); must check about 1st [int_expr] being {!constructor:Ast.int_expr.Val} *)
  | Csub of int_expr * int_expr (** Constructive assignment ("-="); must check about 1st [int_expr] being {!constructor:Ast.int_expr.Val} *)

  | Ifthenelse of bool_expr * program_aug * program_aug

  | If_start of program_aug     (** Parameter: [prg_prev]; that is, the program having the outer [If] containing this program block as the current statement *)

  (* Parameters: [(prg_next, if_b_eval)], where:
        - prg_next is the program having the statement that follows the outer While containing this program block as the current statement;
        - if_b_eval is the bool value resulting from the evaluation of the containing Ifthenelse's bool expression.
  *)
  | If_end of program_aug * bool

  | While of bool_expr * program_aug        (** Parameters: [(prg_next, if_b_eval)] *)
  | While_start of program_aug              (** Parameter: [while_in_outer_prg] *)
  | While_end of bool_expr * program_aug    (** Parameters: [(bool_expr, while_in_outer_prg)] *)
;;


  