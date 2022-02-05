(** Abstract syntax tree for the annotated parallel version of the while language. *)

open Ast

(** In the annotated version we must allow forwards as well as backwards execution,
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
type program_ann =
  | Program_ann of stmt_ann list * stmt_ann * stmt_ann list


(** Statements must be annotated as well with the following additions:
    + [Program_start] and [Program_end], in order to set boundaries for the whole program;
    + [Par_prg_start] and [Par_prg_end], in order to set boundaries for programs contained inside the [Par] statements;
    + An [int list] for each statement (except for boundary-tracking statements), in order to keep track of the statements' execution order;
    + An [int] value for the [Par] statement, used to keep track of how many child threads containing the two programs are done with their execution (values' range: 0-2).

    As for 3), a scalar would probably have sufficed instead of a list since we aren't using any loops in the parallel version
    (and therefore no statement can be executed more than once), however we're using a list anyway in case we decide to add
    loops as well later on.
*)
and stmt_ann =
  | Program_start
  | Program_end
  | Par_prg_start
  | Par_prg_end
  | Skip of int list
  | Assign of int_expr * int_expr * int list (** Destructive assignment; 1st [int_expr] must be {!constructor:Ast.int_expr.Val} *)

  (* Cop (constructive assignments on variables: "+=" and "-=") *)
  | Cadd of int_expr * int_expr * int list (** Constructive assignment ("+="); must check about 1st [int_expr] being {!constructor:Ast.int_expr.Val} *)
  | Csub of int_expr * int_expr * int list (** Constructive assignment ("-="); must check about 1st [int_expr] being {!constructor:Ast.int_expr.Val} *)

  | Par of program_ann * program_ann * int * int list;;
;;