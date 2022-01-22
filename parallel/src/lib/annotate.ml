(** A set of auxiliary functions to annotate the original abstract syntax tree. *)

open Ast
open Ast_ann

(** Raised whenever a [Par] statement contains an empty program. *)
exception Empty_par_prg_block;;

(** Raised if the program to be annotated is empty. *)
exception Empty_prg_block;;

(** Annotates a program inside a [Par] block.
  This is an auxiliary function for {!val:ann_stmt}, so refer to its description for details.
*)
let rec ann_prg_in_par (prg : program) : program_ann = 
  let rec create_list = function
    | Program_empty -> []
    | Program(stmt, prg_next) -> (aug_stmt stmt) :: create_list prg_next
  in
  let create_prg = function
  | [] -> raise Empty_par_prg_block
  | h :: t -> Program_ann (Par_prg_start :: [], h, t @ Par_prg_end)
  in
  create_list prg |> create_prg


(** Annotates a statement.

    An int list (which will keep track of statements' execution order) is added to each statement
    (except for the [Par] statement); as for the [Par] statement, the statements contained in its two
    program blocks will be recursively annotated as well, and [Par_prg_start] - [Par_prg_end] statements
    are added at the beginning and at the end of the program respectively,
    in order to define the programs' boundaries.

@param s The statement to be annotated.
@return The annotated statement.
@raise  Empty_par_prg_block in case a program contained inside a [Par] block is empty.
*)
and ann_stmt ~s:(s : stmt) : stmt_ann = 
  match s with
    | Skip -> Skip []
    | Assign (e1, e2) -> Assign (e1, e2, [])
    | Cadd (e1, e2) -> Cadd (e1, e2, [])
    | Csub (e1, e2) -> Csub (e1, e2, [])
    | Par (prg1, prg2) -> Par (ann_prg_in_par prg1, ann_prg_in_par prg2)

(** Annotates a program (by representing it as a zipper structure, among the other things) and its statements.

    Also, puts [Program_start] statement at the beginning and [Program_end] at the end of the program,
    in order to set the whole program's boundaries.
@param prg The program to be annotated.
@return The annotated program. 
@raise Empty_prg_block when the program to annotate ([prg]) is empty.
*)
and ann_prg ~prg:(prg: program) : program_ann =
  let rec create_list = function
    | Program_empty -> []
    | Program(stmt, prg_next) -> (aug_stmt stmt) :: create_list prg_next
  in
  let create_aug_prg = function
  | [] -> raise Empty_prg_block
  | h :: t -> Program_aug (Program_start :: [], h, t @ [Program_end])
  in
  create_list prg |> create_aug_prg;;