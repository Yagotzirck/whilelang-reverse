(** A set of auxiliary functions to augment the original abstract syntax tree. *)

open Ast
open Ast_aug

(** Raised whenever a [While] or an [Ifthenelse] statement contains an empty program. *)
exception Empty_if_while_block;;

(** Raised if the program to be augmented is empty. *)
exception Empty_prg_block;;


(** Augment a program contained inside an [Ifthenelse] statement.

    This is called at runtime whenever a [Ifthenelse] statement is encountered.

    An [If_start] statement containing the program where [Ifthenelse] was encountered and having
    [Ifthenelse] statement as current instruction is placed as the
    first statement of the augmented program inside the [Ifthenelse] block: this allows execution to
    identify the beginning of the [Ifthenelse] program block, thus allowing jumping back to the program
    containing the [Ifthenelse] statement during reverse execution.
    
    An [If_end] statement containing
      - the outside program having as the current statement the statement following
        the [Ifthenelse] statement containing this block;
      - The boolean evaluation of the containing [Ifthenelse] statement
      
    is placed as the last statement of the program inside the [Ifthenelse] block: this serves
    the same goal as the [If_start] statement, in a specular way.

    [If_end] contains [Ifthenelse]'s boolean evaluation because the boolean value must be pushed
    on delta's [If_stack] when exiting the [Ifthenelse] program block, in order to determine whether
    the [then] or the [else] branch was taken during reverse execution; the value is pushed 
    at the end of [Ifthenelse]'s program execution rather than at the beginning, in order to allow
    nested [Ifthenelse]s to take place while pushing boolean evaluations in the correct order.
@param  if_bool_value The boolean evaluation in [Ifthenelse] statement, which establishes whether the
        [then] or the [else] branch has been taken.
@param  if_prg The program contained inside the [Ifthenelse] statement to be augmented.
@param  prg The program containing the [Ifthenelse] statement under examination, which gets put inside
        [If_start] (as-is, having [Ifthenelse] as the current statement) and [If_end] (setting as the 
        current statement the statement following [Ifthenelse])].
@return The augmented [Ifthenelse] program block. *)
let aug_if_block ~if_bool_value ~if_prg ~prg  =
  Program.prepend_stmt (If_start prg) (Program.append_stmt (If_end ((Program.next_stmt prg), if_bool_value)) if_prg);;

(** Augment a program contained inside a [While] block.

    This is called at runtime whenever a [While] statement is encountered.

    A [While_start] statement containing the [While] container-program having the
    [While] statement as current instruction is placed as the
    first statement of the augmented program inside the [While] block: this allows execution to
    identify the beginning of the [While] program block, thus allowing
      - Forward iterating from the beginning of the program block;
      - Reverse iterating, starting back from While_end when [While_start] is encountered;
      - Jumping back to the program containing the [While] statement during reverse execution,
        when there are no more iterations to be executed.
    
    A [While_end] statement containing
      - The boolean expression of the containing [While] statement;
      - the outside program having as the current statement the statement coming after
        the [While] statement containing this block
    
    is placed as the last statement of the program inside the [While] block: this serves
    the same goal as the [While_start] statement, in a specular way.

    [While_end] contains [While]'s boolean expression because it must be re-evaluated at
    the end of every iteration during forward execution to decide whether to continue
    iterating or not; on the other hand, [While_start] doesn't need it, because
    reverse execution pops boolean elements from delta's [If_stack] to determine
    how many iterations must be performed.
@param  while_bool_expr The boolean expression in [While] statement, to decide whether we should
        continue iterating or not.
@param  while_prg The program contained inside the [While] statement to be augmented.
@param  while_in_outer_prg The program containing the [While] statement under examination,
        which gets put inside [While_start] (as-is, having [While] as the current statement)
        and [While_end] (setting as the current statement the statement following [While])].
@return The augmented [While] program block. *)
let aug_while_block ~while_bool_expr ~while_prg ~while_in_outer_prg =
  let stmt_to_prepend = While_start (while_in_outer_prg) in
  let stmt_to_append = While_end (while_bool_expr, while_in_outer_prg |> Program.next_stmt) in
  Program.prepend_stmt stmt_to_prepend (Program.append_stmt stmt_to_append while_prg);;


(** Augment a program inside an [Ifthenelse] or [While] block.
  This is an auxiliary function for {!val:aug_stmt}, so refer to its description for details.
*)
let rec aug_if_while_in_list (prg : program) : program_aug = 
  let rec create_list = function
    | Program_empty -> []
    | Program(stmt, prg_next) -> (aug_stmt stmt) :: create_list prg_next
  in
  let create_prg = function
  | [] -> raise Empty_if_while_block
  | h :: t -> Program_aug ([], h, t)
  in
  create_list prg |> create_prg



(** Augment a statement.

    All statements are unaffected, except for [Ifthenelse] and [While] which contain program blocks
    which must be recursively augmented as well.

    Unlike the main program, [If_start]/[If_end] and [While_start]/[While_end] boundaries aren't put
    at this stage; instead, they get added at runtime, since they all contain the containing augmented
    program as a parameter (which hasn't been fully constructed yet at this stage).
@param s The statement to be augmented.
@return The augmented statement.
@raise  Empty_if_while_block in case a program contained inside a [While] block or inside either of
        an [Ifthenelse] {b then} or an {b else} branch block is empty. *)
and aug_stmt ~s:(s : stmt) : stmt_aug = 
  match s with
    | Skip -> Skip
    | Assign (e1, e2) -> Assign (e1, e2)
    | Cadd (e1, e2) -> Cadd (e1, e2)
    | Csub (e1, e2) -> Csub (e1, e2)

    | Ifthenelse (b, prg1, prg2) -> Ifthenelse (b, aug_if_while_in_list prg1, aug_if_while_in_list prg2)
    | While (b, prg) -> While (b, aug_if_while_in_list prg)


(** Augment a program (by representing it as a zipper structure, among the other things)) and its statements.

    Also, put [Program_start] instatement at the beginning and [Program_end] at the end of the list,
    in order to set the whole program's boundaries.
@param prg The program to be augmented.
@return The augmented program. *)
and aug_prg ~prg:(prg: program) : program_aug =
  let rec create_list = function
    | Program_empty -> []
    | Program(stmt, prg_next) -> (aug_stmt stmt) :: create_list prg_next
  in
  let create_aug_prg = function
  | [] -> raise Empty_prg_block
  | h :: t -> Program_aug (Program_start :: [], h, t @ [Program_end])
  in
  create_list prg |> create_aug_prg;;