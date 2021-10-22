open Ast;;


(* Exceptions' definitions *)
exception Unbound_variable of string;;
let exc_create_unbound_var var_name = Unbound_variable ("Variable \"" ^ var_name ^ "\" not found in the program store");;

exception Delta_stack_error of string;;
let exc_ide_stack_not_found ide_name = Delta_stack_error ("Auxiliary stack for variable \"" ^ ide_name ^ "\" doesn't exist");;
let exc_ide_stack_empty ide_name = Delta_stack_error ("Auxiliary stack for variable \"" ^ ide_name ^ "\" is empty");;
let exc_if_stack_empty = Delta_stack_error "if_stack is empty";;
let exc_while_stack_empty = Delta_stack_error "while_stack is empty";;


(**************************************************************************************************************************)

(* sigma is a type representing the store (that is, the set of 
** all identifiers(variables) and the respective integer values associated to them).
**
*)
type sigma =
  | Sigma_empty
  | Sigma of (ide * int) * sigma;;

(* dassign: destructive assignment *)
let dassign (s : sigma) (ide_name : ide) (value : int) =
  let rec assign_inner = function
    | Sigma_empty -> Sigma ((ide_name, value), Sigma_empty)
    | Sigma((i, _), next) when i = ide_name -> Sigma((i, value), next)
    | Sigma (kv, next) -> Sigma(kv, assign_inner next)
  in
  assign_inner s;;

(* cassign: constructive assignments ( "+=" and "-=") *)
let cassign (s : sigma) (ide_name : ide) (operation : int -> int) =
  let rec cassign_inner = function
    | Sigma_empty -> raise (exc_create_unbound_var ide_name)
    | Sigma((i, v), next) when i = ide_name -> Sigma((i, operation v), next)
    | Sigma (kv, next) -> Sigma(kv, cassign_inner next)
  in
  cassign_inner s;;

  let increment s ide_name value =
    cassign s ide_name ( (+) value);;

  let decrement s ide_name value =
    cassign s ide_name (fun x -> x - value);;

  (* get_value: returns the integer value associated to the
  ** identifier passed as parameter.
  *)
let rec get_value s ide_name =
  match s with
    | Sigma_empty -> raise (exc_create_unbound_var ide_name)
    | Sigma((i, v), _) when i = ide_name -> v
    | Sigma(_, next) -> get_value next ide_name;;

let empty_sigma = Sigma_empty;;

(**************************************************************************************************************************)

(* delta is a type representing the auxiliary store, consisting of:
** 1) A list of stacks "ide_stacks", where each stack is contained inside a pair <identifier>-<integer list> that keeps track
**    of each variable's value: every time a destructive assignment is performed on a certain <ide> variable, its previous
**    value is pushed onto the stack associated to that <ide> variable, in order to preserve it;
**
** 2) A stack "if_stack", which keeps track of all the boolean values used as tests inside conditional statements
**    (that is, if-then-else statements);
**
** 3) A stack "while_stack", whick keeps track of all the boolean values used as tests inside "while" loops.
*)


(***************** ide_stacks *****************)

type ide_stacks = Ide_stacks of (ide * int list) list;;

let private_push_ide (ide_stk : ide_stacks) (ide_name : ide) (value : int) : ide_stacks =
  let rec push = function
    | [] -> [ (ide_name, [value]) ]
    | (ide, l) :: t when ide = ide_name -> (ide, value :: l) :: t
    | h :: t -> h :: push t
  in
  match ide_stk with
    | Ide_stacks(ide_stk) -> Ide_stacks (push ide_stk);;

let private_top_ide (ide_stk : ide_stacks) (ide_name : ide) : int =
  let rec top = function
    | [] -> raise (exc_ide_stack_not_found ide_name)
    | (ide, []) :: _ when ide = ide_name -> raise (exc_ide_stack_empty ide_name)
    | (ide, h::_) :: _ when ide = ide_name -> h
    | _ :: t_pairs -> top t_pairs
  in
  match ide_stk with
    | Ide_stacks(ide_stk) -> top ide_stk;;
  

  let private_pop_ide (ide_stk : ide_stacks) (ide_name : ide) : ide_stacks =
    let rec pop = function
      | [] -> raise (exc_ide_stack_not_found ide_name)
      | (ide, []) :: _ when ide = ide_name -> raise (exc_ide_stack_empty ide_name)
      | (ide, _::t_ints) :: t_pairs when ide = ide_name -> (ide, t_ints) :: t_pairs
      | h_pair :: t_pairs -> h_pair :: pop t_pairs
    in
    match ide_stk with
    | Ide_stacks(ide_stk) -> Ide_stacks(pop ide_stk);;

  
(***************** if_stack *****************)

type if_stack = If_stack of bool list;;

let private_push_if (if_stk : if_stack) (value : bool) : if_stack =
  match if_stk with
    | If_stack(l) -> If_stack(value :: l);;

let private_top_if (if_stk : if_stack) : bool =
  match if_stk with
    | If_stack([]) -> raise exc_if_stack_empty
    | If_stack(h :: _) -> h;;

let private_pop_if (if_stk : if_stack) : if_stack =
  match if_stk with
    | If_stack([]) -> raise exc_if_stack_empty
    | If_stack(_ :: t) -> If_stack(t);;


(***************** while_stack *****************)

type while_stack = While_stack of bool list;;

let private_push_while (while_stk : while_stack) (value : bool) : while_stack =
  match while_stk with
    | While_stack(l) -> While_stack(value :: l);;

let private_top_while (while_stk : while_stack) : bool =
  match while_stk with
    | While_stack([]) -> raise exc_while_stack_empty
    | While_stack(h :: _) -> h;;

let private_pop_while (while_stk : while_stack) : while_stack =
  match while_stk with
    | While_stack([]) -> raise exc_while_stack_empty
    | While_stack(_ :: t) -> While_stack(t);;


(***************** delta *****************)
(* This type is a tuple including the 3 stack types defined above, and the interpreter 
** interacts with this type only (the functions defined on this type act as 
** the interface.)
*)

type delta = Delta of ide_stacks * if_stack * while_stack;;
  
(* ide_stack interface *)
let push_ide (d : delta) (ide_name : ide) (value : int) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_ide_stks = private_push_ide ide_stks ide_name value in
      Delta(updated_ide_stks, if_stk, while_stk);;

let top_ide (d : delta) (ide_name : ide) : int =
  match d with
    Delta(ide_stks, _, _) -> private_top_ide ide_stks ide_name;;

let pop_ide (d : delta) (ide_name : ide) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_ide_stks = private_pop_ide ide_stks ide_name in
      Delta(updated_ide_stks, if_stk, while_stk);;

(* if_stack interface *)
let push_if (d : delta) (value : bool) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_if_stk = private_push_if if_stk value in
      Delta(ide_stks, updated_if_stk, while_stk);;

let top_if (d : delta) : bool =
  match d with
    Delta(_, if_stk, _) -> private_top_if if_stk;;

let pop_if (d : delta) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_if_stk = private_pop_if if_stk in
      Delta(ide_stks, updated_if_stk, while_stk);;

(* while_stack interface *)
let push_while (d : delta) (value : bool) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_while_stk = private_push_while while_stk value in
      Delta(ide_stks, if_stk, updated_while_stk);;

let top_while (d : delta) : bool =
  match d with
    Delta(_, _, while_stk) -> private_top_while while_stk;;

let pop_while (d : delta) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_while_stk = private_pop_while while_stk in
      Delta(ide_stks, if_stk, updated_while_stk);;

let empty_delta = Delta(Ide_stacks [], If_stack [], While_stack []);;