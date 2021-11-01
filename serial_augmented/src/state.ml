open Ast;;
open Ast_aug;;

(* Exceptions' definitions *)
exception Unbound_variable of string;;
let exc_unbound_var var_name = Unbound_variable ("Variable \"" ^ var_name ^ "\" not found in the program store");;

exception Delta_stack_error of string;;
let exc_ide_stack_not_found ide_name = Delta_stack_error ("Auxiliary stack for variable \"" ^ ide_name ^ "\" doesn't exist");;
let exc_ide_stack_empty ide_name = Delta_stack_error ("Auxiliary stack for variable \"" ^ ide_name ^ "\" is empty");;
let exc_if_stack_empty = Delta_stack_error "if_stack is empty";;
let exc_while_stack_empty = Delta_stack_error "while_stack is empty";;


exception No_previous_statements;;
exception No_next_statements;;

exception If_start_not_found;;
exception While_start_not_found;;


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
    | Sigma_empty -> raise (exc_unbound_var ide_name)
    | Sigma((i, _), next) when i = ide_name -> Sigma((i, value), next)
    | Sigma (kv, next) -> Sigma(kv, assign_inner next)
  in
  assign_inner s;;

(* cassign: constructive assignments ( "+=" and "-=") *)
let cassign (s : sigma) (ide_name : ide) (operation : int -> int) =
  let rec cassign_inner = function
    | Sigma_empty -> raise (exc_unbound_var ide_name)
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
    | Sigma_empty -> raise (exc_unbound_var ide_name)
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
      | (ide, _::[]) :: t_pairs when ide = ide_name -> t_pairs  (* Just one item in stack -> delete the whole pair *)
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

(* program-related functions *)
let prg_prepend_stmt stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts @ [stmt_aug], curr_stmt, next_stmt);;

let prg_append_stmt stmt_aug = function
  | Program_aug (prev_stmts, curr_stmt, next_stmt) -> Program_aug (prev_stmts, curr_stmt, next_stmt @ [stmt_aug]);;

  let prg_prev_stmt = function
  | Program_aug ([], curr, next) -> Program_aug ([], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> Program_aug (prev_t, prev_h, curr :: next);;
let prg_next_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> Program_aug (curr:: prev, next_h, next_t);;

let rec prg_if_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise If_start_not_found
  | Program_aug (If_start outer_if :: [], curr, next) -> Program_aug (If_start outer_if :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> prg_if_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

let rec prg_while_block_1st_stmt = function
  | Program_aug ([], _, _) -> raise While_start_not_found
  | Program_aug (While_start (outer_while) :: [], curr, next) -> Program_aug (While_start (outer_while) :: [], curr, next)
  | Program_aug (prev_h :: prev_t, curr, next) -> prg_while_block_1st_stmt (Program_aug (prev_t, prev_h, curr :: next));;

let rec prg_last_stmt = function
  | Program_aug (prev, curr, []) -> Program_aug (prev, curr, [])
  | Program_aug (prev, curr, next_h :: next_t) -> prg_last_stmt (Program_aug (curr :: prev, next_h, next_t));;

let prg_get_prev_stmt = function
  | Program_aug ([], _, _) -> raise No_previous_statements
  | Program_aug (h::_, _, _) -> h;;

let prg_get_curr_stmt = function
  | Program_aug (_, curr, _) -> curr;;


(***************** prg_state *****************)
(* This type is a tuple including:
** 1) program_aug (current, previous prg and next statement);
** 2) The program store (sigma);
** 3) The auxiliary store (delta).
*)

type prg_state = Pstate of program_aug * sigma * delta;;

(* dassign function wrapper acting on prg_state *)
let state_dassign ide_name value = function
  | Pstate (p, s, d) -> Pstate (p, dassign s ide_name value, d);;

(* Ide_stacks function wrappers acting on prg_state *)
let state_push_ide ide_name = function
  | Pstate (p, s, d) -> Pstate (p, s, push_ide d ide_name (get_value s ide_name));;

let state_top_ide ide_name = function
  | Pstate (_, _, d) -> top_ide d ide_name;;

let state_pop_ide ide_name = function
  | Pstate (p, s, d) -> Pstate (p, s, pop_ide d ide_name);;


(* If_stack function wrappers acting on program_state *)
let state_push_if bool_var = function
  | Pstate (p, s, d) -> Pstate (p, s, push_if d bool_var);;

let state_top_if = function
  | Pstate (_, _, d) -> top_if d;;

let state_pop_if = function
  | Pstate (p, s, d) -> Pstate (p, s, pop_if d);;

(* While_stack function wrappers acting on program_state *)
let state_push_while bool_var = function
  | Pstate (p, s, d) -> Pstate (p, s, push_while d bool_var);;

let state_top_while = function
  | Pstate (_, _, d) -> top_while d;;

let state_pop_while = function
  | Pstate (p, s, d) -> Pstate (p, s, pop_while d);;



let state_get_program = function
| Pstate (p, _, _) -> p;;

let state_get_sigma = function
  | Pstate (_, s, _) -> s;;

let state_get_delta = function
  | Pstate (_, _, d) -> d;;

let state_set_program p = function
  | Pstate (_, s, d) -> Pstate (p, s, d);;

let state_set_sigma s = function
  | Pstate (p, _, d) -> Pstate (p, s, d);;

let state_set_delta d = function
  | Pstate (p, s, _) -> Pstate (p, s, d);;


let state_prepend_stmt stmt = function
  | Pstate (p, s, d) -> Pstate (prg_prepend_stmt stmt p, s, d);;

let state_append_stmt stmt = function
  | Pstate (p, s, d) -> Pstate (prg_append_stmt stmt p, s, d);;

let state_prev_stmt = function
  | Pstate (p, s, d) -> Pstate (prg_prev_stmt p, s, d);;

let state_get_prev_stmt p_state =
  state_get_program p_state |> prg_get_prev_stmt;;

let state_get_curr_stmt p_state =
  state_get_program p_state |> prg_get_curr_stmt;;
let state_next_stmt = function
  | Pstate (p, s, d) -> Pstate (prg_next_stmt p, s, d);;

let state_last_stmt = function
  Pstate (p, s, d) -> Pstate (prg_last_stmt p, s, d);;

let state_is_prg_at_start p_state =
  state_get_prev_stmt p_state = Program_start;;

let state_is_prg_at_end p_state =
  state_get_curr_stmt p_state = Program_end;;
