(** Implementation of the state and the functions acting on it.*)

open Ast;;
open Ast_aug;;


(** [state] is a type consisting of a tuple including:
    + program_aug (the list of statements constituting the program);
    + The program store (sigma);
    + The auxiliary store (delta).
*)
type state = State of program_aug * Sigma.sigma * Delta.delta;;

(** Executes a destructive assignment in forward execution mode, assigning [new_value] to the variable [ide_name].

    In order to preserve [ide_name]'s current value, [curr_value] is retrieved from the
    sigma store and pushed onto [ide_name]'s stack in the auxiliary delta store; at that
    point, [ide_name]'s value in the sigma store can be overwritten with [new_value],
    and the resulting new state is returned.
*)
let dassign_fwd ~ide_name:(ide_name : ide) ~new_value:(new_value : int) = function
  | State (p, s, d) ->
      let curr_value = Sigma.get_value s ide_name in
      State (p, Sigma.dassign s ide_name new_value, Delta.push_ide d ide_name curr_value);;

  
(** Executes a destructive assignment in reverse execution mode.
    
    [ide_name]'s previous value ([retrieved_value]) is retrieved from [ide_name]'s stack in the auxiliary
    delta store, and [retrieved_value] gets popped (removed) from [ide_name]'s stack; at that point,
    [retrieved_value] is assigned to [ide_name] in the sigma store, and the resulting new state is returned.
*)
let dassign_rev ~ide_name:(ide_name : ide) = function
  | State (p, s, d) ->
      let retrieved_value = Delta.top_ide d ide_name in
      let new_delta = Delta.pop_ide d ide_name in
      let new_sigma = Sigma.dassign s ide_name retrieved_value in
      State (p, new_sigma, new_delta);;


(** This is a wrapper which applies {!val:Sigma.cadd} to the sigma store
    contained inside the specified state parameter, and returns the resulting state.
*)
  let cadd ide_name value = function
    | State (p, s, d) -> State (p, Sigma.cadd s ide_name value, d);;
  
  (** This is a wrapper which applies {!val:Sigma.csub} to the sigma store
    contained inside the specified state parameter, and returns the resulting state.
*)
  let csub ide_name value = function
    | State (p, s, d) -> State (p, Sigma.csub s ide_name value, d);;


(* If_stack function wrappers acting on program_state *)

(** This is a wrapper which applies {!val:Delta.push_if} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let push_if bool_var = function
  | State (p, s, d) -> State (p, s, Delta.push_if d bool_var);;

(** This is a wrapper which applies {!val:Delta.top_if} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let top_if = function
  | State (_, _, d) -> Delta.top_if d;;

(** This is a wrapper which applies {!val:Delta.pop_if} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let pop_if = function
  | State (p, s, d) -> State (p, s, Delta.pop_if d);;

(* While_stack function wrappers acting on program_state *)

(** This is a wrapper which applies {!val:Delta.push_while} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let push_while bool_var = function
  | State (p, s, d) -> State (p, s, Delta.push_while d bool_var);;

(** This is a wrapper which applies {!val:Delta.top_while} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let top_while = function
  | State (_, _, d) -> Delta.top_while d;;

(** This is a wrapper which applies {!val:Delta.pop_while} to the auxiliary delta store
    contained inside the specified state parameter, and returns the resulting state.
*)
let pop_while = function
  | State (p, s, d) -> State (p, s, Delta.pop_while d);;


(** Returns the program contained inside the specified state parameter. *)
let get_program = function
| State (p, _, _) -> p;;

(** Given a program [p] and a state [state] as parameters, returns a state consisting of
    the given [state] with [p] replacing the previously contained program.
*)
let set_program p = function
  | State (_, s, d) -> State (p, s, d);;

(** Returns the sigma store contained inside the specified state parameter. *)
let get_sigma = function
  | State (_, s, _) -> s;;


(** Takes a program and an initial sigma store as parameters, and returns a state with
    - The given program;
    - The given sigma store;
    - An empty auxiliary (delta) store.
*)
let init prg sigma =
  State (prg, sigma, Delta.empty_delta);;

(** This is a wrapper which applies {!val:Program.prepend_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let prepend_stmt stmt = function
  | State (p, s, d) -> State (Program.prepend_stmt stmt p, s, d);;

(** This is a wrapper which applies {!val:Program.append_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let append_stmt stmt = function
  | State (p, s, d) -> State (Program.append_stmt stmt p, s, d);;

(** This is a wrapper which applies {!val:Program.prev_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let prev_stmt = function
  | State (p, s, d) -> State (Program.prev_stmt p, s, d);;

(** This is a wrapper which applies {!val:Program.get_prev_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let get_prev_stmt state =
  get_program state |> Program.get_prev_stmt;;

(** This is a wrapper which applies {!val:Program.get_curr_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let get_curr_stmt state =
  get_program state |> Program.get_curr_stmt;;

(** This is a wrapper which applies {!val:Program.next_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let next_stmt = function
  | State (p, s, d) -> State (Program.next_stmt p, s, d);;


(** This is a wrapper which applies {!val:Program.last_stmt} to the program contained inside the specified
    state parameter, and returns the resulting state.
*)
let last_stmt = function
  State (p, s, d) -> State (Program.last_stmt p, s, d);;

(** Returns a boolean value indicating whether the program's current statement/instruction
    in the given [state] is the main program's first statement/instruction. *)
let is_prg_at_start state =
  get_prev_stmt state = Program_start;;

(** Returns a boolean value indicating whether the program's current statement/instruction
    in the given [state] is the main program's last statement/instruction. *)
let is_prg_at_end state =
  get_curr_stmt state = Program_end;;