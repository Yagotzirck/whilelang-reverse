open Ast_aug;;


(***************** state *****************)

(** [state] is a type consisting of a tuple including:
    -1) program_aug;
    -2) The program store (sigma);
    -3) The auxiliary store (delta).
*)
type state = State of program_aug * Sigma.sigma * Delta.delta;;

(** [Sigma.dassign] function wrapper applied to Program.state. *)
let dassign_fwd ide_name new_value = function
  | State (p, s, d) ->
      let curr_value = Sigma.get_value s ide_name in
      State (p, Sigma.dassign s ide_name new_value, Delta.push_ide d ide_name curr_value);;

let dassign_rev ide_name = function
  | State (p, s, d) ->
      let retrieved_value = Delta.top_ide d ide_name in
      let new_delta = Delta.pop_ide d ide_name in
      let new_sigma = Sigma.dassign s ide_name retrieved_value in
      State (p, new_sigma, new_delta);;

  let cadd ide_name value = function
    | State (p, s, d) -> State (p, Sigma.cadd s ide_name value, d);;
  
  let csub ide_name value = function
    | State (p, s, d) -> State (p, Sigma.csub s ide_name value, d);;

(* Ide_stacks function wrappers acting on Program.state *)

let top_ide ide_name = function
  | State (_, _, d) -> Delta.top_ide d ide_name;;

let pop_ide ide_name = function
  | State (p, s, d) -> State (p, s, Delta.pop_ide d ide_name);;


(* If_stack function wrappers acting on program_state *)
let push_if bool_var = function
  | State (p, s, d) -> State (p, s, Delta.push_if d bool_var);;

let top_if = function
  | State (_, _, d) -> Delta.top_if d;;

let pop_if = function
  | State (p, s, d) -> State (p, s, Delta.pop_if d);;

(* While_stack function wrappers acting on program_state *)
let push_while bool_var = function
  | State (p, s, d) -> State (p, s, Delta.push_while d bool_var);;

let top_while = function
  | State (_, _, d) -> Delta.top_while d;;

let pop_while = function
  | State (p, s, d) -> State (p, s, Delta.pop_while d);;



let get_program = function
| State (p, _, _) -> p;;

let get_sigma = function
  | State (_, s, _) -> s;;

let get_delta = function
  | State (_, _, d) -> d;;

let set_program p = function
  | State (_, s, d) -> State (p, s, d);;

let set_sigma s = function
  | State (p, _, d) -> State (p, s, d);;

let set_delta d = function
  | State (p, s, _) -> State (p, s, d);;

let init prg sigma =
  State (prg, sigma, Delta.empty_delta);;

let prepend_stmt stmt = function
  | State (p, s, d) -> State (Program.prepend_stmt stmt p, s, d);;

let append_stmt stmt = function
  | State (p, s, d) -> State (Program.append_stmt stmt p, s, d);;

let prev_stmt = function
  | State (p, s, d) -> State (Program.prev_stmt p, s, d);;

let get_prev_stmt p_state =
  get_program p_state |> Program.get_prev_stmt;;

let get_curr_stmt p_state =
  get_program p_state |> Program.get_curr_stmt;;
let next_stmt = function
  | State (p, s, d) -> State (Program.next_stmt p, s, d);;

let last_stmt = function
  State (p, s, d) -> State (Program.last_stmt p, s, d);;

let is_prg_at_start p_state =
  get_prev_stmt p_state = Program_start;;

let is_prg_at_end p_state =
  get_curr_stmt p_state = Program_end;;