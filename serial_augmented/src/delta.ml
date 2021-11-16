(** Implementation of the auxiliary store and the functions acting on it.*)

open Ast


exception Delta_stack_error of string;;
let exc_ide_stack_not_found ide_name = Delta_stack_error ("Auxiliary stack for variable '" ^ ide_name ^ "' doesn't exist");;
let exc_if_stack_empty = Delta_stack_error "if_stack is empty";;
let exc_while_stack_empty = Delta_stack_error "while_stack is empty";;


(***************** ide_stacks *****************)

(** [ide_stacks] is a type representing a list of tuples [(var_name, prev_values_stack)]; its goal
    is to keep track of variables' previous values each time a destructive assignment is performed,
    by pushing the older value onto the respective variable's stack before performing the assignment.
    By doing so, previous values of any given variable can be retrieved during reverse execution by simply "popping" the value
    in the head of the list and assigning it to that variable in the sigma store. *)
type ide_stacks = Ide_stacks of (ide * int list) list;;


(** Pushes a value onto the specified [ide_name] variable's stack.

    If a tuple ([ide_name], [ide_stack_list]) already exists in [ide_stk], [value] is simply prepended to that list
    (pushed onto that [ide_stack_list]): ([ide_name], [value] :: [ide_stack_list]).

    If a tuple for [ide_name] doesn't exist, a tuple ([ide_name], [value] :: []) gets created and appended to [ide_stk].

@param  ide_stk The list of tuples (variable_name, variable_name_stack_list) representing variables' auxiliary store.
@param  ide_name The name of the variable which underwent a destructive assignment, for which we want to preserve its
        previous value by pushing it onto its associated stack.
@param value The previous integer value to push onto the stack associated to [ide_name].
@return The updated [ide_stk], where [value] has been pushed onto [ide_name]'s associated stack.
*)
let private_push_ide ~ide_stk:(ide_stk : ide_stacks) ~ide_name:(ide_name : ide) ~value:(value : int) : ide_stacks =
  let rec push = function
    | [] -> [ (ide_name, [value]) ]
    | (ide, l) :: t when ide = ide_name -> (ide, value :: l) :: t
    | h :: t -> h :: push t
  in
  match ide_stk with
    | Ide_stacks(ide_stk) -> Ide_stacks (push ide_stk);;


(** Retrieves the integer value from the top of the specified [ide_name] variable's stack.

@param  ide_stk The list of tuples (variable_name, variable_name_stack_list).
@param  ide_name The name of the variable associated to the stack from which we want to retrieve the integer value
        on its top.
@return The integer on the top of the stack associated to [ide_name].
@raise Delta_stack_error with a descriptive message about the variable/identifier, in case the
        tuple ([ide_name], ide_name_stack_list) wasn't found inside [ide_stk].
*)
let private_top_ide ~ide_stk:(ide_stk : ide_stacks) ~ide_name:(ide_name : ide) : int =
  let rec top = function
    | [] -> raise (exc_ide_stack_not_found ide_name)
    | (ide, h::_) :: _ when ide = ide_name -> h
    | _ :: t_pairs -> top t_pairs
  in
  match ide_stk with
    | Ide_stacks(ide_stk) -> top ide_stk;;
  

  (** Pops (removes) the integer value from the top of the specified [ide_name] variable's stack.

@param  ide_stk The list of tuples (variable_name, variable_name_stack_list).
@param  ide_name The name of the variable associated to the stack from which we want to retrieve the integer value
        on its top.
@return The updated [ide_stk], where the element previously located on top of [ide_name]'s associated stack has been removed.

        If [ide_name]'s associated stack contained a single element, the whole ([ide_name], ide_name_stack_list) pair gets removed
        from [ide_stk].
@raise Delta_stack_error with a descriptive message about the variable/identifier, in case the
        tuple ([ide_name], ide_name_stack_list) wasn't found inside [ide_stk].
*)
  let private_pop_ide ~ide_stk:(ide_stk : ide_stacks) ~ide_name:(ide_name : ide) : ide_stacks =
    let rec pop = function
      | [] -> raise (exc_ide_stack_not_found ide_name)
      | (ide, _::[]) :: t_pairs when ide = ide_name -> t_pairs  (* Just one item in stack -> delete the whole pair *)
      | (ide, _::t_ints) :: t_pairs when ide = ide_name -> (ide, t_ints) :: t_pairs
      | h_pair :: t_pairs -> h_pair :: pop t_pairs
    in
    match ide_stk with
    | Ide_stacks(ide_stk) -> Ide_stacks(pop ide_stk);;

  
(***************** if_stack *****************)

  
(** [if_stack] is a type representing a stack of booleans; its goal
    is to keep track of which branches have been executed inside
    [Ifthenelse] statements, in order to identify the branch to execute
    during reverse execution by popping values from this stack
    (true = "then" branch, false = "else" branch.)
*)
type if_stack = If_stack of bool list;;

(** Pushes [value] on the top of [if_stk] stack.*)
let private_push_if ~if_stk:(if_stk : if_stack) ~value:(value : bool) : if_stack =
  match if_stk with
    | If_stack(l) -> If_stack(value :: l);;

(** Retrieves the boolean value from the top of [if_stk] stack.
@raise  Delta_stack_error with a message telling that if_stack is empty, in case
        the function is applied to an empty [if_stk] stack.
*)
let private_top_if (if_stk : if_stack) : bool =
  match if_stk with
    | If_stack([]) -> raise exc_if_stack_empty
    | If_stack(h :: _) -> h;;

(** Pops (removes) the boolean value from the top of the specified [if_stk] stack.
@raise  Delta_stack_error with a message telling that if_stack is empty, in case
        the function is applied to an empty [if_stk] stack.
*)
let private_pop_if (if_stk : if_stack) : if_stack =
  match if_stk with
    | If_stack([]) -> raise exc_if_stack_empty
    | If_stack(_ :: t) -> If_stack(t);;


(***************** while_stack *****************)

(** [while_stack] is a type representing a stack of booleans; its goal
    is to keep track of how many times the program constituting a
    [While]'s body has been executed, in order to iterate that program for the
    same amount of times during reverse execution.

    The way it works is that during forward execution [false] is pushed in [while_stack],
    and [true] is pushed at the end of each [While]'s program body; by doing so, during
    reverse execution we keep reiterating [While]'s body as long as [true] is popped from
    [while_stack], and execution continues to the instruction preceding [While] as soon as
    [false] is popped: this guarantees that the same amount of iterations of [While]'s body
    performed during forward execution is performed during reverse execution as well.
*)
type while_stack = While_stack of bool list;;


(** Pushes [value] on the top of [while_stk] stack.*)
let private_push_while ~while_stk:(while_stk : while_stack) ~value:(value : bool) : while_stack =
  match while_stk with
    | While_stack(l) -> While_stack(value :: l);;

(** Retrieves the boolean value from the top of [while_stk] stack.
@raise  Delta_stack_error with a message telling that while_stack is empty, in case
        the function is applied to an empty [while_stk] stack.
*)
let private_top_while ~while_stk:(while_stk : while_stack) : bool =
  match while_stk with
    | While_stack([]) -> raise exc_while_stack_empty
    | While_stack(h :: _) -> h;;


(** Pops (removes) the boolean value from the top of the specified [while_stk] stack.
@raise  Delta_stack_error with a message telling that while_stack is empty, in case
        the function is applied to an empty [while_stk] stack.
*)
let private_pop_while ~while_stk:(while_stk : while_stack) : while_stack =
  match while_stk with
    | While_stack([]) -> raise exc_while_stack_empty
    | While_stack(_ :: t) -> While_stack(t);;


(** [delta] is a type representing the auxiliary store, consisting of:
    + A list of stacks [ide_stacks], where each stack is contained inside a pair (<identifier>, <integer list>) that keeps track
    of each variable's value: every time a destructive assignment is performed on a certain <ide> variable, its previous
    value is pushed onto the stack associated to that <ide> variable, in order to preserve it;
    + A stack [if_stack], which keeps track of all the boolean values used as tests inside conditional statements
    (that is, [Ifthenelse] statements);
    + A stack [while_stack], whick keeps track of all the boolean values used as tests inside [While] loops.
*)
type delta = Delta of ide_stacks * if_stack * while_stack;;
  
(* ide_stack interface for delta *)

(** This is a wrapper which applies {!val:private_push_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let push_ide ~d:(d : delta) ~ide_name:(ide_name : ide) ~value:(value : int) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_ide_stks = private_push_ide ide_stks ide_name value in
      Delta(updated_ide_stks, if_stk, while_stk);;

(** This is a wrapper which applies {!val:private_top_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let top_ide ~d:(d : delta) ~ide_name:(ide_name : ide) : int =
  match d with
    Delta(ide_stks, _, _) -> private_top_ide ide_stks ide_name;;

(** This is a wrapper which applies {!val:private_pop_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let pop_ide ~d:(d : delta) ~ide_name:(ide_name : ide) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_ide_stks = private_pop_ide ide_stks ide_name in
      Delta(updated_ide_stks, if_stk, while_stk);;

(* if_stack interface for delta *)

(** This is a wrapper which applies {!val:private_pop_ide} to the parameter [if_stk] inside the specified
    [d] delta parameter.
*)
let push_if ~d:(d : delta) ~(value : bool) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_if_stk = private_push_if if_stk value in
      Delta(ide_stks, updated_if_stk, while_stk);;

(** This is a wrapper which applies {!val:private_top_if} to the parameter [if_stk] inside the specified
    [d] delta parameter.
*)
let top_if ~d:(d : delta) : bool =
  match d with
    Delta(_, if_stk, _) -> private_top_if if_stk;;

(** This is a wrapper which applies {!val:private_pop_if} to the parameter [if_stk] inside the specified
    [d] delta parameter.
*)
let pop_if ~d:(d : delta) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_if_stk = private_pop_if if_stk in
      Delta(ide_stks, updated_if_stk, while_stk);;

(* while_stack interface for delta *)

(** This is a wrapper which applies {!val:private_push_while} to the parameter [while_stk] inside the specified
    [d] delta parameter.
*)
let push_while ~d:(d : delta) ~value:(value : bool) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_while_stk = private_push_while while_stk value in
      Delta(ide_stks, if_stk, updated_while_stk);;

(** This is a wrapper which applies {!val:private_top_while} to the parameter [while_stk] inside the specified
    [d] delta parameter.
*)
let top_while ~d:(d : delta) : bool =
  match d with
    Delta(_, _, while_stk) -> private_top_while while_stk;;


(** This is a wrapper which applies {!val:private_pop_while} to the parameter [while_stk] inside the specified
    [d] delta parameter.
*)
let pop_while ~d:(d : delta) : delta =
  match d with
    Delta(ide_stks, if_stk, while_stk) ->
      let updated_while_stk = private_pop_while while_stk in
      Delta(ide_stks, if_stk, updated_while_stk);;

(** Returns an empty auxiliary store, *)
let empty_delta = Delta(Ide_stacks [], If_stack [], While_stack []);;


