(** Implementation of the auxiliary store and the functions acting on it.*)

open Ast


exception Delta_stack_error of string;;
let exc_ide_stack_not_found ide_name = Delta_stack_error ("Auxiliary stack for variable '" ^ ide_name ^ "' doesn't exist");;

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

  



(** [delta] is a type representing the auxiliary store, consisting of:
    + A list of stacks [ide_stacks], where each stack is contained inside a pair (<identifier>, <integer list>) that keeps track
    of each variable's value: every time a destructive assignment is performed on a certain <ide> variable, its previous
    value is pushed onto the stack associated to that <ide> variable, in order to preserve it.

    Unlike the serial version, [if_stack] and [while_stack] aren't present, since the parallel version doesn't implement neither
    [Ifthenelse] conditional execution nor [While] iterations (yet).
    
*)
type delta = Delta of ide_stacks;;
  
(* ide_stack interface for delta *)

(** This is a wrapper which applies {!val:private_push_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let push_ide ~d:(d : delta) ~ide_name:(ide_name : ide) ~value:(value : int) : delta =
  match d with
    Delta(ide_stks) ->
      let updated_ide_stks = private_push_ide ide_stks ide_name value in
      Delta(updated_ide_stks);;

(** This is a wrapper which applies {!val:private_top_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let top_ide ~d:(d : delta) ~ide_name:(ide_name : ide) : int =
  match d with
    Delta(ide_stks) -> private_top_ide ide_stks ide_name;;

(** This is a wrapper which applies {!val:private_pop_ide} to the parameter [ide_stks] inside the specified
    [d] delta parameter.
*)
let pop_ide ~d:(d : delta) ~ide_name:(ide_name : ide) : delta =
  match d with
    Delta(ide_stks) ->
      let updated_ide_stks = private_pop_ide ide_stks ide_name in
      Delta(updated_ide_stks);;

(** Returns an empty auxiliary store, *)
let empty_delta = Delta(Ide_stacks []);;


