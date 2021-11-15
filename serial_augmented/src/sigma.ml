(** Implementation of the program store and the functions acting on it.*)

open Ast;;

(* Exceptions' definitions *)
exception Unbound_variable of string;;
let exc_unbound_var var_name = Unbound_variable ("Variable '" ^ var_name ^ "' not found in the program store");;

(** sigma is a type representing the store (that is, the set of 
    all declared identifiers(variables) and the respective integer values associated to them).
*)
type sigma =
  | Sigma_empty
  | Sigma of (ide * int) * sigma;;

(** Destructive assignment.

    By {b destructive assignment} we mean an assignment where there's no way to retrieve the previous value
    without additional information; for example, after executing {e x = 5} we have no way to determine which
    value was stored inside {e x} without having saved it somewhere before the assignment (that "somewhere"
    is the {e delta} store.)

    Given a sigma store, overwrites the older value associated to [ide_name] with the new [value] specified.
@param s The sigma store where the variable to be updated is stored.
@param ide_name The variable name to update in the sigma store.
@param value The new integer value to assign to the [ide_name] variable.
@return The updated sigma store, where the identifier [ide_name] is now associated to [value].
@raise  Unbound_variable with a descriptive message about the variable/identifier, in case the
        specified [ide_name] wasn't found inside [s].
*)
let dassign ~s:(s : sigma) ~ide_name:(ide_name : ide) ~value:(value : int) =
  let rec assign_inner = function
    | Sigma_empty -> raise (exc_unbound_var ide_name)
    | Sigma((i, _), next) when i = ide_name -> Sigma((i, value), next)
    | Sigma (kv, next) -> Sigma(kv, assign_inner next)
  in
  assign_inner s;;


(** Constructive assignment.

    By {b constructive assignment} we mean an assignment where the previous value can be retrieved
    by looking at the instruction, without any additional information; for example, after executing
    {e x += 2} we can retrieve {e x}'s value prior to the assignment by trivially subtracting 2 from {e x}.

    Given a sigma store, updates the older value associated to [ide_name] in the way specified by the
    [operation] parameter function.
@param s The sigma store where the variable to be updated is stored.
@param ide_name The variable name to update in the sigma store.
@param operation A function specifying the constructive operation to be performed on [ide_name]'s value.
@return The updated sigma store, where the identifier [ide_name] is now associated with [operation]'s result.
@raise  Unbound_variable with a descriptive message about the variable/identifier, in case the
        specified [ide_name] wasn't found inside [s].
*)
let cassign ~s:(s : sigma) ~ide_name:(ide_name : ide) ~operation:(operation : int -> int) =
  let rec cassign_inner = function
    | Sigma_empty -> raise (exc_unbound_var ide_name)
    | Sigma((i, v), next) when i = ide_name -> Sigma((i, operation v), next)
    | Sigma (kv, next) -> Sigma(kv, cassign_inner next)
  in
  cassign_inner s;;


(** Constructive sum (+=): adds [value] to the value associated to
    [ide_name], and returns the updated [sigma] store where

    [ide_name] = ([ide_name]'s previous value) + [value].

    This is a mere wrapper for [cassign] specifying what the
    [operation] parameter function does.
*)
  let cadd ~s ~ide_name ~value =
    cassign s ide_name ( (+) value);;

  
  (** Constructive subtraction (+=): subtracts [value] from the value associated to
    [ide_name], and returns the updated [sigma] store where

    [ide_name] = ([ide_name]'s previous value) - [value].
  
  This is a mere wrapper for [cassign] specifying what the
    [operation] parameter function does.
*)
  let csub ~s ~ide_name ~value =
    cassign s ide_name (fun x -> x - value);;

(** Returns the integer value associated to the
    identifier passed as parameter.

@param s The sigma store where the variable containing the value to retrieve is stored.
@param ide_name The variable name associated to the value to retrieve in the sigma store.

@return The integer value associated to [ide_name] inside [s].
@raise  Unbound_variable with a descriptive message about the variable/identifier, in case the
        specified [ide_name] wasn't found inside [s].
*)
let rec get_value ~s ~ide_name =
  match s with
    | Sigma_empty -> raise (exc_unbound_var ide_name)
    | Sigma((i, v), _) when i = ide_name -> v
    | Sigma(_, next) -> get_value next ide_name;;

(** Returns [Sigma_empty] (an empty program store).*)
let empty_sigma = Sigma_empty;;