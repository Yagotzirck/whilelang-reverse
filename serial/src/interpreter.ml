(* Interpreter for the serial version of the while language. *)

open Ast;;


(* Exceptions' definitions *)
exception Unbound_variable of string;;


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
    | Sigma((i, v), next) when i = ide_name -> Sigma((i, value), next)
    | Sigma (kv, next) -> Sigma(kv, assign_inner next)
  in
  assign_inner s;;

(* cassign: constructive assignments ( "+=" and "-=") *)
let cassign (s : sigma) (ide_name : ide) (operation : int -> int) =
  let rec cassign_inner = function
    | Sigma_empty -> raise (Unbound_variable ("Variable \"" ^ ide_name ^ "\" not found in the program store"))
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
    | Sigma_empty -> raise (Unbound_variable ("Variable \"" ^ ide_name ^ "\" not found in the program store"))
    | Sigma((i, v), _) when i = ide_name -> v
    | Sigma(_, next) -> get_value s ide_name;;

  