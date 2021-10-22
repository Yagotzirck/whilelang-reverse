exception Unbound_variable of string
val exc_create_unbound_var : string -> exn
exception Delta_stack_error of string
val exc_ide_stack_not_found : string -> exn
val exc_ide_stack_empty : string -> exn
val exc_if_stack_empty : exn
val exc_while_stack_empty : exn

type sigma = Sigma_empty | Sigma of (string * int) * sigma
val dassign : sigma -> string -> int -> sigma
val increment : sigma -> string -> int -> sigma
val decrement : sigma -> string -> int -> sigma
val get_value : sigma -> string -> int

val empty_sigma: sigma

type ide_stacks = Ide_stacks of (string * int list) list
type if_stack = If_stack of bool list
type while_stack = While_stack of bool list
type delta = Delta of ide_stacks * if_stack * while_stack;;

val push_ide : delta -> string -> int -> delta
val top_ide : delta -> string -> int
val pop_ide : delta -> string -> delta

val push_if : delta -> bool -> delta
val top_if : delta -> bool
val pop_if : delta -> delta

val push_while : delta -> bool -> delta
val top_while : delta -> bool
val pop_while : delta -> delta

val empty_delta : delta