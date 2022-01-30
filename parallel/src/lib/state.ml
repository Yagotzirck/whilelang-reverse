(** Implementation of the state and the functions acting on it.*)

open Ast;;
open Thread;;


(** [state] is a type consisting of a tuple including:
    + running_threads (the list of threads currently executing);
    + waiting_threads (the list of threads that generated the threads in [running_threads],
      waiting for their children to finish their execution before resuming their own execution
      by moving back to [running_threads];
    + num_curr_stmt (an integer counting the number of statements executed so far, used to annotate
      statements as they are executed);
    + num_curr_thread (an integer counting the number of active threads (both running and waiting), used
      to assign an ID to each thread upon creation);
    + The program store (sigma);
    + The auxiliary store (delta).
*)
type state = State of thread list * thread list * int * int * Sigma.sigma * Delta.delta;;

(** Value used to initialize state's [num_curr_stmt] value. *)
let first_stmt_value = 1;;

(** Value used to initialize state's [num_curr_thread] value. *)
let root_tid_value = 0;;   (* Root thread ID *)


(** Executes a destructive assignment in forward execution mode, assigning [new_value] to the variable [ide_name].

    In order to preserve [ide_name]'s current value, [curr_value] is retrieved from the
    sigma store and pushed onto [ide_name]'s stack in the auxiliary delta store; at that
    point, [ide_name]'s value in the sigma store can be overwritten with [new_value],
    and the resulting new state is returned.
*)
let dassign_fwd ~ide_name:(ide_name : ide) ~new_value:(new_value : int) = function
  | State (t_running, t_waiting, num_stmts, num_threads, s, d) ->
      let curr_value = Sigma.get_value s ide_name in
      State (t_running, t_waiting, num_stmts, num_threads, Sigma.dassign s ide_name new_value, Delta.push_ide d ide_name curr_value);;

  
(** Executes a destructive assignment in reverse execution mode.
    
    [ide_name]'s previous value ([retrieved_value]) is retrieved from [ide_name]'s stack in the auxiliary
    delta store, and [retrieved_value] gets popped (removed) from [ide_name]'s stack; at that point,
    [retrieved_value] is assigned to [ide_name] in the sigma store, and the resulting new state is returned.
*)
let dassign_rev ~ide_name:(ide_name : ide) = function
  | State (t_running, t_waiting, num_stmts, num_threads, s, d) ->
      let retrieved_value = Delta.top_ide d ide_name in
      let new_delta = Delta.pop_ide d ide_name in
      let new_sigma = Sigma.dassign s ide_name retrieved_value in
      State (t_running, t_waiting, num_stmts, num_threads, new_sigma, new_delta);;


(** This is a wrapper which applies {!val:Sigma.cadd} to the sigma store
    contained inside the specified state parameter, and returns the resulting state.
*)
  let cadd ide_name value = function
    | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (t_running, t_waiting, num_stmts, num_threads, Sigma.cadd s ide_name value, d);;
  
  (** This is a wrapper which applies {!val:Sigma.csub} to the sigma store
    contained inside the specified state parameter, and returns the resulting state.
*)
let csub ide_name value = function
  | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (t_running, t_waiting, num_stmts, num_threads, Sigma.csub s ide_name value, d);;


(** Given a thread ID and a list of threads, returns the thread in the list matching the ID
    passed as a parameter, or raises [Not_found] if no thread ID matches the given one.

    Auxiliary function for the functions defined below (not to be used directly outside of this module).
*)
let rec get_thread_from_list tid = function
    | [] -> raise Not_found
    | Thread (prg, curr_tid, ptid, num_chld_done) :: _ when curr_tid = tid -> Thread (prg, curr_tid, ptid, num_chld_done)
    | _ :: t -> get_thread_from_list tid t;;

(** Given a thread ID and a list of threads, removes the thread from the list matching the ID
    passed as a parameter, or raises [Not_found] if no thread ID matches the given one.

    Auxiliary function for the functions defined below (not to be used directly outside of this module).
*)
let rec remove_thread_from_list tid = function
    | [] -> raise Not_found
    | Thread (_, curr_tid, _, _) :: t when curr_tid = tid -> t
    | h :: t -> h :: remove_thread_from_list tid t;;


(** Returns the running thread with the given ID contained inside the specified state parameter. *)
let get_running_thread tid = function
    | State (t_running, _, _, _, _, _) -> get_thread_from_list tid t_running;;

(** Adds a thread to the list of running threads inside the specified state parameter. *)
let add_running_thread new_thread = function
    | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (new_thread :: t_running, t_waiting, num_stmts, num_threads, s, d);;

(** Removes the thread with the given ID from the list of running threads inside the specified state parameter. *)
let remove_running_thread tid = function
    | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (remove_thread_from_list tid t_running, t_waiting, num_stmts, num_threads, s, d);;


(** Returns the waiting thread with the given ID contained inside the specified state parameter. *)
let get_waiting_thread tid = function
    | State(_, t_waiting, _, _, _, _) -> get_thread_from_list tid t_waiting;;

(** Adds a thread to the list of waiting threads inside the specified state parameter. *)
let add_waiting_thread new_thread = function
    | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (t_running, new_thread :: t_waiting, num_stmts, num_threads, s, d);;

(** Removes the thread with the given ID from the list of waiting threads inside the specified state parameter. *)
let remove_waiting_thread tid = function
    | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (t_running, remove_thread_from_list tid t_waiting, num_stmts, num_threads, s, d);;


(** Returns the statement counter (the number of statements executed thus far) contained in the state passed as a parameter. *)
let get_stmt_counter = function
    | State (_, _, num_stmts, _, _, _) -> num_stmts;;

(** Given a thread [thread] and a thread list, updates (replaces) the thread in the list
    having the same ID as [thread] with [thread].

    Auxiliary function for the functions defined below (not to be used directly outside of this module).
*)
let rec update_thread_in_list thread = function
  | [] -> raise Not_found
  | Thread (_, curr_tid, _, _) :: t when curr_tid = (Thread.get_tid thread) -> thread :: t
  | h :: t -> h :: (update_thread_in_list thread t)

  
(** Pushes the statement counter in the source code stack of the current instruction in the program
    contained in the specified running thread ID.
*)
let push_stmt_counter tid state =
    let stmt_counter = get_stmt_counter state in
    let target_thread = get_running_thread tid state in
    let updated_thread = Thread.push_stmt_counter stmt_counter target_thread in
    
    match state with
      | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (update_thread_in_list updated_thread t_running, t_waiting, num_stmts, num_threads, s, d);;




(* get_program and set_program removed *)



(** Returns the sigma store contained inside the specified state parameter. *)
let get_sigma = function
  | State (_, _, _, _, s, _) -> s;;


(** Takes a program and an initial sigma store as parameters, and returns a state with
    - A list of active threads initialized with a thread containing the given program;
    - An empty list of waiting threads;
    - [num_curr_stmt] initialized to [first_stmt_value];
    - [num_curr_thread] initialized to [root_tid_value] + 1;W
    - The given sigma store;
    - An empty auxiliary (delta) store.
*)
let init prg sigma =
  State ([(Thread.create prg 0 0)], [], first_stmt_value, root_tid_value + 1,  sigma, Delta.empty_delta);;




(* prepend_stmt and append_stmt removed *)




(** This is a wrapper which applies {!val:Thread.prev_stmt} to the thread having the
    specified running thread ID contained inside the specified state parameter, and returns the resulting state.
*)
let prev_stmt tid state =
    let target_thread = get_running_thread tid state in
    let updated_thread = Thread.prev_stmt target_thread in
    match state with
      | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (update_thread_in_list updated_thread t_running, t_waiting, num_stmts, num_threads, s, d);;


(** This is a wrapper which applies {!val:Thread.get_prev_stmt} to the thread having the
    specified running thread ID contained inside the specified state parameter, and returns the resulting state.
*)
let get_prev_stmt tid state =
  get_running_thread tid state |> Thread.get_prev_stmt;;

(** This is a wrapper which applies {!val:Thread.get_curr_stmt} to the running thread with the given ID
    contained inside the specified state parameter.
*)
let get_curr_stmt tid state =
  get_running_thread tid state |> Thread.get_curr_stmt;;

(** This is a wrapper which applies {!val:Thread.next_stmt} to the running thread with the given ID
    contained inside the specified state parameter.
*)
let next_stmt tid state =
let target_thread = get_running_thread tid state in
let updated_thread = Thread.next_stmt target_thread in
match state with
  | State (t_running, t_waiting, num_stmts, num_threads, s, d) -> State (update_thread_in_list updated_thread t_running, t_waiting, num_stmts, num_threads, s, d);;



(* last_stmt, is_prg_at_start and is_prg_at_end removed *)