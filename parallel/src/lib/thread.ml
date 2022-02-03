(** Implementation of the thread data type and the functions acting on it.*)

open Ast_ann

(** type used in the [thread] tuple, to discern whether the thread contains a program
    contained in the first (Left) or in the second (Right) element of the [Par] statement
    of the parent thread that created it.
*)
type child_branch = Left | Right | Root;;

(** A thread is a tuple containing the following fields:

  + [prg]:            The annotated program associated to the thread;
  + [tid]:            An integer value that uniquely identifies the thread.
                      The value is released and usable for new threads as soon
                      as the thread terminates;
  + [ptid]:           An integer value that identifies the parent thread's ID
                      (that is, the [tid] of the thread that generated this thread);
  + [branch]:         A variable of type [child_branch], indicating whether the thread is
                      a [Left] child or a [Right] child;
*)
type thread = Thread of program_ann * int * int * child_branch;;

(** Creates and returns a new thread, initializing it with the given parameters
    (annotated program, thread id, id of the thread who requested the creation, and branch).
*)
let create prg tid ptid branch =
  Thread (prg, tid, ptid, branch);;

(** This is a wrapper which applies {!val:Program.prev_stmt} to the program contained inside the specified
    thread parameter.
*)
let prev_stmt = function
  | Thread (prg, tid, ptid, branch) -> Thread (Program.prev_stmt prg, tid, ptid, branch);;

(** This is a wrapper which applies {!val:Program.get_prev_stmt} to the program contained inside the specified
    thread parameter.
*)
let get_prev_stmt = function
  | Thread (prg, _, _, _) -> Program.get_prev_stmt prg;;


(** This is a wrapper which applies {!val:Program.get_curr_stmt} to the program contained inside the specified
    thread parameter.
*)
let get_curr_stmt = function
  | Thread (prg, _, _, _) -> Program.get_curr_stmt prg;;

(** This is a wrapper which applies {!val:Program.next_stmt} to the program contained inside the specified
    thread parameter.
*)
let next_stmt = function
  | Thread (prg, tid, ptid, branch) -> Thread (Program.next_stmt prg, tid, ptid, branch);;

(** This is a wrapper which applies {!val:Program.push_stmt_counter} to the program contained inside the specified
    thread parameter.
*)
let push_stmt_counter stmt_counter = function
  | Thread (prg, tid, ptid, branch) -> Thread (Program.push_stmt_counter stmt_counter prg, tid, ptid, branch);;

(** This is a wrapper which applies {!val:Program.top_stmt_counter} to the program contained inside the specified
    thread parameter.
*)
let top_prev_stmt_counter = function
  | Thread (prg, _, _, _) -> Program.top_prev_stmt_counter prg;;


(** This is a wrapper which applies {!val:Program.pop_prev_stmt_counter} to the program contained inside the specified
    thread parameter.
*)
let pop_prev_stmt_counter = function
  | Thread (prg, _, _, _) -> Program.pop_prev_stmt_counter prg;;


(** Given a thread, returns its ID. *)
let get_tid = function
  | Thread (_, tid, _, _) -> tid;;


(** Given a thread ID and a list of threads, returns the thread in the list matching the ID
    passed as a parameter, or raises [Not_found] if no thread ID matches the given one.
*)
let rec get_thread_from_list tid = function
    | [] -> raise Not_found
    | Thread (prg, curr_tid, ptid, branch) :: _ when curr_tid = tid -> Thread (prg, curr_tid, ptid, branch)
    | _ :: t -> get_thread_from_list tid t;;

(** Given a thread ID and a list of threads, removes the thread from the list matching the ID
    passed as a parameter, or raises [Not_found] if no thread ID matches the given one.
*)
let rec remove_thread_from_list tid = function
    | [] -> raise Not_found
    | Thread (_, curr_tid, _, _) :: t when curr_tid = tid -> t
    | h :: t -> h :: remove_thread_from_list tid t;;

(** Given a thread [thread] and a thread list, updates (replaces) the thread in the list
    having the same ID as [thread] with [thread].
*)
let rec update_thread_in_list thread = function
  | [] -> raise Not_found
  | Thread (_, curr_tid, _, _) :: t when curr_tid = (get_tid thread) -> thread :: t
  | h :: t -> h :: (update_thread_in_list thread t);;