(** Implementation of the thread data type and the functions acting on it.*)

open Ast_ann

(** The string argument indicates the function where the exception has occurred. *)
exception Thread_not_found of string;;


(** A thread is a tuple containing the following fields:

  + [prg]:            The annotated program associated to the thread;
  + [tid]:            An integer value that uniquely identifies the thread;
  + [ptid]:           An integer value that identifies the parent thread's ID
                      (that is, the [tid] of the thread that generated this thread);
  + [branch]:         A variable of type [child_branch], indicating whether the thread is
                      a [Left] child, a [Right] child, or the [Root] thread;
*)
type thread = Thread of program_ann * int * int * Program.child_branch;;

(** Creates and returns a new thread, initializing it with the given parameters
    (annotated program, thread id, id of the thread who requested the creation, and branch).
*)
let create prg tid ptid branch =
  Thread (prg, tid, ptid, branch);;

(** Given a thread, returns the annotated program contained in it. *)
let get_program = function
  | Thread (prg, _, _, _) -> prg;;

(** Given a thread, returns its thread ID. *)
let get_tid = function
  | Thread (_, tid, _, _) -> tid;;

(** Given a thread, returns its parent thread ID. *)
let get_ptid = function
  | Thread (_, _, ptid, _) -> ptid;;

(** Given a thread, returns its child branch (Left, Right, or Root.) *)
let get_child_branch = function
  | Thread (_, _, _, branch) -> branch;;

(** Given a child thread and its parent thread, puts the child thread's program into
    the [Par] statement contained in the parent thread's program.

    The following steps are performed:

    + The child thread branch is retrieved, in order to determine whether the program contained in the child thread belongs
      to the first or the second program inside the {!val:Program.Par} statement of the parent thread;
    + The {!val:Program.Par} program branch contained in the parent thread is updated, according to the branch retrieved from
      the previous step.

@param t_chld The child thread containing the program to put inside [t_parent]'s [Par] statement.
@param t_parent The parent thread whose current statement is the [Par] statement to be updated with the finished child thread's program.

@return The updated parent thread.
*)
let update_par_prg t_chld t_parent =
  match t_parent with
    | Thread (prg, tid, ptid, branch) -> Thread (Program.update_par_prg (get_child_branch t_chld) (get_program t_chld) prg, tid, ptid, branch);;

let update_par_prg_fwd t_chld t_parent =
  match t_parent with
    | Thread (prg, tid, ptid, branch) -> Thread (Program.update_par_prg_fwd (get_child_branch t_chld) (get_program t_chld) prg, tid, ptid, branch);;


(** Given a waiting thread, returns a boolean value indicating whether its children
    are done with their execution ([true]) or not ([false]).

    This is a wrapper which applies {!val:Program.is_children_execution_done} to the program contained inside the specified
    thread parameter.
*)
let is_children_execution_done = function
  | Thread (prg, _, _, _) -> Program.is_children_execution_done prg;;

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
  | Thread (prg, tid, ptid, branch) -> Thread (Program.pop_prev_stmt_counter prg, tid, ptid, branch);;

(** Given a thread ID and a list of threads, returns the thread in the list matching the ID
    passed as a parameter, or raises [Thread_not_found] if no thread ID matches the given one.
*)
let rec get_thread_from_list tid = function
    | [] -> raise (Thread_not_found __FUNCTION__)
    | Thread (prg, curr_tid, ptid, branch) :: _ when curr_tid = tid -> Thread (prg, curr_tid, ptid, branch)
    | _ :: t -> get_thread_from_list tid t;;

(** Given a thread ID and a list of threads, removes the thread from the list matching the ID
    passed as a parameter, or raises [Thread_not_found] if no thread ID matches the given one.
*)
let rec remove_thread_from_list tid = function
    | [] -> raise (Thread_not_found __FUNCTION__)
    | Thread (_, curr_tid, _, _) :: t when curr_tid = tid -> t
    | h :: t -> h :: remove_thread_from_list tid t;;

(** Given a thread [thread] and a thread list, updates (replaces) the thread in the list
    having the same ID as [thread] with [thread].
*)
let rec update_thread_in_list thread = function
  | [] -> raise (Thread_not_found __FUNCTION__)
  | Thread (_, curr_tid, _, _) :: t when curr_tid = (get_tid thread) -> thread :: t
  | h :: t -> h :: (update_thread_in_list thread t);;

(** Given an int representing the number of the last executed statement and a list of threads,
    returns the thread contained in the given list which was executed last.
*)
let rec get_last_executed_thread_from_list num_last_stmt tlist =
  try
    match tlist with
      | [] -> None
      | h :: _ when top_prev_stmt_counter h = num_last_stmt -> Some h
      | _ :: t -> get_last_executed_thread_from_list num_last_stmt t
  with
    | Program.Statement_without_stack _ -> get_last_executed_thread_from_list num_last_stmt (List.tl tlist);;

(** Given an int value representing the last statement's execution counter and
    a list of waiting threads, scans the thread list and returns a pair [Some (prg, branch, ptid)]
    if the [Par statement] in one of the threads contains the last executed instruction in one
    of its two programs (indicated by [branch]), [None] otherwise.
*)
let rec get_last_executed_par_prg_from_list num_last_stmt = function
  | [] -> None
  | Thread (prg, tid, _, _) :: t ->
      match Program.get_last_executed_par_prg num_last_stmt prg with
        | None -> get_last_executed_par_prg_from_list num_last_stmt t
        | Some (prg, branch) -> Some (prg, branch, tid);;


(** Given an int representing the parent thread ID and a list of threads,
    returns the threads generated by the given parent thread ID from the
    list as a pair (thread1, thread2).
*)
let get_child_threads_from_list ptid tlist =
  let rec leave_children_only = function
    | [] -> []
    | h :: t when get_ptid h = ptid -> h :: leave_children_only t
    | _ :: t -> leave_children_only t
  in

  match leave_children_only tlist with
    | [chld1; chld2] -> (chld1, chld2)
    | _ -> raise (Thread_not_found __FUNCTION__);;

(** Given an int representing the parent thread ID and a list of threads,
    removes the threads generated by the given parent thread ID from the
    list and returns it.
*)
let rec remove_child_threads_from_list ptid = function
  | [] -> []
  | h :: t when get_ptid h = ptid -> remove_child_threads_from_list ptid t
  | h :: t -> h :: remove_child_threads_from_list ptid t;;


let inc_prev_par_finished_children = function
  | Thread (prg, curr_tid, ptid, branch) -> Thread (Program.inc_prev_par_finished_children prg, curr_tid, ptid, branch);;

let dec_prev_par_finished_children = function
  | Thread (prg, curr_tid, ptid, branch) -> Thread (Program.dec_prev_par_finished_children prg, curr_tid, ptid, branch);;

let rec is_thread_in_list tid = function
  | [] -> false
  | Thread (_, curr_tid, _, _) :: _ when curr_tid = tid -> true
  | _ :: t -> is_thread_in_list tid t;;