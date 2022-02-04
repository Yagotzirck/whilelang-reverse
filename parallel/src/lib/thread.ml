(** Implementation of the thread data type and the functions acting on it.*)

open Ast_ann


(** A thread is a tuple containing the following fields:

  + [prg]:            The annotated program associated to the thread;
  + [tid]:            An integer value that uniquely identifies the thread.
                      The value is released and usable for new threads as soon
                      as the thread terminates;
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

(** Handles the forward execution of statement {!val:Program.Par_prg_end}
    (that is, a child thread terminated its program execution) at thread level.

    The following steps are performed:

    + The child thread branch is retrieved, in order to determine whether the program contained in the child thread belongs
      to the first or the second program inside the {!val:Program.Par} statement of the parent thread;
    + The {!val:Program.Par} program branch contained in the parent thread is updated, according to the branch retrieved from
      the previous step.

@param t_chld The child thread containing the program to be put inside [t_parent]'s [Par] statement.
@param t_parent The parent thread whose current statement is the [Par] statement to be updated with the finished child thread's program.

@return The updated parent thread.

*)
let update_par_prg_fwd ~t_chld ~t_parent =
  let get_child_branch = function
    | Thread (_, _, _, branch) -> branch
  in

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
  | Thread (prg, _, _, _) -> Program.pop_prev_stmt_counter prg;;


(** Given a thread, returns its ID. *)
let get_tid = function
  | Thread (_, tid, _, _) -> tid;;

(** Given a thread, returns its parent thread ID. *)
let get_ptid = function
  | Thread (_, _, ptid, _) -> ptid;;


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