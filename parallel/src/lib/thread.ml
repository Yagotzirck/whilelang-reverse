(** Implementation of the thread data type and the functions acting on it.*)

open Ast_ann

(** A thread is a tuple containing the following fields:

  + [prg]:            The annotated program associated to the thread;
  + [tid]:            An integer value that uniquely identifies the thread.
                      The value is released and usable for new threads as soon
                      as the thread terminates;
  + [ptid]:           An integer value that identifies the parent thread's ID
                      (that is, the [tid] of the thread that generated this thread);
  + [num_chld_done]:  An integer value that keeps track of how many threads generated
                      by the current thread are done executing their annotated program [prg].
                      This is useful because as soon as all of the child threads are done (2, in our case),
                      the parent thread must be moved from [waiting_threads] to [running_threads]
                      in the [State] module.
*)
type thread = Thread of program_ann * int * int * int;;

(** Creates and returns a new thread, initializing it with the given parameters
    (annotated program, thread id and the id of the thread who requested the creation)
    and setting the number of child threads which finished their execution to 0 (for obvious reasons).
*)
let create prg tid ptid =
  Thread (prg, tid, ptid, 0);;

(** This is a wrapper which applies {!val:Program.prev_stmt} to the program contained inside the specified
    thread parameter.
*)
let prev_stmt = function
  | Thread (prg, tid, ptid, num_chld_done) -> Thread (Program.prev_stmt prg, tid, ptid, num_chld_done);;

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
  | Thread (prg, tid, ptid, num_chld_done) -> Thread (Program.next_stmt prg, tid, ptid, num_chld_done);;

(** This is a wrapper which applies {!val:Program.push_stmt_counter} to the program contained inside the specified
    thread parameter.
*)
let push_stmt_counter stmt_counter = function
  | Thread (prg, tid, ptid, num_chld_done) -> Thread (Program.push_stmt_counter stmt_counter prg, tid, ptid, num_chld_done);;


(** Given a thread, returns its ID. *)
let get_tid = function
  | Thread (_, tid, _, _) -> tid;;