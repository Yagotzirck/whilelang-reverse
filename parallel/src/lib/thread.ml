(** Implementation of the thread data type and the functions acting on it.*)

(**
A thread is a tuple containing the following fields:

  + [prg]:            The annotated program associated to the thread;
  + [tid]:            An integer value that uniquely identifies the thread.
                      The value is released and usable for new threads as soon
                      as the thread terminates;
  + [ptid]:           An integer value that identifies the parent thread's ID
                      (that is, the [tid] of the thread that generated this thread);
  + [num_chld_done]:  An integer value that keeps track of how many threads generated
                      by the current thread are done executing their annotated program [prg].
                      This is useful because as soon as all of its child threads are done (2, in our case),
                      the parent thread must be moved from [waiting_threads] to [running_threads]
                      in the [State] module.
*)
  