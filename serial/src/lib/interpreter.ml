(** Augmented interpreter for the serial version of the while language. *)


open Ast
open Ast_aug
open Augment

(* Exceptions' definitions *)
exception Assignment_to_non_var of string;;
exception Illegal_statement_fwd_execution of string;;
exception Illegal_statement_rev_execution of string;;



(** Integer semantics' definition.
@param expr The integer expression to evaluate.
@param s The sigma store (used in case the expression contains variables whose value must be retrieved.)
@return The integer value resulting from the expression's evaluation.
*)
let rec sem_int ~expr:(expr : int_expr) ~s:(s : Sigma.sigma) : int =
  match expr with
    | Val (i) -> Sigma.get_value s i
    | Eint (x) -> x
    | Add (e1, e2) -> (sem_int e1 s) + (sem_int e2 s)
    | Sub (e1, e2) -> (sem_int e1 s) - (sem_int e2 s);;

(** Boolean semantics' definition.
@param expr The boolean expression to evaluate.
@param s The sigma store (used in case the expression contains variables whose value must be retrieved.)
@return The boolean value resulting from the expression's evaluation.
*)
let rec sem_bool ~expr:(expr : bool_expr) ~expr:(s : Sigma.sigma) : bool =
  match expr with
    | Ebool (b) -> b
    | Not (b) -> not (sem_bool b s)
    | Eq (e1, e2) -> (sem_int e1 s) = (sem_int e2 s)
    | Gt (e1, e2) -> (sem_int e1 s) > (sem_int e2 s)
    | And (b1, b2) -> (sem_bool b1 s) && (sem_bool b2 s);;


(* Auxiliary functions for sem_stmt and sem_stmt_rev *)

(** Performs forward execution's destructive assignment:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Assigns [e2]'s evaluation to the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.dassign_fwd} for details such as pushing the previous value
    in the delta store before performing the assignment.)
@param e1 The expression containing the variable to which we must assign the new value.
@param e2 The expression containing the value to assign.
@param state The state where the assignment must be performed.
@return The new state where the assignment has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let assign_var_fwd ~e1 ~e2 ~state =
  match e1 with
    | Val (i) ->
        let value = sem_int e2 (State.get_sigma state) in
        State.dassign_fwd i value state
        
    | _ -> raise (Assignment_to_non_var "Assign");;

  
(** Performs reverse execution's destructive assignment:

  + Evaluates [e1], making sure it's a variable [ide];
  + Assigns [ide]'s previous value to [ide] and returns the state
    with the above changes applied
    (see {!val:State.dassign_rev} for details such as popping [ide]'s previous value from
    the delta store.)
@param e1 The expression containing the variable to which we must assign the previous value.
@param state The state where the assignment must be performed.
@return The new state where the assignment has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let assign_var_rev ~e1 ~state =
  match e1 with
    | Val (i) -> State.dassign_rev i state

    | _ -> raise (Assignment_to_non_var "Assign (reverse execution)");;


(** Performs constructive sum:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Sums [e2]'s evaluation to the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.cadd} for details.)
@param e1 The expression containing the variable to which we must add [e2]'s evaluation
@param e2 The expression containing the value to add to the variable specified by [e1].
@param state The state where the sum must be performed.
@return The new state where the sum has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let cadd ~e1 ~e2 ~state =
  match e1 with
  | Val (i) ->
      let value = sem_int e2 (State.get_sigma state) in
      State.cadd i value state 
    
  | _ -> raise (Assignment_to_non_var "Cadd, or Csub if execution was reversed");;

  
(** Performs constructive subtraction:

  + Evaluates [e1], making sure it's a variable;
  + Evaluates [e2];
  + Subtracts [e2]'s evaluation from the variable specified by [e1],
    and returns the state with the above changes applied
    (see {!val:State.csub} for details.)
@param e1 The expression containing the variable from which we must subtract [e2]'s evaluation
@param e2 The expression containing the value to subtract from the variable specified by [e1].
@param state The state where the subtraction must be performed.
@return The new state where the subtraction has been performed.
@raise Assignment_to_non_var if [e1]'s evaluation isn't a variable/identifier.
*)
let csub ~e1 ~e2 ~state =
  match e1 with
  | Val (i) ->
        let value = sem_int e2 (State.get_sigma state) in
        State.csub i value state

  | _ -> raise (Assignment_to_non_var "Csub, or Cadd if execution was reversed");;


(** Evaluates [Ifthenelse] statements during forward execution.

    The following operations are performed in the given order:

    + [b] ([Ifthenelse]'s boolean expression) is evaluated;
    + [prg] (The current program where the [Ifthenelse] statement was encountered)
    is retrieved from [state];
    + If the result of [b]'s evaluation is [true], [prg1] (the program associated with
      the "then" branch) is assigned to [if_prg_block] for further processing; otherwise
      ([b]'s evaluation is [false]), [prg2] (the program associated with the "else" branch)
      is assigned to [if_prg_block] for further processing;
    + [Ifthenelse]'s branch to be executed ([if_prg_block]) gets augmented by calling
      {!val:Augment.aug_if_block} and the augmented program is assigned to [aug_if_prg_block];
    + A new state containing [aug_if_prg_block] as program is returned.
@param b [Ifthenelse]'s boolean expression, whose evaluation determines whether to execute [prg1] or [prg2].
@param prg1 The program associated with [Ifthenelse]'s "then" branch.
@param prg2 The program associated with [Ifthenelse]'s "else" branch.
@param state The state where the [Ifthenelse] statement under analysis was encountered.
@return A new state having as program the augmented [Ifthenelse] branch to execute.
*)
let if_eval_fwd ~b ~prg1 ~prg2 ~state =
  let b_eval = sem_bool b (State.get_sigma state) in
  let prg = State.get_program state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval if_prg_block prg  in
  State.set_program aug_if_prg_block state;;

(** Evaluates [Ifthenelse] statements during reverse execution.

    The following operations are performed in the given order:

    + [b_eval] ([Ifthenelse]'s boolean evaluation, which was evaluated during forward execution)
      is retrieved and removed(popped) from {!type:Delta.if_stack} in the auxiliary delta store;
    + [new_state] is created by putting the new delta store with [b_eval] removed from the top of {!type:Delta.if_stack},
      and moving the program's current statement one statement backward (keep in mind that
      {!val:sem_stmt_rev} evaluates the {b previous} statement rather than the current one, therefore
      if we want [Ifthenelse] to become the current statement we must move one step back);
    + [prg] (The current program where the [Ifthenelse] statement under evaluation is now the current
      statement) is retrieved from [new_state];
    + If the result of [b_eval] is [true], [prg1] (the program associated with
      the "then" branch) is assigned to [if_prg_block] for further processing; otherwise
      ([b]'s evaluation is [false]), [prg2] (the program associated with the "else" branch)
      is assigned to [if_prg_block] for further processing;
    + [Ifthenelse]'s branch to be executed ([if_prg_block]) gets augmented by calling
      {!val:Augment.aug_if_block}; the current statement in the augmented program is set to the last one
      by feeding it to {!val:Program.last_stmt} (we are performing reverse execution, so we must begin
      from the program's last statement) and the result is assigned to [aug_if_prg_block];
    + A new state containing [aug_if_prg_block] as program is returned.
@param prg1 The program associated with [Ifthenelse]'s "then" branch.
@param prg2 The program associated with [Ifthenelse]'s "else" branch.
@param state The state where the [Ifthenelse] statement under analysis was encountered.
@return A new state having as program the augmented [Ifthenelse] branch to execute (starting from the program's last statement),
        as well as an updated {!type:Delta.if_stack} in the sigma store with the top value removed from the stack.
*)
let if_eval_rev ~prg1 ~prg2 ~state =
  let b_eval = State.top_if state in
  let new_state = State.pop_if state |> State.prev_stmt in
  let prg = State.get_program new_state in
  
  let if_prg_block =
    if b_eval then prg1 else prg2
  in
  let aug_if_prg_block = aug_if_block b_eval (Program.last_stmt if_prg_block) prg |> Program.last_stmt in
  State.set_program aug_if_prg_block new_state;;


(** Evaluates [While] statements during forward execution.

    The following operations are performed in the given order:

    + [b_expr] ([While]'s boolean expression) is evaluated and assigned to [b_eval];
    + [new_state] is created by pushing [false] onto {!type:Delta.while_stack}; this step is necessary
    since during forward execution we have the following two scenarios:
      - [While]'s first [b_expr] evaluation was [false] ([While]'s program block was skipped): since no [true]
      values were pushed onto {!type:Delta.while_stack}, the [false] value we pushed gets popped during reverse
      execution and [While]'s program block gets skipped again;
      - [While]'s [b_expr] evaluates to [true] for a given {b n} amount of times; {b n} [true] values get pushed
      onto {!type:Delta.while_stack}, therefore during reverse execution we pop those [true] values which make the interpreter
      iterate [While]'s program block {b n} times, at which point the [false] value we pushed gets popped and [While]'s
      reverse iteration stops.
    + The program having the statement following the current [While] as the current statement is assigned to
      [prg_while_next_stmt];
    + if [b_eval] is [true], it means we must execute the program block inside [While] at least once, therefore
      [while_prg] gets augmented by calling {!val:Augment.aug_while_block} and assigned to [prg_to_execute]; otherwise
      ([b_eval] = [false]), [While]'s program block gets skipped and [prg_while_next_stmt] is assigned to [prg_to_execute];
    + A state having [prg_to_execute] as program and [new_state] as state is returned.
@param b_expr [While]'s boolean expression.
@param while_prg The program associated to [While]'s body;
@param state The state where the [While] statement has been encountered.
@return A state having:
        - as a program: the augmented [while_prg] if [b_expr]'s evaluation is [true], or the program having the statement
          following the [While] under evaluation as current statement if [b_expr]'s evaluation is [false];
        - as sigma store: the same as the [state] parameter;
        - as auxiliary delta store: [state]'s delta store, with [false] pushed onto {!type:Delta.while_stack}.
*)
let while_eval_fwd ~b_expr ~while_prg ~state =
  let b_eval = sem_bool b_expr (State.get_sigma state) in
  let new_state = State.push_while false state in
  let prg_while_next_stmt = State.get_program new_state |> Program.next_stmt in
  let outer_prg = State.get_program new_state in

  let prg_to_execute =
    if b_eval then
    aug_while_block b_expr while_prg outer_prg
  else
    prg_while_next_stmt
  in
  State.set_program prg_to_execute new_state;;


(** Evaluates [While] statements during reverse execution.

    The following operations are performed in the given order:

    + The boolean value on the top of {!type:Delta.while_stack} gets retrieved and assigned to [b_eval];
    + [new_state] is created from [state] by removing (popping) {!type:Delta.while_stack}'s top value and setting as the current statement
      the [While] statement (keep in mind that {!val:sem_stmt_rev} evaluates the {b previous} statement rather than the current one, therefore
      if we want [While] to become the current statement we must move one step back);
    + The program having the statement preceding the current [While] as the current statement is assigned to
      [prg_while_prev_stmt];
    + if [b_eval] is [true], it means we must execute the program block inside [While] at least once, therefore
      [while_prg] gets augmented by calling {!val:Augment.aug_while_block} and assigned to [prg_to_execute]; otherwise
      ([b_eval] = [false]), [While]'s program block gets skipped and [prg_while_next_stmt] is assigned to [prg_to_execute];
    + A state having [prg_to_execute] as program and [new_state] as state is returned.
@param b_expr [While]'s boolean expression.
@param while_prg The program associated to [While]'s body;
@param state The state where the [While] statement has been encountered.
@return A state having:
        - as a program: the augmented [while_prg] if [b_expr]'s evaluation is [true], or the program having the statement
          following the [While] under evaluation as current statement if [b_expr]'s evaluation is [false];
        - as sigma store: the same as the [state] parameter;
        - as auxiliary delta store: [state]'s delta store, with [false] pushed onto {!type:Delta.while_stack}.
*)
let while_eval_rev ~b_expr ~while_prg ~state =
  let b_eval = State.top_while state in
  let new_state = State.pop_while state |> State.prev_stmt in
  let prg_while_prev_stmt = State.get_program new_state in
  let outer_prg = State.get_program new_state in

  let prg_to_execute =
    if b_eval then
    aug_while_block b_expr while_prg outer_prg |> Program.last_stmt
  else
    prg_while_prev_stmt
  in
  State.set_program prg_to_execute new_state ;;

(** Evaluates [While_start] statements during reverse execution.

    NOTE: [While_start] statements can be evaluated ONLY during reverse execution, since they mark the
    beginning of a [While]'s program body; if they get evaluated during forward execution, then there's a problem
    (exception [Illegal_statement_fwd_execution] would be raised), but it should never happen anyway.

    The following operations are performed in the given order:

    + The boolean value on the top of {!type:Delta.while_stack} gets retrieved and assigned to [b_eval];
    + [new_state] is created from [state] by removing (popping) {!type:Delta.while_stack}'s top value (which we just assigned
      to [b_eval] in the previous step);
    + if [b_eval] is [true], it means we must perform another reverse iteration of the [While] program block we're currently executing
      starting from the last statement,therefore we set the current statement to the last statement of the current program block
      (by calling {!val:Program.last_stmt}) and the result is assigned to [prg_to_execute].
      Otherwise ([b_eval] is [false]), it means the last iteration of [While]'s program has been performed and we need to jump out of the loop, therefore
      [while_stmt_in_outer_prg] (which is [While_start]'s parameter consisting in the outer program having as current statement the [While] statement
      containing the program block which is currently being executed) gets assigned to [prg_to_execute];
    + A state having [prg_to_execute] as program and [new_state] as state is returned.
@param  while_stmt_in_outer_prg The program having the [While] statement whose body is being executed, where execution will continue in case reverse iteration
        must be stopped due to a [false] value popped from {!type:Delta.while_stack}.
@param state The current state, where the [While_start] statement has been encountered.
@return A state having:
        - as a program: the current program having the current statement set to the program's last statement if the value retrieved from
          {!type:Delta.while_stack} is [true], or [while_stmt_in_outer_prg] if the value retrieved from {!type:Delta.while_stack} is [false];
        - as sigma store: the same as the [state] parameter;
        - as auxiliary delta store: [state]'s delta store, with the retrieved boolean value [b_eval] popped(removed) from the top of {!type:Delta.while_stack}.
*)
let while_start_eval ~while_stmt_in_outer_prg ~state =
  let b_eval = State.top_while state in
  let new_state = State.pop_while state in
  let prg_to_execute =
    if b_eval then
      State.get_program new_state |> Program.last_stmt
    else
      while_stmt_in_outer_prg
  in
  State.set_program prg_to_execute new_state;;  


(** Evaluates [While_end] statements during forward execution.

    NOTE: [While_end] statements can be evaluated ONLY during forward execution, since they mark the
    end of a [While]'s program body; if they get evaluated during reverse execution, then there's a problem
    (exception [Illegal_statement_rev_execution] would be raised), but it should never happen anyway.

    The following operations are performed in the given order:

    + [b_expr] ([While]'s boolean expression) is evaluated and assigned to [b_eval];
    + [new_state] is created from [state] by pushing [true] onto {!type:Delta.while_stack} (We're at the end of a [While] program iteration which has
      has been executed, therefore we must push [true] to mark the fact an iteration has been executed when we reverse execute the program,
      regardless of [b_eval]'s value);
    + if [b_eval] is [true], it means we must perform another forward iteration of the [While] program block we're currently executing
      starting from the first statement,therefore we set the current statement to the first statement of the current program block
      (by calling {!val:Program.while_block_1st_stmt}) and the result is assigned to [prg_to_execute].
      Otherwise ([b_eval] is [false]), it means the last iteration of [While]'s program has been performed and we need to jump out of the loop, therefore
      [while_next_stmt_in_outer_prg] (which is [While_end]'s parameter consisting in the outer program having as current statement the statement following
      the [While] statement containing the program block which is currently being executed) is assigned to [prg_to_execute];
    + A state having [prg_to_execute] as program and [new_state] as state is returned.
@param  b_expr [While_end]'s boolean expression, inherited from the containing [While] statement.
@param  while_next_stmt_in_outer_prg The program having the [While] statement whose body is being executed, where execution will continue to [While]'s next
        statement in case iteration must be stopped due to [b_expr] evaluating to [false].
@param state The current state, where the [While_start] statement has been encountered.
@return A state having:
        - as a program: the current program having the current statement set to the program's first statement if [b_expr]'s evaluation
          is [true], or [while_next_stmt_in_outer_prg] if [b_expr]'s evaluation is [false];
        - as sigma store: the same as the [state] parameter;
        - as auxiliary delta store: [state]'s delta store, with [true] pushed onto {!type:Delta.while_stack}.
*)
let while_end_eval ~b_expr ~while_next_stmt_in_outer_prg ~state =
  let b_eval = sem_bool b_expr (State.get_sigma state) in
  let new_state = State.push_while true state in
  let prg_to_execute =
    if b_eval then
      State.get_program new_state |> Program.while_block_1st_stmt
    else
      while_next_stmt_in_outer_prg
  in
  State.set_program prg_to_execute new_state;; 

(** Given a state, performs a single evaluation step in forward execution mode and returns the resulting state.*)
let sem_stmt_fwd (curr_state : State.state) : State.state =
  let expr = State.get_curr_stmt curr_state in
  match expr with
    | Program_end -> curr_state
    | Skip -> curr_state |> State.next_stmt
    | Assign (e1, e2) -> (assign_var_fwd e1 e2 curr_state) |> State.next_stmt
    | Cadd (e1, e2) -> (cadd e1 e2 curr_state) |> State.next_stmt
    | Csub (e1, e2) -> (csub e1 e2 curr_state) |> State.next_stmt
    | Ifthenelse (b_expr, prg1, prg2) -> if_eval_fwd b_expr prg1 prg2 curr_state
    | If_end (prg, b_eval) -> State.set_program prg (State.push_if b_eval curr_state)
    | While (b_expr, prg) -> while_eval_fwd b_expr prg curr_state
    | While_end (b_expr, while_in_outer_prg) -> while_end_eval b_expr while_in_outer_prg curr_state

    (* The following statement expressions aren't supposed to be encountered during forward execution *)
    | Program_start -> raise (Illegal_statement_fwd_execution "Program_start")
    | If_start _ -> raise (Illegal_statement_fwd_execution "If_start")
    | While_start _ -> raise (Illegal_statement_fwd_execution "While_start");;
  

(** Given a state, performs a single evaluation step in reverse execution mode and returns the resulting state.*)
let sem_stmt_rev (curr_state : State.state) : State.state =
  let expr = State.get_prev_stmt curr_state in
  match expr with
    | Program_start -> curr_state
    | Skip -> curr_state |> State.prev_stmt
    | Assign (e1, _) -> (assign_var_rev e1 curr_state) |> State.prev_stmt
    | Cadd (e1, e2) -> (csub e1 e2 curr_state) |> State.prev_stmt
    | Csub (e1, e2) -> (cadd e1 e2 curr_state) |> State.prev_stmt
    | Ifthenelse (_, prg1, prg2) -> if_eval_rev prg1 prg2 curr_state
    | If_start (prg_prev) -> State.set_program prg_prev curr_state
    | While (b_expr, prg) -> while_eval_rev b_expr prg curr_state
    | While_start (while_in_outer_prg) -> while_start_eval while_in_outer_prg curr_state

    (* The following statement expressions aren't supposed to be encountered during reverse execution *)
    | Program_end -> raise (Illegal_statement_rev_execution "Program_end")
    | If_end _ -> raise (Illegal_statement_rev_execution "If_end")
    | While_end (_, _) -> raise (Illegal_statement_rev_execution "While_end");;


(** Given a state, evaluates all statements until the end of the program in forward execution mode and returns the resulting state.*)
let rec sem_prg_fwd curr_state =
  if not (State.is_prg_at_end curr_state) then
    sem_prg_fwd (sem_stmt_fwd curr_state)
  else
    curr_state;;

(** Given a state, evaluates all statements until the beginning of the program in reverse execution mode and returns the resulting state.*)
let rec sem_prg_rev curr_state =
  if not (State.is_prg_at_start curr_state) then
    sem_prg_rev (sem_stmt_rev curr_state)
  else
    curr_state;;

(** Given a state [curr_state] and a specified [num_steps] integer, let [remaining_stmts] be the number of statements 
    between the current statement in [curr_state]'s program and the [Program_end] boundary statement; then the function
    performs min([remaining_stmts], [num_steps]) statement evaluations in forward execution mode and returns the resulting state.

    If [num_steps] <= 0, [curr_state] is returned unaltered.
*)
let rec sem_prg_fwd_steps curr_state num_steps =
  if not (State.is_prg_at_end curr_state) && num_steps > 0 then
    sem_prg_fwd_steps (sem_stmt_fwd curr_state) (num_steps - 1)
  else
    curr_state;;

(** Given a state [curr_state] and a specified [num_steps] integer, let [remaining_stmts] be the number of statements 
    between the current statement in [curr_state]'s program and the [Program_start] boundary statement; then the function
    performs min([remaining_stmts], [num_steps]) statement evaluations in reverse execution mode and returns the resulting state.

    If [num_steps] <= 0, [curr_state] is returned unaltered.
*)
    let rec sem_prg_rev_steps curr_state num_steps =
  if not (State.is_prg_at_start curr_state) && num_steps > 0 then
    sem_prg_rev_steps (sem_stmt_rev curr_state) (num_steps -1)
  else
    curr_state;;


(** Given a state [curr_state] and a specified [num_steps] integer, it acts as an interface allowing both forward and reverse
    execution by calling
      - {!val:sem_prg_fwd_steps} (forward execution), if [num_steps] >= 0;
      - {!val:sem_prg_rev_steps} (reverse execution), if [num_steps] < 0 (in this case, [num_steps]'s sign gets inverted to obtain the
        positive amount of steps to execute to pass to {!val:sem_prg_rev_steps} as parameter.)
*)
let sem_prg_steps curr_state num_steps =
  if num_steps >= 0 then
    sem_prg_fwd_steps curr_state num_steps
  else
    sem_prg_rev_steps curr_state (-num_steps);;