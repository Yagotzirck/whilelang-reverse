open OUnit2
open Interpreter
open State
open Augment
open Ast

let exec_and_get_var initial_state var =
  let final_state = sem_prg_fwd initial_state in
  let final_sigma = state_get_sigma final_state in
  get_value final_sigma var;;

  let exec_and_get_sigma initial_state =
    let final_state = sem_prg_fwd initial_state in
    state_get_sigma final_state;;

(* test programs *)
let n = 2;;

let prg_sum_1_to_n =
  Program(Assign (Val("acc"), Eint 0),
  Program(Assign (Val("i"), Eint 1),
  Program(
    While (Not (Eq (Val("i"), Eint (n+1))),
      Program(Cadd (Val("acc"), Val("i")), 
      Program(Cadd (Val("i"), Eint 1), Program_empty))),
  Program_empty)
  ));;

let prg_sum_1_to_n_initial_state = Pstate (aug_prg prg_sum_1_to_n, Sigma ( ("acc", -1), Sigma (("i", -1), Sigma_empty)), empty_delta);;

(* Program based on the one illustrated in paper
** "ReversingImperativeParallelPrograms.pdf", Figure 1
*)
let prg_fib =
  Program(
    Ifthenelse (Gt (Val "X", Val"Y"),
      Program(Assign (Val "Z", Val "Y"),
      Program(Assign (Val "Y", Val "X"),
      Program(Assign (Val "X", Val "Z"),
      Program_empty))),

      Program(Skip, Program_empty)),
  
  Program(
    While (Gt (Sub (Val "N", Eint 2), Eint 0),
      Program(Assign (Val "Z", Val "X"),
      Program(Assign (Val "X", Val "Y"),
      Program(Cadd (Val "Y", Val "Z"),
      Program(Csub (Val "N", Eint 1),
      Program_empty))))),
  Program_empty
  )
  );;

  let prg_fib_initial_sigma = 
    Sigma(("X", 4),
    Sigma(("Y", 3),
    Sigma(("Z", 0),
    Sigma(("N", 5),
    Sigma_empty))));;
  
  let prg_fib_initial_state = Pstate (aug_prg prg_fib, prg_fib_initial_sigma, empty_delta);;

  let prg_fib_final_sigma =
    Sigma(("X", 11),
    Sigma(("Y", 18),
    Sigma(("Z", 7),
    Sigma(("N", 2),
    Sigma_empty))));;




let test_interpreter = "Test suite for Interpreter module" >::: [

"sum_1_to_n" >:: (fun _ -> assert_equal ((n * (n+1)) / 2) (exec_and_get_var prg_sum_1_to_n_initial_state "acc"));
"fib_paper" >:: (fun _ -> assert_equal prg_fib_final_sigma (exec_and_get_sigma prg_fib_initial_state));

"rev_sum_1_to_n" >:: (fun _ ->
  let sum_final_state = sem_prg_fwd prg_sum_1_to_n_initial_state in
  assert_equal prg_sum_1_to_n_initial_state (sem_prg_rev sum_final_state)
  );

  "rev_fib_paper" >:: (fun _ ->
    let fib_paper_final_state = sem_prg_fwd prg_fib_initial_state in
    assert_equal prg_fib_initial_state (sem_prg_rev fib_paper_final_state);
  );
]

let _ = run_test_tt_main test_interpreter;;