open OUnit2
open Interpreter
open State
open Ast

let exec_and_get_var prg initial_state var =
  let final_state = sem_prg prg initial_state in
  let final_sigma = get_sigma final_state in
  get_value final_sigma var;;

  let exec_and_get_sigma prg initial_state =
    let final_state = sem_prg prg initial_state in
    get_sigma final_state;;

(* test programs *)
let n = 5;;
let prg_sum_1_to_n =
  Program(Assign (Val("acc"), Eint 0),
  Program(Assign (Val("i"), Eint 1),
  Program(
    While (Not (Eq (Val("i"), Eint (n+1))),
      Program(Cadd (Val("acc"), Val("i")), 
      Program(Cadd (Val("i"), Eint 1), Program_empty))),
  Program_empty)
  ));;

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
  
  let prg_fib_initial_state = Pstate (prg_fib_initial_sigma, empty_delta);;

  let prg_fib_final_sigma =
    Sigma(("X", 11),
    Sigma(("Y", 18),
    Sigma(("Z", 7),
    Sigma(("N", 2),
    Sigma_empty))));;




let test_interpreter = "Test suite for Interpreter module" >::: [
"sum_1_to_n" >:: (fun _ -> assert_equal ((n * (n+1)) / 2) (exec_and_get_var prg_sum_1_to_n empty_state "acc"));
"fib_paper" >:: (fun _ -> assert_equal prg_fib_final_sigma (exec_and_get_sigma prg_fib prg_fib_initial_state))
]

let _ = run_test_tt_main test_interpreter;;