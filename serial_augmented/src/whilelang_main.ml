open Ast
(* open Ast_aug *)
open Augment
open Interpreter
open State
open Print_state

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


 let sum_prg_start = Pstate (aug_prg prg_sum_1_to_n, Sigma ( ("acc", -1), Sigma (("i", -1), Sigma_empty)), empty_delta);;
let fib_prg_start = prg_fib_initial_state;;

let rec exec_prg prg_state =
  print_state prg_state;
  print_string "\n\n\n\n\nSteps (0 to finish): ";

  let steps = read_int() in
  if steps <> 0 then
    exec_prg (sem_prg_steps prg_state steps)
  else
    ();;

let open_arg_file =
  match Sys.argv with
    | [| _; filename |] ->
      open_in filename
    
    | _ ->
      Printf.eprintf "Usage: whilelang_main <source file>\n";
      exit 1;;


let main =
  let in_ch = open_arg_file in
  let lexbuf = Lexing.from_channel in_ch in
  let prg_sigma = Lexer_parser.Parser.prg_state Lexer_parser.Lexer.read lexbuf in
  close_in in_ch;
  match prg_sigma with
    | (p, s) -> exec_prg (state_init (aug_prg p) s);;