open OUnit2
open State


(* Data for sigma tests *)
let sigma_one_var = Sigma(("X", 5), Sigma_empty);;
let sigma_two_vars = Sigma(("X", 5), Sigma(("Y", 76), Sigma_empty));;
let sigma_two_vars_d_assignment = Sigma(("X", 84), Sigma(("Y", 76), Sigma_empty));;

let sigma_inc_result = Sigma(("X", 5), Sigma(("Y", 101), Sigma_empty));;
let sigma_dec_result = Sigma(("X", 2), Sigma(("Y", 76), Sigma_empty));;


(* Data for delta tests *)
let ide_stk = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [7;8;9]) ];;
let if_stk = If_stack [true; true; true];;
let while_stk = While_stack [false; false; false];;
let delta_test = Delta (ide_stk, if_stk, while_stk);;

let test_state = "Test suite for State module" >::: [
"sigma_one_decl" >:: (fun _ -> assert_equal sigma_one_var (dassign Sigma_empty "X" 5));
"sigma_two_decl" >:: (fun _ -> assert_equal sigma_two_vars (dassign sigma_one_var "Y" 76));

"sigma_destructive_assign" >:: (fun _ -> assert_equal sigma_two_vars_d_assignment (dassign sigma_two_vars "X" 84));

"sigma_increment" >:: (fun _ -> assert_equal sigma_inc_result (increment sigma_two_vars "Y" 25));
"sigma_decrement" >:: (fun _ -> assert_equal sigma_dec_result (decrement sigma_two_vars "X" 3));

"sigma_inc_unbound_var" >:: (fun _ -> assert_raises (exc_create_unbound_var "Z") (fun () -> increment sigma_two_vars "Z" 30));
"sigma_dec_unbound_var" >:: (fun _ -> assert_raises (exc_create_unbound_var "Z") (fun () -> decrement sigma_two_vars "Z" 30));

]
let _ = run_test_tt_main test_state