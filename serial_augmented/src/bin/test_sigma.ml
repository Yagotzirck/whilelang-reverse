open OUnit2
open Serial_interp.Sigma

(* Data for sigma tests *)

let sigma_one_var = Sigma(("X", 5), Sigma_empty);;
let sigma_two_vars = Sigma(("X", 5), Sigma(("Y", 76), Sigma_empty));;
let sigma_two_vars_d_assignment = Sigma(("X", 84), Sigma(("Y", 76), Sigma_empty));;

let sigma_cadd_result = Sigma(("X", 5), Sigma(("Y", 101), Sigma_empty));;
let sigma_csub_result = Sigma(("X", 2), Sigma(("Y", 76), Sigma_empty));;


let test_sigma = "Test suite for Sigma module" >::: [
"sigma_destructive_assign" >:: (fun _ -> assert_equal sigma_two_vars_d_assignment (dassign sigma_two_vars "X" 84));
"sigma_destructive_assign_unbound_var" >:: (fun _ -> assert_raises (exc_unbound_var "Y") (fun () -> dassign sigma_one_var "Y" 5));

"sigma_cadd" >:: (fun _ -> assert_equal sigma_cadd_result (cadd sigma_two_vars "Y" 25));
"sigma_csub" >:: (fun _ -> assert_equal sigma_csub_result (csub sigma_two_vars "X" 3));

"sigma_cadd_unbound_var" >:: (fun _ -> assert_raises (exc_unbound_var "Z") (fun () -> cadd sigma_two_vars "Z" 30));
"sigma_csub_unbound_var" >:: (fun _ -> assert_raises (exc_unbound_var "Z") (fun () -> csub sigma_two_vars "Z" 30));
]
let _ = run_test_tt_main test_sigma