open OUnit2
open Serial_interp.Delta


(****** Data for delta tests ******)
let ide_stk = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [7;8;9]); ("Z", []) ];;
let if_stk = If_stack [true; true; true];;
let while_stk = While_stack [false; false; false];;
let delta_start = Delta (ide_stk, if_stk, while_stk);;

(* Ide_stacks data *)
let ide_stk_push_ide_result = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [85; 7;8;9]); ("Z", [])];;
let delta_push_ide_result = Delta (ide_stk_push_ide_result, if_stk, while_stk);;

  let ide_stk_push_empty_ide_result = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [7;8;9]); ("Z", [4]) ];;
let delta_push_empty_ide_result = Delta (ide_stk_push_empty_ide_result, if_stk, while_stk);;

let ide_stk_push_undeclared_ide_result = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [7;8;9]); ("Z", []); ("NewIde", [100]) ];;
let delta_push_undeclared_ide_result = Delta (ide_stk_push_undeclared_ide_result, if_stk, while_stk);;

let ide_stk_pop_ide_result = Ide_stacks [("A", [1;2;3]); ("B", [4;5;6]); ("C", [8;9]); ("Z", []) ];;
let delta_pop_ide_result = Delta (ide_stk_pop_ide_result, if_stk, while_stk);;


(* If_stack data *)
let if_stk_push_result = If_stack [false; true; true; true];;
let delta_push_if_result = Delta (ide_stk, if_stk_push_result, while_stk);;

let delta_empty_if = Delta (ide_stk, If_stack [], while_stk);;
let delta_push_empty_if_result = Delta (ide_stk, If_stack[true], while_stk);;

let if_stk_pop_result = If_stack [true; true];;
let delta_pop_if_result = Delta (ide_stk, if_stk_pop_result, while_stk);;






let test_delta = "Test suite for Delta module" >::: [

(****** delta tests ******)

(* Ide_stacks tests *)
"delta_push_ide" >:: (fun _ -> assert_equal delta_push_ide_result (push_ide delta_start "C" 85));
"delta_push_empty_ide" >:: (fun _ -> assert_equal delta_push_empty_ide_result (push_ide delta_start "Z" 4));
"delta_push_undeclared_ide" >:: (fun _ -> assert_equal delta_push_undeclared_ide_result (push_ide delta_start "NewIde" 100));

"delta_top_ide1" >:: (fun _ -> assert_equal 1 (top_ide delta_start "A"));
"delta_top_ide2" >:: (fun _ -> assert_equal 7 (top_ide delta_start "C"));

"delta_top_empty_ide" >:: (fun _ -> assert_raises (exc_ide_stack_not_found "Z") (fun () -> top_ide delta_start "Z"));
"delta_top_not_found_ide" >:: (fun _ -> assert_raises (exc_ide_stack_not_found "UndeclaredIde") (fun() -> top_ide delta_start "UndeclaredIde"));

"delta_pop_ide" >:: (fun _ -> assert_equal delta_pop_ide_result (pop_ide delta_start "C"));
"delta_pop_empty_ide" >:: (fun _ -> assert_raises (exc_ide_stack_not_found "Z") (fun () -> pop_ide delta_start "Z"));
"delta_pop_not_found_ide" >:: (fun _ -> assert_raises (exc_ide_stack_not_found "UndeclaredIde") (fun() -> top_ide delta_start "UndeclaredIde"));

(* If_stack tests *)
"delta_push_if" >:: (fun _ -> assert_equal delta_push_if_result (push_if delta_start false));
"delta_push_empty_if" >:: (fun _ -> assert_equal delta_push_empty_if_result (push_if delta_empty_if true));

"delta_top_if" >:: (fun _ -> assert_equal true (top_if delta_start));
"delta_top_empty_if" >:: (fun _ -> assert_raises (exc_if_stack_empty) (fun () -> top_if delta_empty_if));

"delta_pop_if" >:: (fun _ -> assert_equal delta_pop_if_result (pop_if delta_start));
"delta_pop_empty_if" >:: (fun _ -> assert_raises (exc_if_stack_empty) (fun () -> pop_if delta_empty_if));

(* While_stack tests - pretty much the same as If_stack, so this can be skipped *)

]
let _ = run_test_tt_main test_delta