open Serial_interp.Ast
open Serial_interp.Ast_aug
open Serial_interp.Sigma
open Serial_interp.Delta
open Serial_interp.State 

open Printf


let print_program prg =
  let print_stmt_assign e1 e2 =
    match e1, e2 with
      | Val i, Val j ->       printf "Assign (%s, %s)" i j
      | Val i, Eint n ->      printf "Assign (%s, %d)" i n
      | Val i, Add (_, _) ->  printf "Assign (%s, Add())" i
      | Val i, Sub (_, _) ->  printf "Assign (%s, Sub())" i
      | _, _ ->               print_string "Assign (invalid format: assignment to non-var)"
  in
  
  let print_stmt_cadd e1 e2 =
    match e1, e2 with
      | Val i, Val j ->       printf "Cadd (%s, %s)" i j
      | Val i, Eint n ->      printf "Cadd (%s, %d)" i n
      | Val i, Add (_, _) ->  printf "Cadd (%s, Add())" i
      | Val i, Sub (_, _) ->  printf "Cadd (%s, Sub())" i
      | _, _ ->               print_string "Cadd (invalid format: assignment to non-var)"
  in
  
  let print_stmt_csub e1 e2 =
    match e1, e2 with
      | Val i, Val j ->       printf "Csub (%s, %s)" i j
      | Val i, Eint n ->      printf "Csub (%s, %d)" i n
      | Val i, Add (_, _) ->  printf "Csub (%s, Add())" i 
      | Val i, Sub (_, _) ->  printf "Csub (%s, Sub())" i
      | _, _ ->               print_string "Csub (invalid format: assignment to non-var)"
  in
  

  let print_stmt = function
    | Program_start -> print_string "Program_start"
    | Program_end -> print_string "Program_end"
    | Skip -> print_string "Skip"
    | Assign (e1, e2) -> print_stmt_assign e1 e2
    | Cadd (e1, e2) -> print_stmt_cadd e1 e2
    | Csub (e1, e2) -> print_stmt_csub e1 e2
    | Ifthenelse (_, _, _) ->  print_string "Ifthenelse ()"
    | If_start _ -> print_string "If_start ()"
    | If_end (_, _) -> print_string "If_end ()"
    | While (_, _) -> print_string "While ()"
    | While_start (_) -> print_string "While_start ()"
    | While_end (_, _) -> print_string "While_end ()"
  in

  let rec print_prg_list = function
    | [] -> ()
    | h :: t -> print_stmt h; print_newline (); print_prg_list t
  in

  match prg with
    Program_aug (prev_stmts, curr_stmt, next_stmts) ->
      print_prg_list (List.rev prev_stmts);
      print_string ">>>>>>>>>> "; print_stmt curr_stmt; print_string " <<<<<<<<<<\n";
      print_prg_list next_stmts;;

(************************************************************** Sigma *************************************************************)

let rec print_sigma = function
      | Sigma_empty -> ()
      | Sigma ( (var_id, value), sigma_next) ->
          printf "%s :\t\t%d\n" var_id value;
          print_sigma sigma_next;;

(************************************************************** Delta *************************************************************)

let print_delta delta =

  (* Ide_stacks *)
  let rec print_ide_stacks ide_stks =
    let print_ide_stack ide stk =
      let rec print_stack = function
        | [] -> print_char '\n'
        | h::t -> printf "%-4d\t" h; print_stack t
      in
      printf "%s :\t\t" ide; print_stack stk

  in
  match ide_stks with
    | Ide_stacks ([]) -> print_char '\n'
    | Ide_stacks ((var_id, var_stk) :: t ) ->
        print_ide_stack var_id var_stk;
        print_ide_stacks (Ide_stacks t)
  
  in

  (* Auxiliary function for if and while stacks *)
  let rec print_b_list = function
    | [] -> print_char '\n'
    | h :: t ->
        print_string (if h then "T " else "F " );
        print_b_list t

  in


  (* If_stack *)
  let print_if_stack = function
      | If_stack (l) -> print_b_list l

  in

          (* While_stack *)
  let print_while_stack = function
  | While_stack (l) -> print_b_list l

  in

  match delta with
    Delta (ide_stks, if_stk, while_stk) ->
      print_endline "---------- Ide_stacks ----------\n";
      print_ide_stacks ide_stks;

      print_endline "---------- If_stack ----------\n";
      print_if_stack if_stk;

      print_endline "---------- While_stack ----------\n";
      print_while_stack while_stk;;



let print_state = function
  | State (prg, sigma, delta) ->
    print_endline "========================= Current program state =========================\n";

      print_endline "==================== Program ====================\n";
      print_program prg;
      print_newline();

      print_endline "==================== Sigma ====================\n";
      print_sigma sigma;
      print_newline();

      print_endline "==================== Delta ====================\n";
      print_delta delta;
      print_endline "=========================================================================\n";;