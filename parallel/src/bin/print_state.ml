open Par_interp
open Par_interp.Ast
open Par_interp.Ast_ann
open Par_interp.Thread
open Par_interp.Sigma
open Par_interp.Delta
open Par_interp.State 

open Printf


let print_program prg =
  let print_stmt_stk stmt_stk =
    let rec print_stmt_stk_inner = function
      | [] -> ()
      | h :: [] -> print_int h
      | h :: t  -> printf "%d; " h; print_stmt_stk_inner t
    in
    print_string " ["; print_stmt_stk_inner stmt_stk; print_char ']'
  in

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
    | Par_prg_start -> print_string "Par_prg_start"
    | Par_prg_end -> print_string "Par_prg_end"
    | Skip stmt_stk -> print_string "Skip"; print_stmt_stk stmt_stk
    | Assign (e1, e2, stmt_stk) -> print_stmt_assign e1 e2; print_stmt_stk stmt_stk
    | Cadd (e1, e2, stmt_stk) -> print_stmt_cadd e1 e2; print_stmt_stk stmt_stk
    | Csub (e1, e2, stmt_stk) -> print_stmt_csub e1 e2; print_stmt_stk stmt_stk
    | Par (_, _, num_chld_done, stmt_stk) -> printf "Par (prg1, prg2, %d)" num_chld_done; print_stmt_stk stmt_stk
  in

  let rec print_prg_list = function
    | [] -> ()
    | h :: t -> print_stmt h; print_newline (); print_prg_list t
  in

  match prg with
    Program_ann (prev_stmts, curr_stmt, next_stmts) ->
      print_prg_list (List.rev prev_stmts);
      print_string ">>>>>>>>>> "; print_stmt curr_stmt; print_string " <<<<<<<<<<\n";
      print_prg_list next_stmts;;


(************************************************************* Threads ************************************************************)

let rec print_threads thread_list = 
  let branch_to_str = function
    | Program.Left -> "Left"
    | Program.Right -> "Right"
    | Program.Root -> "Root"
  in

  match thread_list with
    | [] -> ()
    | h :: t -> printf "\t(%3d, %3d, %s)\n" (get_tid h) (get_ptid h) (branch_to_str (get_child_branch h));
      print_threads t;;

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

  
  match delta with
    Delta (ide_stks) ->
      print_endline "---------- Ide_stacks ----------\n";
      print_ide_stacks ide_stks;;



let print_state = function
  | State (t_running, t_waiting, num_curr_stmt, num_curr_thread, sigma, delta) ->
    print_endline "========================= Current program state =========================\n";

      printf "\tnum_curr_stmt:\t\t%d\n" num_curr_stmt;
      printf "\tnum_curr_thread:\t%d\n" num_curr_thread;


      print_endline "==================== Running threads (<id>, <ptid>, <branch>) ====================\n";
      print_threads t_running;
      print_newline();

      print_endline "==================== Waiting threads (<id>, <ptid>, <branch>) ====================\n";
      print_threads t_waiting;
      print_newline();

      print_endline "==================== Sigma ====================\n";
      print_sigma sigma;
      print_newline();

      print_endline "==================== Delta ====================\n";
      print_delta delta;
      print_endline "=========================================================================\n";;