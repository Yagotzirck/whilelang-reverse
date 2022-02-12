open Par_interp
open Par_interp.Annotate
open Par_interp.Interpreter
open Print_state


type input_char =
  | Char of char
  | Char_error of string;;

type input_int =
  | Int of int
  | Int_error of string;;

type cmd =
  | Print_thread_prg of int   (* (<thread_id>) *)
  | Print_state
  | Print_help
  | Forward of int * int      (* (<thread_id>, <num_steps>) *)
  | Reverse of int            (* (<num_steps>) *)
  | Invalid_input of string;; (* (<err_msg>)*)


let rec exec_prg prg_state =
  let read_char() =
    try
      Scanf.scanf " %c" (fun x -> Char x)
    with
      | Scanf.Scan_failure msg -> Char_error ("read_char error: " ^ msg)
  in

  let read_positive_int() =
    try
      let read_value = Scanf.scanf " %d" (fun x -> x) in
      match read_value with
      | value when value >= 0 -> Int value
      | _ -> Int_error "read_positive_int error: Negative value"

    with
      | Scanf.Scan_failure msg -> Int_error ("read_positive_int error: " ^ msg)
    
  in

  let print_help() =
    print_string (
      "List of available commands:\n\n" ^
      "\tt <thread_id>               Print the program contained in the thread with the given ID\n" ^
      "\ts                           Print the program state\n" ^
      "\tf <thread_id> <num_steps>   Perform forward execution on the thread with the given ID for <num_steps> steps\n" ^
      "\tr <num_steps>               Perform reverse execution for <num_steps> steps\n"
    )
  in

  let read_cmd() =
    print_string "\n\nEnter a command (enter \"h\" for a list of available commands): ";
    flush_all();

    match read_char()  with
      (* Print a thread program *)
      | Char 't' -> (
        match read_positive_int() with
          | Int tid -> Print_thread_prg tid
          | Int_error msg -> Invalid_input msg
      )
        
      (* Print the state *)
      | Char 's' -> Print_state

      (* Perform forward execution on the thread with the given ID for <num_steps> steps *)
      | Char 'f' -> (
        match read_positive_int() with
          | Int_error msg -> Invalid_input msg
          | Int tid -> match read_positive_int() with
              | Int_error msg -> Invalid_input msg
              | Int num_steps -> Forward (tid, num_steps)
      )
      
      (* Perform reverse execution for <num_steps> steps *)
      | Char 'r' -> (
        match read_positive_int() with
          | Int num_steps -> Reverse num_steps
          | Int_error msg -> Invalid_input msg
      )

      (* Print help (a list of available commands) *)
      | Char 'h' -> Print_help
      
      (* Unrecognized command *)
      | Char _ -> Invalid_input "Unrecognized command"
      

      (* Scan failure *)
      | Char_error msg -> Invalid_input msg
    
  in


  match read_cmd() with
    | Print_thread_prg tid -> (
        (* Search the given thread ID in the list of running threads*)
        try
          State.get_running_thread tid prg_state |> Thread.get_program |> print_program
        with
            (* If the thread wasn't found in the list of running threads, search in the list of waiting threads *)
          | Thread.Thread_not_found ->
              try
                State.get_waiting_thread tid prg_state |> Thread.get_program |> print_program
              with
                (* If the thread wasn't found in the list of waiting threads either, print an error message *)
                | Thread.Thread_not_found -> Printf.eprintf "\nNo thread with the specified ID was found.\n%!"
    ); exec_prg prg_state

    | Print_state -> print_state prg_state; exec_prg prg_state
    | Print_help -> print_help(); exec_prg prg_state

    | Forward (tid, num_steps) -> (
        try
          sem_prg_fwd_steps tid num_steps prg_state |> exec_prg
        with
          | Thread.Thread_not_found -> Printf.eprintf "\nNo thread with the specified ID was found.\n%!"
    ); exec_prg prg_state

    | Reverse num_steps -> (
        try
          sem_prg_rev_steps num_steps prg_state |> exec_prg
        with
        | Thread.Thread_not_found -> Printf.eprintf "\nNo thread with the specified ID was found.\n%!"
    ); exec_prg prg_state

    | Invalid_input err_msg ->
        Printf.eprintf "\nInvalid input: %s\n%!" err_msg;
        exec_prg prg_state;;

    
let open_arg_file =
  match Sys.argv with
    | [| _; filename |] ->
      open_in filename
    
    | _ ->
      Printf.eprintf "Usage: whilelang_par <source file>\n%!";
      exit 1;;
let main =
  let in_ch = open_arg_file in
  let lexbuf = Lexing.from_channel in_ch in
  let prg_sigma = Lexer_parser.Parser.prg_state Lexer_parser.Lexer.read lexbuf in
  close_in in_ch;
  match prg_sigma with
    | (p, s) ->
      let initial_state = State.init (ann_prg p) s in

      print_string "\n\n\n\n\n";
      print_state initial_state;
      print_string "\n\n";

      exec_prg initial_state;;