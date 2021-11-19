open Serial_interp
open Serial_interp.Augment
open Serial_interp.Interpreter
open Print_state

let rec exec_prg prg_state =
  let rec read_steps() =
    print_string "Steps (0 to finish): ";
    match read_int_opt() with
  | Some steps -> steps
  | None ->
      print_endline "The inserted value isn't a valid integer.";
      read_steps()
  in
  print_string "\n\n\n\n\n";
  print_state prg_state;
  print_string "\n\n\n\n\n";

  let steps = read_steps() in
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
    | (p, s) -> exec_prg (State.init (aug_prg p) s);;