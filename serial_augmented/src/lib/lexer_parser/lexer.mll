(* Header
 * Copied literally into the generated lexer.ml
*)
{
        open Parser

        exception SyntaxError of string;;
        let exc_syntax_err num_line ch =
                SyntaxError (Printf.sprintf "Unexpected char at line %d: \'%s\'" num_line ch);;

        let curr_line = ref 1;;
}

(* Identifiers
 * identifiers are named regular expressions, which will be used in the rules section, next.
*)
let white = [' ' '\t' '\r']+
let newline = ['\n']
let digit = ['0'-'9']
let int = '-'? digit+
let letter = ['a'-'z' 'A'-'Z']
let alnum_underscore = ['A'-'Z' 'a'-'z' '0'-'9' '_']
let id = letter alnum_underscore*


(* Rules
 * 'rule' and 'parse' are reserved keywords.
 * When a regex matches, the lexer produces the token specified by its action.
*)

(* NOTE: It's important to keep the 'id' regex at the bottom of the list; otherwise,
 * keywords like 'true' and 'if' would be lexed as variable names rather than as TRUE
 * and IF tokens, respectively.
*)

rule read =
        parse
        | white { read lexbuf }
        | newline { Printf.printf "Source line %d has been parsed\n" !curr_line; incr curr_line; read lexbuf }
        | "true" { TRUE }
        | "false" { FALSE }
        | ">" { GT }
        | "!" { NOT }
        | "&&" { AND }
        | "==" {EQ}
        | "=" { ASSIGN }
        | "+" { PLUS }
        | "-" { MINUS }
        | "(" { LPAREN }
        | ")" { RPAREN }
        | "if" { IF }
        | "then" { THEN }
        | "else" { ELSE }
        | "while" { WHILE }
        | "do" { DO }
        | "skip" { SKIP }
        | ";" { SEMICOLON }
        | "sigmadef" { SIGMADEF }
        | "end" { END }
        | id { ID (Lexing.lexeme lexbuf) }
        | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
        | _ { raise (exc_syntax_err !curr_line (Lexing.lexeme lexbuf) ) }
        | eof { EOF }