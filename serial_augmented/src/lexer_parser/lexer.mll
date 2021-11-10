(* Header
 * Copied literally into the generated lexer.ml
*)
{
        open Parser
}

(* Identifiers
 * identifiers are named regular expressions, which will be used in the rules section, next.
*)
let white = [' ' '\t']+
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
        | "true" { TRUE }
        | "false" { FALSE }
        | ">" { GT }
        | "!" { NOT }
        | "&&" { AND }
        | "=" { EQ }
        | "+" { PLUS }
        | "-" { MINUS }
        | "(" { LPAREN }
        | ")" { RPAREN }
        | "if" { IF }
        | "then" { THEN }
        | "else" { ELSE }
        | "while" { WHILE }
        | "do" { DO }
        | "SKIP" { SKIP }
        | ";" { SEMICOLON }
        | "sigmadef" { SIGMADEF }
        | "end" { END }
        | id { ID (Lexing.lexeme lexbuf) }
        | int { INT (int_of_string (Lexing.lexeme lexbuf)) }
        | eof { EOF }