(* Header
 * Copied as-is in the generated parser.ml file
*)
%{
    open Par_interp.Ast
    open Par_interp.Sigma
%}

(* Declarations
 * Tokens' definitions
*)

(* int_expr and constructive assignment tokens *)
%token <int>                    INT
%token <Par_interp.Ast.ide>     ID
%token                          PLUS
%token                          MINUS

(* program tokens *)
%token          ASSIGN

%token          SIGMADEF

%token          PAR

%token          SKIP
%token          SEMICOLON
%token          END

%token          LCURL
%token          RCURL

%token          LPAREN
%token          RPAREN

%token          EOF

(* Associativity and precedence (lower to higher *)
%left PLUS
%left MINUS


(* Starting point for parsing the language *)
%start < (Par_interp.Ast.program * Par_interp.Sigma.sigma) > prg_state


%% (* End of declarations section *)


(* Rules
 * Production rules that resemble BNF (':' instead of '::=')
*)

(* Format:

name:
        | production1 { action1 }
        | production2 { action2 }
        | ...
        ;
*)

prg_state:
        | SIGMADEF; s = prod_sigma; END; p = prod_prg; EOF { (p, s) }
        ;

prod_sigma:
        | (* empty *) { Sigma_empty }
        | x = ID; ASSIGN; i = INT; SEMICOLON; next_var = prod_sigma { Sigma ((x, i), next_var) }
        ;


prod_int_expr:
        | x = ID { Val x }
        | i = INT { Eint i }
        | e1 = prod_int_expr; PLUS; e2 = prod_int_expr { Add (e1, e2) }
        | e1 = prod_int_expr; MINUS; e2 = prod_int_expr { Sub (e1, e2) }
        | LPAREN; e = prod_int_expr; RPAREN { e }
        ;


prod_stmt:
        | SKIP { Skip }
        | x = ID; ASSIGN; e = prod_int_expr; SEMICOLON { Assign (Val x, e) }
        | x = ID; PLUS; ASSIGN; e = prod_int_expr; SEMICOLON { Cadd (Val x, e) }
        | x = ID; MINUS; ASSIGN; e = prod_int_expr; SEMICOLON { Csub (Val x, e) }
        | LCURL; p1 = prod_prg; RCURL; PAR; LCURL; p2 = prod_prg;  RCURL { Par (p1, p2) }
        ;


prod_prg:
        | (* empty *) { Program_empty }
        | statement = prod_stmt; next_statement = prod_prg { Program (statement, next_statement) }
        ;