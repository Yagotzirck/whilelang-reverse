(* Header
 * Copied as-is in the generated parser.ml file
*)
%{
    open Serial_interp.Ast
    open Serial_interp.Sigma
%}

(* Declarations
 * Tokens' definitions
*)

(* int_expr and constructive assignment tokens *)
%token <int>                    INT
%token <Serial_interp.Ast.ide>  ID
%token                          PLUS
%token                          MINUS

(* bool_expr tokens *)
%token          TRUE
%token          FALSE
%token          NOT
%token          EQ
%token          GT
%token          AND

(* program tokens *)
%token          ASSIGN

%token          IF
%token          THEN
%token          ELSE

%token          WHILE
%token          DO

%token          SIGMADEF

%token          SKIP
%token          SEMICOLON
%token          END

%token          LPAREN
%token          RPAREN

%token          EOF

(* Associativity and precedence (lower to higher *)
%left AND
%left NOT
%left PLUS
%left MINUS


(* Starting point for parsing the language *)
%start < (Serial_interp.Ast.program * Serial_interp.Sigma.sigma) > prg_state


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

prod_bool_expr:
        | TRUE { Ebool true }
        | FALSE { Ebool false }
        | NOT; bexp = prod_bool_expr { Not bexp }
        | iexp1 = prod_int_expr; EQ; iexp2 = prod_int_expr { Eq (iexp1, iexp2) }
        | iexp1 = prod_int_expr; GT; iexp2 = prod_int_expr { Gt (iexp1, iexp2) }
        | bexp1 = prod_bool_expr; AND; bexp2 = prod_bool_expr { And (bexp1, bexp2) }
        | LPAREN; bexp = prod_bool_expr; RPAREN { bexp }
        ;

prod_stmt:
        | SKIP { Skip }
        | x = ID; ASSIGN; e = prod_int_expr; SEMICOLON { Assign (Val x, e) }
        | x = ID; PLUS; ASSIGN; e = prod_int_expr; SEMICOLON { Cadd (Val x, e) }
        | x = ID; MINUS; ASSIGN; e = prod_int_expr; SEMICOLON { Csub (Val x, e) }
        | IF; bexp = prod_bool_expr; THEN; p1 = prod_prg; ELSE; p2 = prod_prg; END { Ifthenelse (bexp, p1, p2) }
        | WHILE; bexp = prod_bool_expr; DO; while_block = prod_prg; END { While (bexp, while_block) }
        ;



prod_prg:
        | (* empty *) { Program_empty }
        | statement = prod_stmt; next_statement = prod_prg { Program (statement, next_statement) }
        ;