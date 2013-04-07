%{
  open Printf
  open Expr

  let type_of_string = function
    | "unit" -> `Unit
    | "bool" -> `Bool
    | "int" -> `Int
    | "float" -> `Float
    | s -> invalid_arg("type_of_string \""^s^"\"")

  let named = function
    | "array", [ty] -> `Array ty
    | "array", tys ->
	invalid_arg(sprintf "Found %d type arguments when one was expected" (List.length tys))
    | name, args -> invalid_arg "Only arrays are implemented"
%}

%token LET REC IN
%token PIPE PLUS MINUS TIMES DIVIDE CONS
%token IF THEN ELSE
%token COLON COMMA OPEN CLOSE SQOPEN SQCLOSE
%token LT LE EQ NE GE GT
%token DOT LEFTARROW SEMI SEMISEMI
%token <char> CHAR
%token <string> INT
%token <string> FLOAT
%token <Expr.cmp> CMP
%token <string> IDENT
%token <string> STRING

%start toplevel
%type <Expr.toplevel> toplevel

%left SEMISEMI
%right prec_let
%right SEMI
%nonassoc below_if
%right IF THEN
%right ELSE
%right prec_ifelse
%right LEFTARROW
%left COLON
%left LT LE EQ NE GE GT
%right CONS
%left PIPE
%nonassoc below_COMMA
%left COMMA
%left PLUS
%left MINUS
%nonassoc below_TIMES
%left TIMES
%left DIVIDE
%left prec_uminus
%left prec_apply
%left DOT
%nonassoc prec_simple
%nonassoc OPEN CHAR INT FLOAT IDENT STRING

%%

ty_times_list:
| ty TIMES ty { [$3; $1] }
| ty_times_list TIMES ty { $3::$1 }
;

ty_comma_list:
| ty COMMA ty { [$3; $1] }
| ty_times_list COMMA ty { $3::$1 }
;

simple_ty:
| OPEN ty CLOSE { $2 }
| IDENT { type_of_string $1 }
;

ty:
| simple_ty %prec prec_simple { $1 }
| ty IDENT %prec prec_apply { named($2, [$1]) }
| OPEN ty_comma_list CLOSE IDENT %prec prec_apply { named($4, List.rev $2) }
| ty_times_list %prec below_TIMES { `Tuple(List.rev $1) }
;

patt_comma_list:
| patt COMMA patt { [$3; $1] }
| patt_comma_list COMMA patt { $3::$1 }
;

patt:
| OPEN patt CLOSE { $2 }
| IDENT { PVar $1 }
| patt_comma_list %prec below_COMMA { PTup(List.rev $1) }
;

expr_comma_list:
| expr COMMA expr { [$3; $1] }
| expr_comma_list COMMA expr { $3::$1 }
;

simple_expr:
| OPEN CLOSE { Unit }
| OPEN expr CLOSE { $2 }
| CHAR { Int(int_of_char $1) }
| INT { Int(int_of_string $1) }
| FLOAT { Float(float_of_string $1) }
| STRING { String $1 }
| IDENT { Var($1) }
;

expr:
| IF expr THEN expr { If($2, $4, Unit) }
| expr_noifthen %prec below_if { $1 }
;

expr_noifthen:
| simple_expr %prec prec_simple { $1 }
| simple_expr expr %prec prec_apply { Apply($1, $2) }
| expr_comma_list %prec below_COMMA { Tuple(List.rev $1) }
| MINUS expr %prec prec_uminus { UnArith(`Neg, $2) }
| expr PLUS expr { BinArith(`Add, $1, $3) }
| expr MINUS expr { BinArith(`Sub, $1, $3) }
| expr TIMES expr { BinArith(`Mul, $1, $3) }
| expr DIVIDE expr { BinArith(`Div, $1, $3) }
| expr LT expr { Cmp(`Lt, $1, $3) }
| expr LE expr { Cmp(`Le, $1, $3) }
| expr EQ expr { Cmp(`Eq, $1, $3) }
| expr NE expr { Cmp(`Ne, $1, $3) }
| expr GE expr { Cmp(`Ge, $1, $3) }
| expr GT expr { Cmp(`Gt, $1, $3) }
| expr SEMI expr { LetIn(PVar "_", $1, $3) }
| LET patt EQ expr IN expr %prec prec_let { LetIn($2, $4, $6) }
| IF expr THEN expr_noifthen ELSE expr { If($2, $4, $6) }
| expr DOT SQOPEN expr SQCLOSE { ArrayGet($1, $4) }
| expr DOT SQOPEN expr SQCLOSE LEFTARROW expr { ArraySet($1, $4, $7) }
| expr DOT OPEN expr CLOSE { ArrayGet($1, $4) }
| expr DOT OPEN expr CLOSE LEFTARROW expr { ArraySet($1, $4, $7) }
;

toplevel:
| expr SEMISEMI { Expr $1 }
| LET REC IDENT OPEN patt COLON ty CLOSE COLON ty EQ expr SEMISEMI
    %prec prec_let
    { Defun($3, $5, $7, $10, $12) }
;
