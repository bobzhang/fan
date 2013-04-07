%{
  open Expr

  let type_of_string = function
    | "unit" -> `Unit
    | "int" -> `Int
    | "float" -> `Float
    | s -> invalid_arg("type_of_string \""^s^"\"")
%}

%token LET REC IN
%token PIPE PLUS MINUS TIMES DIVIDE CONS
%token IF THEN ELSE
%token COLON COMMA OPEN CLOSE
%token LT LE EQ NE GE GT
%token SEMI SEMISEMI EOF
%token <char> CHAR
%token <string> INT
%token <string> FLOAT
%token <Expr.cmp> CMP
%token <string> IDENT

%start toplevel
%type <Expr.toplevel option> toplevel

%left SEMISEMI
%left LET IN
%left SEMI
%left IF
%left ELSE
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
%nonassoc prec_simple
%nonassoc OPEN CHAR INT FLOAT IDENT

%%

ty_times_list:
| ty TIMES ty { [$3; $1] }
| ty_times_list TIMES ty { $3::$1 }
;

ty:
| OPEN ty CLOSE { $2 }
| IDENT { type_of_string $1 }
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
| IDENT { Var $1 }
;

expr:
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
| LET patt EQ expr IN expr { LetIn($2, $4, $6) }
| IF expr THEN expr ELSE expr { If($2, $4, $6) }
;

toplevel:
| EOF { None }
| expr SEMISEMI { Some(Expr $1) }
| LET REC IDENT OPEN patt COLON ty CLOSE COLON ty EQ expr SEMISEMI
      { Some(Defun($3, $5, $7, $10, $12)) }
;
