%token <string> ID
%token AND OR NOT OP CL EOF

%left OR AND
%nonassoc NOT

%{
  (* open Lexer *)
  open Formula
%}

%start parse_formula
%type <Formula.formula> parse_formula formula_expr

%%

%public formula_expr:
| ID                                    { Var ($1) }
| NOT formula_expr                      { Not ($2) }
| formula_expr AND formula_expr         { And ($1, $3) }
| f = formula_expr OR g = formula_expr  { Or (f, g) }
| OP f = formula_expr CL                { f }

parse_formula:
| formula_expr EOF                      { $1 }
    
