open StdMeta;
{:fans|derive(MetaExpr);|};
{:ocaml|
type expr =
  [= `Number of float
  | `Variable of string
  | `Binary of (char * expr * expr)
  | `Call of (string * array expr) ]
and proto = [= `Prototype of (string * array string) ]
and func = [= `Function of (proto * expr) ];
  |};

__MetaExpr__;
let g = Gram.create_gram ();
{:create|
  (g:Gram.t)
  (expr: Gram.t expr)
  (proto: Gram.t proto)
  (func: Gram.t func)
|};

  
{:extend| (g:Gram.t)  
expr:
  {"+"
   [S{a};"+";S{b} -> `Binary ('+',a,b)
   |S{a};"-";S{b} -> `Binary ('-',a,b)]
   "*"  
   [S{a};"*";S{b} -> `Binary ('*',a,b)
   |S{a};"/";S{b} -> `Binary ('/',a,b)]
   "call"
   [ `Lid x; "(";L0 expr SEP "," {ls};")" -> `Call(x,Array.of_list ls) ]  
   "simple"
   [`Flo(x,_) -> `Number x
   |`Lid x -> `Variable x 
   |"(";S{x};")" -> x ]}
  
proto:
  [ OPT "extern"; `Lid x; "("; L0 [`Lid x -> x ] SEP ","{ls}; ")"
    ->  `Prototype(x,Array.of_list ls) ]
func:
  [ "def"; `Lid x; "("; L0 [`Lid x -> x ] SEP "," {xs} ;")" ; expr{e} ->
    `Function(`Prototype(x,Array.of_list xs),e) ]
|};

let d = `Absolute ["Fan";"Llvm"];

let () =
  AstQuotation.add_quotation (d,"func") func
    ~mexpr:meta_func
    ~mpatt:meta_func
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter
    ;  
