open FanAst;
{:fans|derive(MetaExpr);|};
{:ocaml|
type expr =
  [= `Number of float
  | `Variable of string
  | `Binary of (char * expr * expr)
  | `Call of (string * array expr)
  | `If of (expr * expr * expr)
  | `For of (string * expr * expr * option expr * expr )
  | `Var of (array (string * option expr) * expr)
  | `Sem of (expr * expr) ]
and proto = [= `Prototype of (string * array string) ]
and func = [= `Function of (proto * expr) ];
 
type top = [= proto | func ]      
and tops = list top     
; |};

__MetaExpr__;
let g = Gram.create_gram ();
{:create|
  (g:Gram.t)
  (expr: Gram.t expr)
  (proto: Gram.t proto)
  (func: Gram.t func)
  (top: Gram.t top)
  (tops: Gram.t tops)
|};


  
{:extend| (g:Gram.t)
  local:asn;
  expr:
  {"top"
   ["if" ; S{a}; "then"; S{b}; "else"; S{c} -> `If (a,b,c)
   |"for"; `Lid i;"="; S{a}; ","; S{b}; ","; S{c};"in";S{d} ->
       `For(i,a,b,Some c,d)
   |"for"; `Lid i;"=";S{a};","; S{b};"in";S{d} ->
       `For(i,a,b,None,d)
   | "var"; L1 asn SEP ","{xs};"in"; S{e} -> `Var (Array.of_list xs,e)
   | S{e0}; ";"; S{e1} -> `Sem (e0,e1) ]
   "+"
   [S{a};"+";S{b} -> `Binary ('+',a,b)
   |S{a};"-";S{b} -> `Binary ('-',a,b)]
   "*"  
   [S{a};"*";S{b} -> `Binary ('*',a,b)
   |S{a};"/";S{b} -> `Binary ('/',a,b)]
   "<"
   [S{a};"<";S{b} -> `Binary ('<',a,b)]  
   "call"
   [ `Lid x; "(";L0 expr SEP "," {ls};")" -> `Call(x,Array.of_list ls) ]  
   "simple"
   [`Flo(x,_) -> `Number x
   |`Lid x -> `Variable x 
   |"(";S{x};")" -> x ]}
  asn:
  [`Lid i;"=";expr{e} -> (i,Some e)  |`Lid i -> (i,None) ]

  proto:
  [ OPT "extern"; `Lid x; "("; L0 [`Lid x -> x ] SEP ","{ls}; ")"
    ->  `Prototype(x,Array.of_list ls) ]
  func:
  [ "def"; `Lid x; "("; L0 [`Lid x -> x ] SEP "," {xs} ;")" ; expr{e} ->
    `Function(`Prototype(x,Array.of_list xs),e) ]
  top:
  [ proto{x} -> (x:>top) | func{x} -> (x:>top) ]

  tops:
  [ L1 top  SEP ";" {xs} -> xs ]
|};

let d = `Absolute ["Fan";"Llvm"];

begin
  AstQuotation.add_quotation (d,"func") func
    ~mexpr:meta_func
    ~mpatt:meta_func
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter ;
  AstQuotation.add_quotation (d,"expr") expr
    ~mexpr:meta_expr
    ~mpatt:meta_expr
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter ;
  AstQuotation.add_quotation (d,"proto") proto
    ~mexpr:meta_proto
    ~mpatt:meta_proto
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter ;

  AstQuotation.add_quotation (d,"top") top
    ~mexpr:meta_top
    ~mpatt:meta_top
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter ;

  AstQuotation.add_quotation (d,"tops") tops
    ~mexpr:meta_tops
    ~mpatt:meta_tops
    ~expr_filter:Syntax.expr_filter
    ~patt_filter:Syntax.patt_filter ;
  
end;
