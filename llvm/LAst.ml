type expr =
  [= `Number of float
  | `Variable of string
  | `Binary of (char * expr * expr)
  | `Call of (string * array expr) ]
and proto = [= `Prototype of (string * array string) ]
and func = [= `Function of (proto * expr) ];

let g = Gram.create_gram ();
{:extend.create|
(g:Gram.t)
  (expr: Gram.t expr)
  (proto: Gram.t proto)
  (func: Gram.t func)
  test
  
|};

  
{:extend|(g:Gram.t)  
expr:
  {"+"
   [S{a};"+";S{b} -> `Binary ('+',a,b)
   |S{a};"-";S{b} -> `Binary ('-',a,b)]
   "*"  
   [S{a};"*";S{b} -> `Binary ('*',a,b)
   |S{a};"/";S{b} -> `Binary ('/',a,b)]
   "call"
   [ `LID x; L0 expr SEP "," {ls} -> `Call(x,Array.of_list ls) ]  
   "simple"
   [`FLO(x,_) -> `Number x
   |`LID x -> `Variable x 
   |"(";S{x};")" -> x ]}
  
proto:
  [ OPT "extern"; `LID x; "("; L0 [`LID x -> x ] SEP ","{ls}; ")" ->  `Prototype(x,Array.of_list ls) ]
func:
  ["def"; proto{x}; expr{e} -> `Function(x,e) ]
test:
  [OPT "extern"; `LID y -> print_endline y ]
|};


Gram.parse_string test FanLoc.string_loc "extern x y"  ;
  (* Gram.parse_string test FanLoc.string_loc "def extern x(z,y)  x + y + z"; *)
(* {:extend|(LAst.g:Gram.t) LAst.expr: Level "simple"[`LID x -> `Variable x ] |}; *)
