
open PlAst ;

module Ast = Camlp4Ast;
(* let prog = Gram.Entry.mk "prog" ;; *)
(* let rule = Gram.Entry.mk "rule" ;; *)
(* let term = Gram.Entry.mk "term" ;; *)
(* let mask = Gram.Entry.mk "mask" ;; *)

(* type rule_or_mask = *)
(*   [ Rule of pred * Loc.t term list * Loc.t term list * Loc.t *)
(*   | Mask of pred * Loc.t arg_mask list * Loc.t]; *)

let list_of_list_option = fun
  [ Some l -> l
  | None -> []];

let term_list _loc ts e =
  List.fold_right
    (fun t c ->
      Comp (PlNames.cons,[t;c],_loc)) ts
    (match e with
    [ Some t -> t
    | None -> Comp (PlNames.nil,[],_loc)]);

let group_rs = ref PlTranslate.group_rs ;

Fan.Syntax.Options.add ("-nogroup", (FanArg.Unit (fun () -> group_rs := PlTranslate.nogroup_rs)),"Don't try to optimally group predicate rules" );
  
{:extend.create|Gram prog rule_or_mask rule body args term mask var arg_mask|};  

{:extend|Gram
prog:
  [ L0 rule_or_mask{rd} ->
    let res = List.fold_left (fun m -> fun
      [ `Rule (p,ts,body,_loc) ->
	let (l1,l2) = try PredMap.find p m with Not_found -> ([],[])
	in PredMap.add p ([(ts,body,_loc)::l1],l2) m
      | `Mask (p,args,_loc) ->
	  let (l1,l2) = try PredMap.find p m with Not_found -> ([],[]) in
          PredMap.add p (l1,[(args,_loc)::l2]) m]) PredMap.empty rd  in
    {:str_item| $(list:PlTranslate.prog_statics _loc res) ; $(list:PlTranslate.prog_rules _loc !group_rs res) |}
  ]
rule_or_mask: [ rule{x} -> `Rule x | mask{x} -> `Mask x ]
rule: [ `LID x; OPT args{t};  OPT body{b};  "." ->
  let t = list_of_list_option t and b = list_of_list_option b
  in ((x,List.length t),t,b,_loc)]
body: [ ":"; "-"; L1 term SEP ","{r} -> r ] 
args: [ "(";  L1 term SEP ","{r}; ")" -> r ]
term:
  { "relop" NA
      [ S{x}; "=";  S{y} -> Comp (PlNames.same,[x;y],_loc)
      | S{x}; "\\="; S{y} -> Comp (PlNames.diff,[x;y],_loc)
      | S{x}; "is"; S{y} -> Comp (PlNames.is,[x;y],_loc)
      | S{x}; "=:="; S{y} -> Comp (PlNames.eq,[x;y],_loc)
      | S{x}; "=\\="; S{y} -> Comp (PlNames.ne,[x;y],_loc)
      | S{x}; "<"; S{y} -> Comp (PlNames.lt,[x;y],_loc)
      | S{x}; "=<"; S{y} -> Comp (PlNames.lte,[x;y],_loc)
      | S{x}; ">"; S{y} -> Comp (PlNames.gt,[x;y],_loc)
      | S{x}; ">="; S{y} -> Comp (PlNames.gte,[x;y],_loc) ]
 "add" LA
      [ S{x}; "+"; S{y} -> Comp (PlNames.add,[x;y],_loc)
      | S{x}; "-"; S{y} -> Comp (PlNames.sub,[x;y],_loc) ]
 "unary minus" NA
      [ "-"; `INT(x,_) -> Integer (-x, _loc)
      | "-"; S{x} -> Comp (PlNames.neg,[x],_loc) ]
 "simple" NA
      [ `LID x; OPT args{t} ->
	(match (x,t) with
	[ (* ("_",None) -> Anon _loc *)
	(* | ("_",Some _) -> FanLoc.raise _loc (Failure "Anonymous with arguments") *)
	 (x,None) -> Comp (x,[],_loc)
	| (x,Some t) -> Comp (x,t,_loc)])
      | "_" -> Anon _loc 
      | "!" -> Comp (PlNames.cut,[],_loc)
      | `UID x -> Var (x,_loc)
      | `INT (x,_) -> Integer (x, _loc)
      | "("; S{t}; ")" -> t
      | "["; L0 S SEP ","{t};  OPT [ "|"; term{t} -> t]{e}; "]" ->
	  term_list _loc t e ]  }

mask: [ "%:"; `LID x; "(";  L1 arg_mask SEP ","{t}; ")" -> ((x, List.length t),t,_loc)] 
var:  [ `UID x -> (x,_loc) ] 
arg_mask:
   [ "+"; OPT var -> ArgClosed _loc
   | "-"; OPT var -> ArgOpen _loc
   | "?";  OPT var -> ArgAny _loc ] |};


Fan.Syntax.Quotation.add_quotation_of_str_item ~name:"plc" ~entry:prog;

