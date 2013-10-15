%import{
Gram_def:
  simple_pat
  ;
Gram_gen:
  gm
  grammar_module_name
  text_of_functorial_extend
  exp_delete_rule
  mk_name
  mk_entry
  mk_level
  retype_rule_list_without_patterns
  mk_rule
  check_not_tok
  mk_slist
  mk_symbol
  token_of_simple_pat
  ;
Fsyntax:
  ctyp
  a_lident
  exp
  parse_exp
  ;
Ast_gen:
  sem_of_list
  loc_of
  seq_sem
  tuple_com
  ;
}

open FAst
open Util

%create{Fgram (* FIXME can not ignore Fgram here*)
   (nonterminals: stru Fgram.t)
   (nonterminalsclear:  exp Fgram.t)
   delete_rule_header
   extend_header
   (qualuid : vid Fgram.t)
   (qualid:vid Fgram.t)
   (t_qualid:vid Fgram.t )
   (entry_name : ([`name of Ftoken.name option | `non] * Gram_def.name) Fgram.t )
    entry position assoc name string rules
    symbol rule meta_rule rule_list psymbol level level_list
   (entry: Gram_def.entry Fgram.t)
   (pattern: Gram_def.action_pattern Fgram.t )
   extend_body
   newterminals
   unsafe_extend_body
   delete_rule_body
   simple_exp
   delete_rules } ;;

%extend{
  let ty:
  [ "("; qualid{x} ; ":"; t_qualid{t};")" -> `Dyn(x,t)
  |  qualuid{t} -> `Static t
  | -> `Static (`Uid(_loc,"Fgram")) (** BOOTSTRAP, associated with module [Fgram]*)]    

  nonterminals :
  [ ty {t}; L1 type_entry {ls} ->
    with stru
    let mk =
      match t with
      |`Static t -> let t = (t : vid :> exp ) in %exp{ $t.mk }
      |`Dyn(x,t) ->
          let x = (x : vid :> exp) in
          let t = (t : vid :> exp ) in 
          %exp{$t.mk_dynamic $x }  in   
    sem_of_list
      ( List.map
      (fun (_loc,x,descr,ty) ->
        match (descr,ty) with
        |(Some d,None) ->
            %{ let $lid:x = $mk $str:d }
        | (Some d,Some typ) ->
            %{ let $lid:x : $typ = $mk $str:d }
        |(None,None) ->
            %{ let $lid:x = $mk $str:x  }
        | (None,Some typ) ->
            %{ let $lid:x : $typ = $mk $str:x  }  ) ls) ]

  let str : [`Str y -> y]
      
  let type_entry :
      [ `Lid x  -> (_loc,x,None,None)
      | "("; `Lid x ;`Str y; ")" ->(_loc,x,Some y,None)
      | "(";`Lid x ;`Str y;ctyp{t};  ")" -> (_loc,x,Some y,Some t)
      | "("; `Lid x; ":"; ctyp{t}; OPT str {y};  ")" -> (_loc,x,y,Some t) ]

  newterminals :
  [ "("; qualid{x}; ":";t_qualid{t};")"; L1 type_entry {ls}
    ->
      let mk  =
        let x = (x : vid :> exp) in
        %exp{$id:t.mk_dynamic $x }  in
      sem_of_list (* FIXME improve *)
        (%stru{ let $((x:>pat)) = $id:t.create_lexer ~annot:"" ~keywords:[] ()} ::
         ( List.map
            (fun (_loc,x,descr,ty) ->
              match (descr,ty) with
              |(Some d,None) ->
                  %stru{ let $lid:x = $mk $str:d }
              | (Some d,Some typ) ->
                  %stru{ let $lid:x : $typ = $mk $str:d }
              |(None,None) ->
                  %stru{ let $lid:x = $mk $str:x  }
              | (None,Some typ) ->
                  %stru{ let $lid:x : $typ = $mk $str:x  }  ) ls)) ]
  nonterminalsclear :
  [ qualuid{t}; L1 a_lident {ls} ->
    let rest = List.map (fun (x:alident) ->
      let  x = (x:alident :> exp) in 
      let _loc = loc_of x in
      let t = (t:vid :> exp) in
      %exp{ $t.clear $x }) ls in
    seq_sem rest ]

  extend_header :
  [ "("; qualid{i}; ":"; t_qualid{t}; ")" -> 
    let old=gm() in 
    let () = grammar_module_name := t  in
    (Some i,old)
  | qualuid{t}  ->
      let old = gm() in
      let () = grammar_module_name :=  t in 
      (None,old)
  | -> (None,gm())]

  extend_body :
  [ extend_header{(gram,old)};   L1 entry {el} -> 
    let res = text_of_functorial_extend _loc  gram  el in 
    let () = grammar_module_name := old in
    res      ]
  (* see [extend_body] *)

  unsafe_extend_body :
  [ extend_header{(gram,old)};   L1 entry {el} -> 
    let res = text_of_functorial_extend ~safe:false _loc  gram  el in 
    let () = grammar_module_name := old in
    res      ]
  (*for side effets, parser action *)
  delete_rule_header:
  [ qualuid{g} -> let old = gm () in let () = grammar_module_name := g  in old  ]

  delete_rule_body:
  [ delete_rule_header{old};  L1 delete_rules {es} ->
    begin
      grammar_module_name := old;
      seq_sem es
    end]

  delete_rules:
  [ name{n} ;":"; "["; L1  psymbols SEP "|" {sls}; "]" ->
    exp_delete_rule _loc n sls ]
  let psymbols:
  [ L0 psymbol SEP ";"{sl} -> sl  ] 
  (* parse qualified [X.X] *)
  qualuid:
  [ `Uid x; ".";  S{xs} -> %ident'{$uid:x.$xs}
  | `Uid x -> `Uid(_loc,x) ] 

  qualid:
  [ `Uid x ; "."; S{xs} -> `Dot(_loc,`Uid(_loc,x),xs)
  | `Lid i -> `Lid(_loc,i)]

  t_qualid:
  [ `Uid x; ".";  S{xs} -> %ident'{$uid:x.$xs}
  | `Uid x; "."; `Lid "t" -> `Uid(_loc,x) ] 

  (* stands for the non-terminal  *)
  name:[ qualid{il} -> mk_name _loc il] 

  (* parse entry name, accept a quotation name setup (FIXME)*)
  entry_name:
  [ qualid{il}; OPT  str {name} -> 
    (match name with
    | Some x ->
        let old = !Ast_quotation.default in
        begin 
          match Ast_quotation.resolve_name (`Sub [], x)
          with
          | None -> Locf.failf _loc "DDSL `%s' not resolved" x 
          | Some x -> (Ast_quotation.default:= Some x; `name old)
        end
  | None -> `non, mk_name _loc il)]

  entry:
  [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
    ->
      begin 
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> mk_entry ~local:false ~name:p ~pos ~levels
      end
  |  "let"; entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels} ->
      begin
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> mk_entry ~local:true ~name:p ~pos ~levels
      end  ]
  position :
  [ `Uid ("First"|"Last" as x ) ->   %exp{ $vrn:x }
  | `Uid ("Before" | "After" | "Level" as x) ; string{n} ->
      %exp{ $vrn:x  $n }
  | `Uid x ->
      failwithf
        "%s is not the right position:(First|Last) or (Before|After|Level)" x]

  level_list :
  [ "{"; L1 level {ll}; "}" -> `Group ll
  | level {l} -> `Single l] (* FIXME L1 does not work here *)

  level :
  [  OPT str {label};  OPT assoc{assoc}; rule_list{rules} ->
    mk_level ~label ~assoc ~rules ]
  (* FIXME a conflict %extend{Fgram e:  "simple" ["-"; a_FLOAT{s} -> () ] } *)



  assoc :
  [ `Uid ("LA"|"RA"|"NA" as x) ->     %exp{ $vrn:x } 
  | `Uid x -> failwithf "%s is not a correct associativity:(LA|RA|NA)" x  ]

      
  rule_list :
  [ "["; "]" -> []
  | "["; L1 rule SEP "|"{rules}; "]" ->
    retype_rule_list_without_patterns _loc rules ]

  rule :
  [ L0 psymbol SEP ";"{prod}; OPT opt_action{action} ->
    mk_rule ~prod ~action ]
  let opt_action : ["->"; exp{act}-> act]

  pattern :
  [ `Lid i -> %pat'{ $lid:i }
  | "_" -> %pat'{ _ }
  | "("; pattern{p}; ")" -> p
  | "("; pattern{p1}; ","; L1 S SEP ","{ps}; ")"-> tuple_com (p1::ps) ]
      
  let brace_pattern : ["{";pattern{p};"}"->p]

  psymbol :
  [ symbol{s} ; OPT  brace_pattern {p} ->
    match p with
    |Some _ ->
        { s with pattern = (p:  Gram_def.action_pattern option :>  pat option) }
    | None -> s  ] 

  let sep_symbol : [`Uid "SEP"; symbol{t}->t]
  let level_str :  [`Uid "Level"; `Str  s -> s ]
  symbol:
  [ `Uid ("L0"| "L1" as x); S{s}; OPT  sep_symbol{sep } ->
    let () = check_not_tok s in
    let styp = %ctyp'{ $(s.styp) list   } in 
    let text = mk_slist _loc
        (match x with
        |"L0" -> false | "L1" -> true
        | _ -> failwithf "only (L0|L1) allowed here") sep s in
    mk_symbol ~text ~styp ~pattern:None
  |`Uid "OPT"; S{s}  ->
    let () = check_not_tok s in
    let styp = %ctyp'{  $(s.styp) option } in 
    let text = `Sopt _loc s.text in
    mk_symbol  ~text ~styp ~pattern:None
  |`Uid "TRY"; S{s} ->
      let text = `Stry _loc s.text in
      mk_symbol  ~text ~styp:(s.styp) ~pattern:None
  | `Uid "PEEK"; S{s} ->
      let text = `Speek _loc s.text in
      mk_symbol ~text ~styp:(s.styp) ~pattern:None
  | `Uid "S" ->
      mk_symbol  ~text:(`Sself _loc)  ~styp:(`Self _loc ) ~pattern:None
  | simple_pat{p} ->
      token_of_simple_pat _loc p 
  | `Str s ->
        mk_symbol  ~text:(`Skeyword _loc s) ~styp:(`Tok _loc) ~pattern:None
  | name{n};  OPT level_str{lev} ->
        mk_symbol  ~text:(`Snterm _loc n lev)
          ~styp:( %ctyp'{'$(lid:n.tvar)} ) ~pattern:None (* {' *)
  | "("; S{s}; ")" -> s ]

  string:
  [ `Str  s -> %exp{$str:s}
  | `Ant ("", s) -> parse_exp _loc s ] (*suport antiquot for string*)

  simple_exp:
  [ a_lident{i} -> (i : alident :>exp) 
  | "("; exp{e}; ")" -> e ]  };;


let d = Ns.lang in
begin
  Ast_quotation.of_exp
    ~name:(d,  "extend") ~entry:extend_body ();
  Ast_quotation.of_exp
    ~name:(d,  "unsafe_extend") ~entry:unsafe_extend_body ();
  Ast_quotation.of_stru
    ~name:(d,"create") ~entry:nonterminals ();
  Ast_quotation.of_stru
    ~name:(d,"new") ~entry:newterminals ();
  Ast_quotation.of_exp
    ~name:(d,"clear") ~entry:nonterminalsclear ();
end;;


(*
  Ast_quotation.add_quotation
  (d,"rule") rule
  ~mexp:Gram_def.Exp.meta_rule
  ~mpat:Gram_def.Pat.meta_rule
  ~exp_filter:(fun x-> (x :ep :>exp))
  ~pat_filter:(fun x->(x : ep :> pat));

  Ast_quotation.add_quotation
  (d,"entry") entry
  ~mexp:Gram_def.Expr.meta_entry
  ~mpat:Gram_def.Patt.meta_entry
  ~exp_filter:(fun x-> (x :ep :> exp))
  ~pat_filter:(fun x-> (x :ep :> pat));

  Ast_quotation.add_quotation
  (d,"level") level
  ~mexp:Gram_def.Expr.meta_level
  ~mpat:Gram_def.Patt.meta_level
  ~exp_filter:(fun x-> (x :ep :> exp))
  ~pat_filter:(fun x-> (x :ep :> pat));

  Ast_quotation.add_quotation
  (d,"symbol") psymbol
  ~mexp:Gram_def.Expr.meta_symbol
  ~mpat:Gram_def.Patt.meta_symbol
  ~exp_filter:(fun x -> (x :ep :>exp))
  ~pat_filter:(fun x->  (x :ep :>pat));
 *)  






(* let _loc = Locf.ghost; *)
(* let u : FanGrammar.entry= {:entry| *)
(*   simple_exp: *)
(*   [ a_lident{i} -> %exp{ $(id:(i:>ident)) } *)
(*   | "("; exp{e}; ")" -> e ] *)
(* |};   *)
(* let u : Gram_def.rule = {:rule| *)
(*   a_lident{i} -> print_string i *)
(* |};   *)

(* let u : Gram_def.symbol = {:symbol| *)
(*   "x" *)
(* |}; *)

















(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_grammar.cmo" *)
(* end: *)
