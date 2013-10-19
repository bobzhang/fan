%import{
Gram_gen:
  gm
  grammar_module_name
  text_of_functorial_extend
  mk_name
  mk_entry
  mk_level
  retype_rule_list_without_patterns
  mk_rule
  check_not_tok
  mk_slist
  mk_symbol
  (* token_of_simple_pat *)
  ;
Fan_ops:
  is_irrefut_pat
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

let g =
  Fgram.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["`";"("; ")" ; ","; "as"; "|"; "_"; ":";
               "."; ";"; "{"; "}"; "let";"[";"]";
               "SEP";"LEVEL"; "S";
               "EOI"; "Lid";"Uid";
               "Ant";"Quot";
               "DirQuotation";
               "Str";
               "Label";
               "Optlabel";
               "Chr";
               "Int";
               "Int32";
               "Int64";
               "Int64";
               "Nativeint";
               "Flo"
             ]
    ();;


%create{(g:Fgram.t)
   extend_header
   (qualuid : vid Fgram.t)
   (qualid:vid Fgram.t)
   (t_qualid:vid Fgram.t )
   (entry_name : ([`name of Ftoken.name option | `non] * Gram_def.name) Fgram.t )
    entry position assoc name string rules
    symbol rule meta_rule rule_list psymbol level level_list
   (entry: Gram_def.entry Fgram.t)
   extend_body
   unsafe_extend_body

   (simple : Gram_pat.t list Fgram.t)
}

%extend{(g:Fgram.t)
  (** FIXME bring antiquotation back later*)        
  simple :
  [ "`"; "EOI" %{[%pat'{`EOI}]}
  | "`"; "Lid"; `Str v %{[%pat'{`Lid $str:v}]}
  | "`"; "Uid"; `Str v %{[%pat'{`Uid $str:v}]}      
  | "`"; "Lid" ; `Lid x %{[%pat'{`Lid $lid:x }]}
  | "`"; "Uid" ; `Lid x %{[%pat'{`Uid $lid:x }]}
  | "`"; "Quot"; `Lid x %{[%pat'{`Quot $lid:x }]}
  | "`"; "Label"; `Lid x %{[%pat'{`Label $lid:x }]}      
  | "`"; "DirQuotation"; `Lid x %{[%pat'{`DirQuotation $lid:x}]}
  | "`"; "Optlabel"; `Lid x %{[%pat'{`Optlabel $lid:x}]}      
  | "`"; "Str"; `Lid x %{[%pat'{`Str $lid:x}]}
  | "`"; "Chr"; `Lid x %{[%pat'{`Chr $lid:x}]}
  | "`"; "Int"; `Lid x %{[%pat'{`Int $lid:x}]}
  | "`"; "Int32"; `Lid x %{[%pat'{`Int32 $lid:x}]}
  | "`"; "Int64"; `Lid x %{[%pat'{`Int64 $lid:x}]}      
  | "`"; "Nativeint"; `Lid x %{[%pat'{`Nativeint $lid:x}]}
  | "`"; "Flo"; `Lid x %{[%pat'{`Flo $lid:x}]}      
  | "`"; "Lid" ; "_"    %{[%pat'{`Lid _}]}
  | "`"; "Uid"; "_" %{[%pat'{`Uid _}]}
  | "`"; "Ant"; "("; or_words{p};",";lid{p1}; ")" %{
    match p with
    | (v,None) ->
        List.map (fun x -> %pat'{`Ant ($x, $p1) }) v
    | (v,Some u) ->
        List.map (fun x -> %pat'{`Ant (($x as $lid:u), $p1) }) v 
  }
  | "`"; "Uid"; "("; or_words{p}; ")" %{
    match p with
    | (v,None) ->
        List.map (fun x -> %pat'{`Uid $x}) v
    | (v,Some x) ->
        List.map (fun a -> %pat'{`Uid ($a as $lid:x)}) v 
  }
  ]
  let or_words :
      [ L1 str SEP "|"{v} %{  (v,None)  }
      | L1 str SEP "|"{v}; "as"; `Lid s %{
          (v , Some s) } ]
  let str :
      [`Str s %pat'{$str:s} ]
  let lid :
      [`Lid s %pat'{$lid:s}]
}
  

let normalize (x:Gram_pat.t) : Gram_def.data =
  match x with
  | %pat'{$vrn:x} ->  (x, `Empty)
    
  | %pat'{$vrn:x $str:s} | %pat'{$vrn:x ($str:s as $_ )} -> 
      (x,  `A s)

  | %pat'{$vrn:x $lid:_ }
  | %pat'{$vrn:x _}-> 
      (x, `Any)
  | %pat'{$vrn:x ($lid:_, $_)} -> 
      (x, `Any)
    
  | %pat'{$vrn:x (($str:s as $_), $_) }
  | %pat'{$vrn:x ($str:s, $_) }  ->
      (x, `A s)

  | _ -> failwithf "normalize %s" @@ Gram_pat.to_string x ;;

let token_of_simple_pat  (p:Gram_pat.t) : Gram_def.symbol  =
  let _loc = loc_of p in
  let p_pat = (p:Gram_pat.t :> pat) in 
  let (po,ls) =
    Gram_gen.filter_pat_with_captured_variables p_pat in
  (* let v = object (\* to be improved *\) *)
  (*   inherit FanAstN.meta *)
  (*   method! ant _loc x = *)
  (*     match x with *)
  (*     | `Ant(_loc,{FanUtil.content=x;_}) -> *)
  (*         %ep{ `Str $lid:x } *)
  (* end in *)
  let mdescr = (Gram_def.meta_data#data _loc (normalize p)  :> exp) in
  let no_variable = Gram_pat.wildcarder#t p in
  (* let mdescr = *)
  (*   (v#pat _loc (Objs.strip_pat (no_variable :> pat)) :> exp) in *)
  let mstr = Gram_pat.to_string no_variable in

  match ls with
  | [] ->
      let match_fun =
        let v = (no_variable :> pat) in  
        if is_irrefut_pat v  then
          %exp{function | $v -> true }
        else
          %exp{function | $v -> true | _ -> false  } in
      {text =  `Stok(_loc,match_fun, mdescr,mstr) ;
       styp=`Tok _loc;pattern = Some p_pat}
  | (x,y)::ys ->
      let guard =
          List.fold_left (fun acc (x,y) -> %exp{$acc && ( $x = $y )} )
            %exp{$x = $y} ys  in
      let match_fun = %exp{ function |$po when $guard -> true | _ -> false } in
      {text = `Stok(_loc,match_fun,  mdescr, mstr);
       styp = `Tok _loc;
       pattern= Some (Objs.wildcarder#pat po) }


let simple_meta =
  Gentry.map ~name:"simple_meta" (List.map token_of_simple_pat) simple
;;

%extend{(g:Fgram.t)
  let str : [`Str y  %{y}]
  (*****************************)
  (* extend language           *)
  (*****************************)      
  extend_header :
  [ "("; qualid{i}; ":"; t_qualid{t}; ")" %{
    let old=gm() in 
    let () = grammar_module_name := t  in
    (Some i,old)}
  | qualuid{t} %{
      let old = gm() in
      let () = grammar_module_name :=  t in 
      (None,old)}
  | %{ (None,gm())} ]

  extend_body :
  [ extend_header{rest};   L1 entry {el} %{
    let (gram,old) = rest in
    let res = text_of_functorial_extend _loc  gram  el in 
    let () = grammar_module_name := old in
    res}      ]

  (* see [extend_body] *)
  unsafe_extend_body :
  [ extend_header{rest};   L1 entry {el} %{
    let (gram,old) = rest in
    let res = text_of_functorial_extend ~safe:false _loc  gram  el in 
    let () = grammar_module_name := old in
    res}      ]
      
  (* parse qualified [X.X] *)
  qualuid:
  [ `Uid x; ".";  S{xs}  %ident'{$uid:x.$xs}
  | `Uid x %{ `Uid(_loc,x)}
  ] 

  qualid:
  [ `Uid x ; "."; S{xs} %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | `Lid i %{ `Lid(_loc,i)}]

  t_qualid:
  [ `Uid x; ".";  S{xs} %{ %ident'{$uid:x.$xs}}
  | `Uid x; "."; `Lid "t" %{ `Uid(_loc,x)}] 

  (* stands for the non-terminal  *)
  name: [ qualid{il} %{mk_name _loc il}] 

  (* parse entry name, accept a quotation name setup (FIXME)*)
  entry_name:
  [ qualid{il}; OPT  str {name} %{
    let x =
      match name with
      | Some x ->
          let old = !Ast_quotation.default in
          begin 
            match Ast_quotation.resolve_name (`Sub [], x)
            with
            | None -> Locf.failf _loc "DDSL `%s' not resolved" x 
            | Some x -> (Ast_quotation.default:= Some x; `name old)
          end
      | None -> `non in
    (x,mk_name _loc il)}]

  entry:
  [ entry_name{rest}; ":";  OPT position{pos}; level_list{levels}
    %{
    let (n,p) = rest in
      begin 
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> mk_entry ~local:false ~name:p ~pos ~levels
      end}
  |  "let"; entry_name{rest}; ":";  OPT position{pos}; level_list{levels} %{
    let (n,p) = rest in
      begin
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> mk_entry ~local:true ~name:p ~pos ~levels
      end}  ]
  position :
  [ `Uid ("First"|"Last" as x )    %exp{ $vrn:x }
  | `Uid ("Before" | "After" | "Level" as x) ; string{n} %exp{ $vrn:x  $n }
  | `Uid x %{failwithf
               "%s is not the right position:(First|Last) or (Before|After|Level)" x}]

  level_list :
  [ "{"; L1 level {ll}; "}" %{ `Group ll}
  | level {l} %{ `Single l}] (* FIXME L1 does not work here *)

  level :
  [  OPT str {label};  OPT assoc{assoc}; rule_list{rules}
       %{mk_level ~label ~assoc ~rules} ]
  (* FIXME a conflict %extend{Fgram e:  "simple" ["-"; a_FLOAT{s} %{()} ] } *)
  assoc :
  [ `Uid ("LA"|"RA"|"NA" as x)  %exp{ $vrn:x }
  | `Uid x %{failwithf "%s is not a correct associativity:(LA|RA|NA)" x}  ]

      
  rule_list :
  [ "["; "]" %{ []}
  | "["; L1 rule SEP "|"{ruless}; "]" %{
    let rules = Listf.concat ruless in
    retype_rule_list_without_patterns _loc rules}]

  rule :
  [ L0 psymbol SEP ";"{prod}; OPT opt_action{action} %{
    let prods = Listf.cross prod in
    List.map (fun prod -> mk_rule ~prod ~action) prods
    (* [mk_rule ~prod ~action]  *)} ]

  let opt_action :
      [ `Quot x %{
        if x.name = Ftoken.empty_name then 
          let expander loc _ s = Fgram.parse_string ~loc Syntaxf.exp s in
          Ftoken.quot_expand expander x
        else
          Ast_quotation.expand x Dyn_tag.exp
      }]

  let tmp_lid : [`Lid i %pat'{$lid:i}]
   (* FIXME a new entry introduced here only for precise location *)    
  let brace_pattern : ["{"; tmp_lid{p};"}"  %{p}]

  psymbol :
  [ symbol{ss} ; OPT  brace_pattern {p} %{
    List.map (fun (s:Gram_def.symbol) ->
      match p with
      |Some _ ->
          { s with pattern = (p:  Gram_def.action_pattern option :>  pat option) }
      | None -> s) ss }  ] 

  let sep_symbol : [ "SEP"; symbol{t} %{let [t] =  t in t}]
  let level_str :  [`Uid "Level"; `Str  s %{s} ]

  symbol : (* be more precise, no recursive grammar? *)
  [ `Uid ("L0"| "L1" as x); S{s}; OPT  sep_symbol{sep } %{
    let [s] =  s in

    let () =  check_not_tok s in (* s should be singleton here actually*)
    let styp = %ctyp'{ $(s.styp) list   } in 
    let text = mk_slist _loc
        (match x with
        |"L0" -> false | "L1" -> true
        | _ -> failwithf "only (L0|L1) allowed here") sep s in
    [mk_symbol ~text ~styp ~pattern:None]}
  |`Uid "OPT"; S{s}  %{
    let [s] = s in
    let () = check_not_tok s in
    let styp = %ctyp'{$(s.styp) option } in 
    let text = `Sopt _loc s.text in
    [mk_symbol  ~text ~styp ~pattern:None] }
  |`Uid "TRY"; S{s} %{
    let [s] = s in 
    let text = `Stry (_loc, s.text) in
    [mk_symbol  ~text ~styp:(s.styp) ~pattern:None] }
  | `Uid "PEEK"; S{s} %{
    let [s] = s in
    let text = `Speek(_loc, s.text) in
    [mk_symbol ~text ~styp:(s.styp) ~pattern:None]}
  | "S" %{
      [mk_symbol  ~text:(`Sself _loc)  ~styp:(`Self _loc ) ~pattern:None]}
  | simple_meta{p} %{ p}
  | `Str s %{[mk_symbol  ~text:(`Skeyword _loc s) ~styp:(`Tok _loc) ~pattern:None]}
  | name{n};  OPT level_str{lev} %{
        [mk_symbol  ~text:(`Snterm (_loc ,n, lev))
          ~styp:(%ctyp'{'$(lid:n.tvar)}) ~pattern:None ]}
   | "("; S{s}; ")" %{s} ]

   string :
  [ `Str  s  %exp{$str:s}
  | `Ant ("", s) %{Parsef.exp _loc s}
  ] (*suport antiquot for string*)
  };;


let d = Ns.lang in
begin
  Ast_quotation.of_exp
    ~name:(d,  "extend") ~entry:extend_body ();
  Ast_quotation.of_exp
    ~name:(d,  "unsafe_extend") ~entry:unsafe_extend_body ();

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
