%import{
Compile_gram:
  gm
  module_name
  mk_entry
  mk_level
  mk_rule
  mk_slist
  mk_symbol
  make
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

let mk_name _loc (i:FAst.vid) : Gram_def.name =
  let rec aux  x =
    match (x:FAst.vid) with 
    | `Lid (_,x) | `Uid(_,x) -> x
    | `Dot(_,`Uid(_,x),xs) -> x ^ "__" ^ aux xs
    | _ -> failwith "internal error in the Grammar extension" in
  {exp = (i :> FAst.exp) ; tvar = aux i; loc = _loc}
  
  
open FAst
open Util

let g =
  Fgram.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["("; ")" ; ","; "as"; "|"; "_"; ":";
               "."; ";"; "{"; "}"; "let";"[";"]";
               "SEP";"LEVEL"; "S";
               "EOI"; "Lid";"Uid";
               "Ant";"Quot";
               "DirQuotation"; "Str";
               "Label"; "Optlabel";
               "Chr"; "Int";
               "Int32"; "Int64";
               "Int64"; "Nativeint";
               "Flo"; "OPT";
               "TRY"; "PEEK";
               "L0"; "L1";
               "First"; "Last";
               "Before"; "After";
               "Level"; "LA";
               "RA"; "NA"; "+";"*";"?"; "="; "@";
               "Inline"
             ] ();;


let inline_rules : (string, Gram_def.rule list) Hashtbl.t =
  Hashtbl.create 50     

let query_inline (x:string) =
   Hashtblf.find_opt inline_rules x 
  

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
    Compile_gram.filter_pat_with_captured_variables p_pat in
  let mdescr = (Gram_def.meta_data#data _loc (normalize p)  :> exp) in
  let no_variable = Gram_pat.wildcarder#t p in
  let mstr = Gram_pat.to_string no_variable in
  match ls with
  | [] ->
      let match_fun =
        let v = (no_variable :> pat) in  
        if is_irrefut_pat v  then
          %exp{function | $v -> true }
        else
          %exp{function | $v -> true | _ -> false  } in
      {text =
       `Stoken(_loc,match_fun, mdescr,mstr) ;
       styp=`Tok _loc;pattern = Some p_pat}
  | (x,y)::ys ->
      let guard =
          List.fold_left (fun acc (x,y) -> %exp{$acc && ( $x = $y )} )
            %exp{$x = $y} ys  in
      let match_fun = %exp{ function |$po when $guard -> true | _ -> false } in
      {text = `Stoken(_loc,match_fun,  mdescr, mstr);
       styp = `Tok _loc;
       pattern= Some (Objs.wildcarder#pat po) };;

%create{(g:Fgram.t)
   extend_header
   (qualuid : vid Fgram.t)
   (qualid:vid Fgram.t)
   (t_qualid:vid Fgram.t )
   (entry_name : ([`name of Ftoken.name option | `non] * Gram_def.name) Fgram.t )
    position assoc name string rules
    symbol rule meta_rule rule_list psymbol level level_list
   (entry: Gram_def.entry option Fgram.t)
   extend_body
   unsafe_extend_body

   (simple : Gram_def.symbol list Fgram.t)
}


(* let lid_or_any (x:string option) = *)
(*   match x with  *)
(* (\** *)
(*    Handle *)
(*    {[ *)
(*    Lid@xloc i *)
(*    Lid i *)
(*    Lid _ *)
(*    ]} *)
(*  *\)   *)
(* let make_simple_symbol (_loc:Locf.t) *)
(*     (v:string) *)
(*     (x:string option) *)
(*     (locname:string option) *)
(*     (idloc:Locf.t option) = *)
(*   let pred =  %exp{ *)
(*     function *)
(*       | $vrn:v (_, _) -> true *)
(*       | _ -> false} in *)
(*   let des = %exp{($str:v,`Any )} in *)
(*   let des_str = *)
(*     Gram_pat.to_string @@ *)
(*       (let u = *)
(*         match x with *)
(*         | None -> %pat'{_} *)
(*         | Some x -> %pat'{$lid:x} in *)
(*       %pat'{$vrn:v $u}) in *)
(*   let pattern = *)
(*     let xloc = *)
(*       match idloc with *)
(*       | Some x -> x *)
(*       | None -> _loc in *)
(*     let l = *)
(*       match locname with *)
(*       |None -> %pat'{_} *)
(*       |Some x -> %pat'{$lid:x} in *)
(*     let lx = *)
(*       match x with *)
(*       | None -> %pat'{_} *)
(*       | Some x -> %pat'{$lid:x} in *)
(*     Some %pat@xloc{$vrn:v ($l,$lx)} in *)
(*   [{Gram_def.text = `Stoken(_loc, pred,des,des_str); *)
(*     styp = `Tok _loc; *)
(*     pattern}] *)
(* ;; *)
%extend{(g:Fgram.t)
  (** FIXME bring antiquotation back later*)
  Inline simple_token :
  [ ("EOI" as v) %{
    let pred = %exp{
      function
        | `EOI _ -> true
        | _ -> false 
    } in
    let des = %exp{($str:v, `Empty)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v} in
    (* let pattern = Some %pat{$vrn:v _ } in *)
    [{Gram_def.text = `Stoken(_loc,pred,des,des_str);
      styp = `Tok _loc;
      pattern = None; (* means understore? *)
    }]

  }
  | ("Lid"|"Uid"|"Str" as v); Str@xloc x %{
    let pred = %exp{
                    function
                      | $vrn:v (_, $str:x) -> true
                      | _ -> false} in
    let des = %exp{($str:v,`A $str:x )} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $str:x} in
    let pattern = Some %pat@xloc{$vrn:v (_, $str:x) } in
    [{Gram_def.text = `Stoken(_loc, pred, des,des_str);
      styp = `Tok _loc;
      pattern}]}

  | ("Lid"|"Uid"| "Int"
     | "Int32" | "Int64"
     | "Nativeint" |"Flo" | "Chr" |"Label" 
     | "Optlabel" |"Str" as v); Lid@xloc x %{
    let pred =  %exp{
                    function
                      | $vrn:v (_, _) -> true
                      | _ -> false} in
    let des = %exp{($str:v,`Any )} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $lid:x} in
    let pattern = Some %pat@xloc{$vrn:v (_,$lid:x)} in
    [{Gram_def.text = `Stoken(_loc, pred,des,des_str);
     styp = `Tok _loc;
     pattern}]}
  (** split opt, introducing an epsilon predicate? *)    
  | ("Lid"|"Uid"|"Str" as v); "@"; Lid loc ; Lid@xloc x %{
    let pred =  %exp{
                    function
                      | $vrn:v (_, _) -> true
                      | _ -> false} in
    let des = %exp{($str:v,`Any )} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $lid:x} in
    let pattern = Some %pat@xloc{$vrn:v ($lid:loc,$lid:x)} in
    [{Gram_def.text = `Stoken(_loc, pred,des,des_str);
     styp = `Tok _loc;
     pattern}]}

  | ("Lid" |"Uid"|"Str" as v) ; "_"    %{
    let pred = %exp{function
                    | $vrn:v (_, _) -> true
                    | _ -> false} in
    let des = %exp{($str:v,`Any )} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v _} in
    let pattern = Some %pat{$vrn:v  (_, _) } in
    [{Gram_def.text = `Stoken(_loc,pred, des,des_str);
      styp = `Tok _loc;
      pattern}]}

  |  ("Quot"|"DirQuotation" as v) ; Lid x %{
    let pred = %exp{function
      | $vrn:v _ -> true
      | _ -> false} in
    let des = %exp{($str:v,`Any)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v _} in
    let pattern = Some %pat{$vrn:v $lid:x} in
    [{Gram_def.text = `Stoken(_loc,pred,des,des_str);
      styp = `Tok _loc;
      pattern}]}
  ]
  let or_words :
      [ L1 str SEP "|"{v} %{  (v,None)  }
      | L1 str SEP "|"{v}; "as"; Lid s %{
          (v , Some s) } ]
  let str :
      [Str s %pat'{$str:s} ]

  simple :
  [ @simple_token
  |  ("Ant" as v); "("; or_words{ps};",";Lid@xloc s; ")" %{
      let p = %pat'@xloc{$lid:s} in
      match ps with
      | (vs,None) ->
          List.map (fun x ->

           (* let pred = %exp{function *)
           (*   | $vrn:v ($(x :>pat), _) -> true *)
           (*   | _ -> false} in *)
           (* let des = %exp{($str:v,`A $(x :> exp) )} in *)
           (* let des_str = Gram_pat.to_string %pat'{$vrn:v $p1} in *)
           (* (\** FIXME why $ is allowed to lex here, should *)
           (*     be disallowed to provide better error message *\) *)
           (* let pattern = Some %pat{$vrn:v ($x, $(p1 : Gram_pat.t :>pat))} in *)
           (* {Gram_def.text = `Stoken(_loc,pred,des,des_str); *)
           (*   styp= `Tok _loc; *)
           (*   pattern} *)
           token_of_simple_pat %pat'{`Ant ($x, $p) }
                  ) vs
      | (vs,Some u) ->
          vs  |>
          List.map (fun x -> token_of_simple_pat %pat'{`Ant (($x as $lid:u), $p) }) 
  }
  |  Str s %{[mk_symbol  ~text:(`Skeyword _loc s) ~styp:(`Tok _loc) ~pattern:None]}       
  | "("; or_strs{v}; ")" %{
    match v with
    | (vs, None) ->
        vs |>
        List.map
          (fun x -> mk_symbol ~text:(`Skeyword (_loc,x)) ~styp:(`Tok _loc) ~pattern:None )
    | (vs, Some b) ->
        vs |>
        List.map
          (fun x ->
            mk_symbol ~text:(`Skeyword (_loc,x))
              ~styp:(`Tok _loc) ~pattern:(Some %pat{`Key (_,$lid:b)}) )
          
    }
  (* |  ("Uid" as v) ; "("; or_words{p}; ")" %{ *)
  (*   match p with *)
  (*   | (vs,None) -> *)
  (*       List.map (fun x -> token_of_simple_pat %pat'{$vrn:v $x}) vs *)
  (*   | (vs,Some x) -> *)
  (*       List.map (fun a -> token_of_simple_pat %pat'{$vrn:v ($a as $lid:x)}) vs} *)
  | "S" %{[mk_symbol  ~text:(`Sself _loc)  ~styp:(`Self _loc ) ~pattern:None]}

  |  name{n};  OPT level_str{lev} %{
        [mk_symbol  ~text:(`Snterm (_loc ,n, lev))
          ~styp:(%ctyp'{'$(lid:n.tvar)}) ~pattern:None ]}
  ]
  let or_strs :
      [ L1 str0 SEP "|"{xs} %{(xs,None)}
      | L1  str0 SEP "|" {xs}; "as"; Lid s %{ (xs,Some s)}]
  let str0 :
      [ Str s %{s}]
  let level_str :  ["Level"; Str  s %{s} ]      
 
  let sep_symbol : [ "SEP"; simple{t} %{let [t] =  t in t}]

  symbol : (* be more precise, no recursive grammar? *)
  [ ("L0"|"L1" as l) ; simple{s}; OPT  sep_symbol{sep } %{
    let [s] =  s in
    let styp = %ctyp'{ $(s.styp) list   } in 
    let text = mk_slist _loc (if l = "L0" then false else true) sep s in
    [mk_symbol ~text ~styp ~pattern:None]}
  | "OPT"; simple{s}  %{
    let [s] = s in
    let styp = %ctyp'{$(s.styp) option } in 
    let text = `Sopt (_loc, s.text) in
    [mk_symbol  ~text ~styp ~pattern:None] }
  | ("TRY"|"PEEK" as p); simple{s} %{
    let [s] = s in
    let v = (_loc, s.text) in
    let text = if p = "TRY" then `Stry v else `Speek v  in
    [mk_symbol  ~text ~styp:(s.styp) ~pattern:None] }
  | simple{p} %{ p}

  ]

  let brace_pattern :
      ["{"; Lid@loc i ;"}" %pat'@loc{$lid:i}]

  psymbol :
  [ symbol{ss} ; OPT  brace_pattern {p} %{
    List.map (fun (s:Gram_def.symbol) ->
      match p with
      |Some _ ->
          { s with pattern = (p:pat option)}
      | None -> s) ss }  ] 
      
}




%extend{(g:Fgram.t)
  let str : [Str y  %{y}]
  (*****************************)
  (* extend language           *)
  (*****************************)      
  extend_header :
  [ "("; qualid{i}; ":"; t_qualid{t}; ")" %{
    let old=gm() in 
    let () = module_name := t  in
    (Some i,old)}
  | qualuid{t} %{
      let old = gm() in
      let () = module_name :=  t in 
      (None,old)}
  | %{ (None,gm())} ]

  extend_body :
  [ extend_header{rest};   L1 entry {el} %{
    let (gram,old) = rest in
    let items = Listf.filter_map (fun x -> x) el in
    let res = make _loc {items ; gram; safe = true} in 
    let () = module_name := old in
    res}      ]

  (* see [extend_body] *)
  unsafe_extend_body :
  [ extend_header{rest};   L1 entry {el} %{
    let (gram,old) = rest in
    let items = Listf.filter_map (fun x ->x ) el in 
    let res = make _loc {items ; gram; safe = false} in 
    let () = module_name := old in
    res}      ]
      
  (* parse qualified [X.X] *)
  qualuid:
  [ Uid x; ".";  S{xs}  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ] 

  qualid:
  [ Uid x ; "."; S{xs} %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | Lid i %{ `Lid(_loc,i)}]

  t_qualid:
  [ Uid x; ".";  S{xs} %{ %ident'{$uid:x.$xs}}
  | Uid x; "."; Lid "t" %{ `Uid(_loc,x)}] 

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
        | _ -> Some (mk_entry ~local:false ~name:p ~pos ~levels)
      end}
  |  "let" ;
    entry_name{rest}; ":";  OPT position{pos}; level_list{levels} %{
    let (n,p) = rest in
      begin
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> Some (mk_entry ~local:true ~name:p ~pos ~levels)
      end}
  | "Inline"; Lid x ; ":"; rule_list{rules} %{
    begin
      Hashtbl.add inline_rules x rules;
      None
    end
  }]
  position :
  [ ("First"|"Last"|"Before"|"After"|"Level" as x) %exp{$vrn:x}]

  level_list :
  [ "{"; L1 level {ll}; "}" %{ `Group ll}
  | level {l} %{ `Single l}] (* FIXME L1 does not work here *)

  level :
  [  OPT str {label};  OPT assoc{assoc}; rule_list{rules}
       %{mk_level ~label ~assoc ~rules} ]
  (* FIXME a conflict %extend{Fgram e:  "simple" ["-"; a_FLOAT{s} %{()} ] } *)
  assoc :
  [ ("LA"|"RA"|"NA" as x) %exp{$vrn:x} ]

      
  rule_list :
  [ "["; "]" %{ []}
  | "["; L1 rule SEP "|"{ruless}; "]" %{Listf.concat ruless}]

  rule :
  [ left_rule {prod}; OPT opt_action{action} %{
    let prods = Listf.cross prod in
    List.map (fun prod -> mk_rule ~prod ~action) prods}

  | "@"; Lid x %{
    match query_inline x with
    | Some x -> x
    | None -> Locf.failf _loc "inline rules %s not found" x
  }
  ]
  let left_rule :
   [ psymbol{x} %{[x]}
   | psymbol{x};";" ;S{xs} %{ x::xs }
   |    %{[]}]   
   (* [ L0 psymbol SEP ";"{prod} %{prod}]    *)
  let opt_action :
      [ Quot x %{
        if x.name = Ftoken.empty_name then 
          let expander loc _ s = Fgram.parse_string ~loc Syntaxf.exp s in
          Ftoken.quot_expand expander x
        else
          Ast_quotation.expand x Dyn_tag.exp
      }]

  string :
  [ Str  s  %exp{$str:s}
  | Ant ("", s) %{Parsef.exp _loc s}
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
