%import{
Compile_gram:
  gm
  module_name
  mk_prule
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

  
  
open FAst
open Util

let mk_name (i:FAst.vid) : Gram_def.name =
  let rec aux  x =
    match (x:FAst.vid) with
    | `Lid (_,x) | `Uid(_,x) -> x
    | `Dot(_,`Uid(_,x),xs) -> x ^ "__" ^ aux xs
    | _ -> failwith "internal error in the Grammar extension" in
  {id = i ; tvar = aux i; loc = loc_of i}

let g =
  Gramf.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["("; ")" ; ","; "as"; "|"; "_"; ":";
               "."; ";"; "{"; "}"; "let";"[";"]";
               "SEP";"LEVEL"; "S"; "EOI"; "Lid";"Uid";
               "Ant";"Quot"; "DirQuotation"; "Str";
               "Label"; "Optlabel"; "Chr"; "Int"; "Int32"; "Int64"; "Int64"; "Nativeint";
               "Flo"; 
               "TRY"; "PEEK"; "L0"; "L1"; "First"; "Last";
               "Before"; "After"; "Level"; "LA"; "RA"; "NA"; "+";"*";"?"; "="; "@";
               "Inline"] ();;


let inline_rules : (string, Gram_def.rule list) Hashtbl.t =
  Hashtbl.create 50     

let query_inline (x:string) =
   Hashtblf.find_opt inline_rules x ;;
  
%create{(g:Gramf.t)
   extend_header
   (qualuid : vid Gramf.t)
   (qualid:vid Gramf.t)
   (t_qualid:vid Gramf.t )
   (entry_name : ([`name of Tokenf.name option | `non] * Gram_def.name) Gramf.t )
    position assoc name string rules
    (symbol:Gram_def.psymbol list Gramf.t)
    rule meta_rule rule_list psymbol level level_list
   (entry: Gram_def.entry option Gramf.t)
   extend_body
   unsafe_extend_body
  (simple : Gram_def.psymbol list Gramf.t)
  (single_symbol : Gram_def.symbol Gramf.t)        
}


%extend{(g:Gramf.t)


  (****************************************)        
  (* FIXME bring antiquotation back later*)
  (****************************************)                  
  Inline simple_token :
  [ ("EOI" as v) %{
    let i = hash_variant v in
    let pred = %exp{function
      | `EOI _ -> true
      | _ -> false} in
    let des = %exp{($int':i, `Empty)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v} in
    {text = `Token(_loc,pred,des,des_str);
     styp = `Tok _loc;
     pattern = None;
     bounds = [];
     outer_pattern = None;
   }}
  | ("Lid"|"Uid"|"Str" as v); Str@xloc x %{
    let i = hash_variant v in
    let pred = %exp{function (*BOOTSTRAPPING*)
      | $vrn:v ({txt=$str:x;_}:Tokenf.txt) -> true
      | _ -> false} in
    let des = %exp{($int':i,`A $str:x)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $str:x} in

    {text = `Token(_loc, pred, des,des_str);
     styp = `Tok _loc;
     bounds = [];
     pattern = Some %pat@xloc{$vrn:v ({ txt = $str:x; _ }:Tokenf.txt)}; (* BOOTSTRAPING *)
     outer_pattern = None;}}
  | ("Lid" |"Uid"|"Str" as v)    %{
    let i = hash_variant v in
    let pred = %exp{function
      | $vrn:v _ -> true
      | _ -> false} in
    let des = %exp{($int':i,`Any)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v _} in
    {text = `Token(_loc,pred, des,des_str);
     styp = `Tok _loc;
     pattern = None ;
     bounds = [];
     outer_pattern = None}}
      
  | ("Lid"|"Uid"| "Int" | "Int32" | "Int64"
     | "Nativeint" |"Flo" | "Chr" |"Label" 
     | "Optlabel" |"Str" as v);  Lid@xloc x %{
    let i = hash_variant v in                                 
    let pred =  %exp{function
      | $vrn:v _ -> true
      | _ -> false} in
    let des = %exp{($int':i,`Any)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $lid:x} in
    {text = `Token(_loc, pred,des,des_str);
     styp = `Tok _loc;
     pattern = Some %pat@xloc{$vrn:v ({ txt = $lid:x; _ }:Tokenf.txt) (* BOOTSTRAPING *)};
     bounds = [(xloc,x)];
     outer_pattern = None}}
  (** split opt, introducing an epsilon predicate? *)    
  | ("Lid"|"Uid"|"Str" as v); "@"; Lid@lloc loc ; Lid@xloc x %{
    let i = hash_variant v in
    let pred =  %exp{function
      | $vrn:v _ -> true
      | _ -> false} in
    let des = %exp{($int':i,`Any)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v $lid:x} in
    {text = `Token(_loc, pred,des,des_str);
     styp = `Tok _loc;
     bounds = [(xloc,x);(lloc,loc)];
     pattern = Some %pat@xloc{$vrn:v ({loc = $lid:loc; txt = $lid:x;_}:Tokenf.txt)  (* BOOTSTRAPING*)};
     outer_pattern = None}}



  |  ("Quot"|"DirQuotation" as v) ; Lid@loc x %{
    let i = hash_variant v in                                              
    let pred = %exp{function
      | $vrn:v _ -> true
      | _ -> false} in
    let des = %exp{($int':i,`Any)} in
    let des_str = Gram_pat.to_string %pat'{$vrn:v _} in
    {text = `Token(_loc,pred,des,des_str);
     styp = `Tok _loc;
     bounds = [(loc,x)];
     pattern = Some %pat{$vrn:v $lid:x};
     outer_pattern = None}}
  ]
  Inline simple_symbol:
  [  Str s %{
     {text = `Keyword (_loc,s);
      styp=`Tok _loc;
      pattern= None;
      bounds = [];
      outer_pattern = None}}
  | Str s ; "@"; Lid@xloc i %{
     {text = `Keyword (_loc,s);
      styp = `Tok _loc;
      pattern = Some %pat@xloc{`Key ({loc = $lid:i; _ } : Tokenf.txt ) (*BOOTSTRAPING*)};
      bounds  =[(xloc,i)] ;
      outer_pattern = None;
    }}
  | name as n;  ? level_str as lev %{
    { text = `Nterm (_loc ,n, lev);
      styp = %ctyp'{'$lid{n.tvar}};
      bounds = [];
      pattern = None;
      outer_pattern = None}}
  | "S" %{
    {text = `Self _loc;
     styp = `Self _loc;
     pattern = None;
     bounds = [];
     outer_pattern = None}}]        
  single_symbol :
  [@simple_token 
  |@simple_symbol]
          
  let or_words :
      [ L1 str SEP "|" as v %{  (v,None)  }
      | L1 str SEP "|" as v; "as"; Lid@xloc s %{
          (v , Some (xloc,s)) } ]
  let str :
      [Str s %{(s,_loc)} ]
  let or_strs :
      [ L1 str0 SEP "|" as xs %{(xs,None)}
      | L1  str0 SEP "|"  as xs; "as"; Lid@xloc s %{ (xs,Some (xloc,s))}]
  let str0 :
      [ Str s %{s}]

  simple :
  [ @simple_token %{fun (symbol :Gram_def.symbol) -> [ ({kind = Gram_def.KNormal; symbol}:Gram_def.psymbol) ]}
  | @simple_symbol %{fun (symbol : Gram_def.symbol) -> [({kind = KNormal; symbol}:Gram_def.psymbol)]} 
  |  ("Ant" as v); "("; or_words as ps;",";Lid@xloc s; ")" %{
      let i = hash_variant v in
      let p = %pat'@xloc{$lid:s} in
      match ps with
      | (vs,y) ->
          vs |>
          List.map (fun (x,xloc) ->
           let  z = %pat'@xloc{$str:x} in
           let pred = %exp{function
             | $vrn:v ({ kind = $z; _}:Tokenf.ant) -> true
             | _ -> false} in
           let des = %exp{($int':i,`A $z)} in
           let des_str = Gram_pat.to_string %pat'{$vrn:v $p} in
           
           (** FIXME why $ is allowed to lex here, should
               be disallowed to provide better error message *)
           let (pp,bounds) =
             match y with
             | None -> (%pat{$z},[])
             | Some ((xloc,u) as v) -> (%pat@xloc{( $z as $lid:u)},[v]) in
           ({kind = KNormal;
            symbol = {
             text = `Token(_loc,pred,des,des_str);
             styp= `Tok _loc;
             pattern = Some %pat{$vrn:v (({kind = $pp; _} as $p) :Tokenf.ant)(* BOOTSTRAPPING *)};
             bounds;
             outer_pattern = None }}:Gram_def.psymbol))}

  | "("; or_strs as v; ")" %{
    match v with
    | (vs, None) ->
        vs |>
        List.map
          (fun x ->
            ({kind = KNormal;
              symbol = {
              text = `Keyword(_loc,x);
              styp = `Tok _loc;
              bounds = [];
              pattern = None;
              outer_pattern = None}}:Gram_def.psymbol))
    | (vs, Some ((xloc,v) as b)) ->
        let p = %pat@xloc{$lid:v} in
        vs |>
        List.map
          (fun x ->
            ({kind = KNormal;
             symbol = {
              text = `Keyword (_loc,x);
              styp = `Tok _loc;
              bounds = [b];
              pattern = Some %pat{`Key ({txt=$p;_}:Tokenf.txt)};
              outer_pattern = None}}:Gram_def.psymbol))}


  (* |  ("Uid" as v) ; "("; or_words as p; ")" %{ *)
  (*   match p with *)
  (*   | (vs,None) -> *)
  (*       List.map (fun x -> token_of_simple_pat %pat'{$vrn:v $x}) vs *)
  (*   | (vs,Some x) -> *)
  (*       List.map (fun a -> token_of_simple_pat %pat'{$vrn:v ($a as $lid:x)}) vs} *)    
  ]
  let level_str :  ["Level"; Str  s %{s} ]      
 
  let sep_symbol : [ "SEP"; single_symbol as t %{t}]
  symbol :
  (* be more precise, no recursive grammar? *)
  (*
    transformation 
    L1 Str Sep ";"
    L1 Lid Sep ";"
   *)
      
  [
   (* ("L0"|"L1" as l); ("Lid"|"Uid"| "Int" | "Int32" | "Int64" *)
   (*   | "Nativeint" |"Flo" | "Chr" |"Label"  *)
   (*   | "Optlabel" |"Str" as v); ? sep_symbol as sep %{ *)
   (* let (s:Gram_def.symbol) = *)
   (*   { text = *)
   (*     styp = *)
   (*     pattern = None *)
   (*   } in *)
   (* [mk_symbol ~text ~styp ~pattern:None ] *)
   (* } *)
   ("L0"|"L1" as l) ; single_symbol as s; ?sep_symbol as sep  %{
    let styp = %ctyp'{ ${s.styp} list   } in 
    let text =
      `List(_loc, (if l = "L0" then false else true), s, sep) in
    [{kind =KNormal; (* FIXME More precise, or meaning full warning message *)
      symbol = {text; styp; pattern=None; outer_pattern = None; bounds = [] }}]}
  | "?"; single_symbol as s  %{
    [{kind = KNone;
      symbol = {s with (* pattern=None; *)outer_pattern = None }};
     {kind = KSome;
      symbol = {s with (* pattern = None ; *) outer_pattern = None}
    }]}

  | ("TRY"|"PEEK" as p); single_symbol as s %{
    let v = (_loc, s.text) in
    let text = if p = "TRY" then `Try v else `Peek v  in
    (* FIXME more precise *)
    [{kind = KNormal; symbol= {text;styp=s.styp;pattern=None;outer_pattern = None; bounds = s.bounds}
    }]}
  | simple as p %{ p}   ]
  psymbol :
  [ symbol as ss %{ss}
  | symbol as ss; "as"; Lid@xloc i %{
    List.map (fun (s:Gram_def.psymbol) ->
      {s with symbol = {(s.symbol) with outer_pattern = Some (xloc,i)}}) ss }]
}




%extend{(g:Gramf.t)
  let str : [Str y  %{y}]
  (*****************************)
  (* extend language           *)
  (*****************************)      
  extend_header :
  [ "("; qualid as i; ":"; t_qualid as t; ")" %{
    let old=gm() in 
    let () = module_name := t  in
    (Some i,old)}
  | qualuid as t %{
      let old = gm() in
      let () = module_name :=  t in 
      (None,old)}
  | %{ (None,gm())} ]

  extend_body :
  [ extend_header as rest;   L1 entry  as el %{
    let (gram,old) = rest in
    let items = Listf.filter_map (fun x -> x) el in
    let res = make _loc {items ; gram; safe = true} in 
    let () = module_name := old in
    res}      ]

  (* see [extend_body] *)
  unsafe_extend_body :
  [ extend_header as rest;   L1 entry as el %{
    let (gram,old) = rest in
    let items = Listf.filter_map (fun x ->x ) el in 
    let res = make _loc {items ; gram; safe = false} in 
    let () = module_name := old in
    res}      ]
      
  (* parse qualified [X.X] *)
  qualuid:
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}
  ] 

  qualid:
  [ Uid x ; "."; S as xs %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | Lid i %{ `Lid(_loc,i)}]

  t_qualid:
  [ Uid x; ".";  S as xs %{ %ident'{$uid:x.$xs}}
  | Uid x; "."; Lid "t" %{ `Uid(_loc,x)}] 

  (* stands for the non-terminal  *)
  name: [ qualid as il %{mk_name il}] 

  (* parse entry name, accept a quotation name setup (FIXME)*)
  entry_name:
  [ qualid as il; ?  str  as name %{
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
    (x,mk_name il)}]

  entry:
  [ entry_name as rest; ":";  ? position as pos; level_list as levels
    %{
    let (n,p) = rest in
      begin 
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> Some {name=p; local=false;pos;levels}
      end}
  |  "let" ; entry_name as rest; ":";  ? position as pos; level_list as levels %{
    let (n,p) = rest in
      begin
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> Some {name=p;local=true;pos;levels}
      end}
  | "Inline"; Lid x ; ":"; rule_list as rules %{
    begin
      Hashtbl.add inline_rules x rules;
      None
    end
  }]
  position :
  [ ("First"|"Last"|"Before"|"After"|"Level" as x) %exp{$vrn:x}]

  level_list :
  [ "{"; L1 level  as ll; "}" %{ `Group ll}
  | level as l  %{ `Single l}] (* FIXME L1 does not work here *)

  level :
  [  ?str as  label ;  ?assoc as assoc; rule_list as rules
       %{{label;assoc;rules}} ]

  assoc :
  [ ("LA"|"RA"|"NA" as x) %exp{$vrn:x} ]

      
  rule_list :
  [ "["; "]" %{ []}
  | "["; L1 rule SEP "|" as ruless; "]" %{Listf.concat ruless}]

  rule :
  [ left_rule as prod; ? opt_action as action %{
    let prods = Listf.cross prod in
    List.map
      (fun
        (prod:Gram_def.psymbol list) ->
          mk_prule ~prod ~action) prods}

  | "@"; Lid@xloc x ; ? opt_action as action %{
    let rules =
      match query_inline x with
      | Some x -> x
      | None -> Locf.failf xloc "inline rules %s not found" x in
    (* rules *)
    match action with
    | None -> rules
    | Some a ->
        List.map
          (fun (x:Gram_def.rule) ->
            match x.action with
            | None -> {x with action = Some a}
            | Some b -> {x with action = Some %exp{ $a $b}}) rules 
  }
  ]
  let left_rule :
   [ psymbol as x %{[x]}
   | psymbol as x;";" ;S as xs %{ x::xs }
   |    %{[]}]   
   (* [ L0 psymbol SEP ";"{prod} %{prod}]    *)
  let opt_action :
      [ Quot x %{
        if x.name = Tokenf.empty_name then 
          let expander loc _ s = Parsef.exp loc s  in
          Tokenf.quot_expand expander x
        else
          Ast_quotation.expand x Dyn_tag.exp
      }]

  string :
  [ Str  s  %exp{$str:s}
  | Ant ("", s) %{Tokenf.ant_expand Parsef.exp s}
  ] (*suport antiquot for string*)
  };;


let d = Ns.lang in
begin
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream
    ~name:(d,  "extend") ~entry:extend_body ();
  Ast_quotation.of_exp
    ~lexer:Lex_gram.from_stream
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
(*   [ a_lident as i -> %exp{ $(id:(i:>ident)) } *)
(*   | "("; exp as e; ")" -> e ] *)
(* |};   *)
(* let u : Gram_def.rule = {:rule| *)
(*   a_lident as i -> print_string i *)
(* |};   *)

(* let u : Gram_def.symbol = {:symbol| *)
(*   "x" *)
(* |}; *)









(*
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
       `Token(_loc,match_fun, mdescr,mstr) ;
       styp=`Tok _loc;pattern = Some p_pat}
  | (x,y)::ys ->
      let guard =
          List.fold_left (fun acc (x,y) -> %exp{$acc && ( $x = $y )} )
            %exp{$x = $y} ys  in
      let match_fun = %exp{ function |$po when $guard -> true | _ -> false } in
      {text = `Token(_loc,match_fun,  mdescr, mstr);
       styp = `Tok _loc;
       pattern= Some (Objs.wildcarder#pat po) };;
*)

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
(*   [{Gram_def.text = `Token(_loc, pred,des,des_str); *)
(*     styp = `Tok _loc; *)
(*     pattern}] *)
(* ;; *)




(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_parse.cmo" *)
(* end: *)
