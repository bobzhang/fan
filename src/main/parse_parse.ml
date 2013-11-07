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



type outer_symbol = {
    psymbols : Gram_def.symbol Gram_def.decorate list ;
    outer_pattern : Gram_def.locid option;
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
               "."; ";"; "{"; "}"; "[";"]";
               "SEP";"LEVEL"; "S"; "EOI"; "Lid";"Uid";
               "Ant";"Quot"; "DirQuotation"; "Str";
               "Label"; "Optlabel"; "Chr"; "Int"; "Int32"; "Int64"; "Int64"; "Nativeint";
               "Flo"; "Pre"; "Inf";
               "TRY"; "PEEK";
               "L0"; "L1"; "First"; "Last";
               "Before"; "After"; "Level";
               "LA"; "RA"; "NA"; "+";"*";"?"; "=";
               "@";
               "Inline";
               "Local"] ();;


let inline_rules : (string, Gram_def.rule list) Hashtbl.t =
  Hashtbl.create 50     

let query_inline (x:string) =
   Hashtblf.find_opt inline_rules x ;;
  
%create{(g:Gramf.t)
   extend_header
   (left_rule : outer_symbol list  Gramf.t)
   (qualuid : vid Gramf.t)
   (qualid:vid Gramf.t)
   (t_qualid:vid Gramf.t )
   (entry_name : ([`name of Tokenf.name option | `non] * Gram_def.name) Gramf.t )
    position assoc name string rules
    (symbol:Gram_def.symbol Gram_def.decorate list Gramf.t)
    rule meta_rule rule_list
   (psymbol: outer_symbol  Gramf.t)
   level level_list
   (entry: Gram_def.entry option Gramf.t)
   extend_body
   unsafe_extend_body
  (simple : Gram_def.symbol Gram_def.decorate list Gramf.t)
  (single_symbol : Gram_def.symbol Gramf.t)        
}


%extend{(g:Gramf.t)
  (****************************************)        
  (* FIXME bring antiquotation back later*)
  (****************************************)                  
  simple_token @Inline :
  [ ("EOI" as v) %{
    {text = `Token(_loc,%exp{
                   ({pred = (function
                     | $vrn:v _ -> true
                     | _ -> false) ;
                     descr = { tag = $vrn:v ; word = Empty; tag_name = $str:v }}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.txt};
     pattern = (* None *) [];
     bounds = []
     }}
  | ("Lid"|"Uid"|"Str" as v); Str x %{
    {text = `Token(_loc,
                   %exp{({ pred = (function
                             | $vrn:v ({txt=$str:x;_}:Tokenf.txt) -> true
                             | _ -> false) ;
                           descr = {tag = $vrn:v; word = A $str:x; tag_name = $str:v }}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.txt};
     bounds = [];
     pattern = []}}
  | ("Lid"|"Uid"| "Int" | "Int32" | "Int64"
     | "Nativeint" |"Flo" | "Chr" |"Label" 
     | "Optlabel" |"Str" |"Pre" as v);  ? Lid@xloc x %{
    let (pattern,bounds)  =
      match (x,xloc) with
      | (Some x, Some xloc) -> 
          ([((xloc,x),Some "txt") ] , [(xloc,x)])
      | _ -> ([], [])in
    {text = `Token(_loc,
                   %exp{({pred =
                          (function
                            | $vrn:v _ -> true
                            | _ -> false);
                          descr = { tag = $vrn:v ; word = Any; tag_name = $str:v }}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.txt};
     pattern;
     bounds ;
     }}
  (** split opt, introducing an epsilon predicate? *)    
  | ("Lid"|"Uid"|"Str" | "Pre" as v); "@"; Lid@lloc loc ; Lid@xloc x %{
    {text = `Token(_loc,
                   %exp{({pred =
                          (function
                            | $vrn:v _ -> true
                            | _ -> false);
                          descr = {tag = $vrn:v; word = Any; tag_name = $str:v}}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.txt};
     bounds = [(xloc,x);(lloc,loc)];
     pattern = [((lloc,loc),Some "loc");((xloc,x),Some "txt")]}}
  | ("Lid"|"Uid"|"Str" | "Pre" as v); "@"; Lid@lloc loc ; Str x %{
    {text = `Token(_loc,
                   %exp{({pred =
                          (function
                            | $vrn:v ({txt=$str:x;_}:Tokenf.txt) -> true
                            | _ -> false);
                          descr = {tag = $vrn:v; word = Any; tag_name = $str:v}}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.txt};
     bounds = [(lloc,loc)];
     pattern = [((lloc,loc),Some "loc")]}} 
  |  ("Quot"|"DirQuotation" as v) ; Lid@loc x %{
    {text = `Token(_loc,
                   %exp{({pred =
                          (function
                            | $vrn:v _ -> true
                            | _ -> false);
                          descr =
                          {tag = $vrn:v; word = Any; tag_name = $str:v}}:Tokenf.pattern)});
     styp = %ctyp'{Tokenf.quot};
     bounds = [(loc,x)];
     pattern = [((loc,x),None)] (* FIXME *)
   }}
  | ("Inf" as v); "("; Int level; ","; Lid@xloc x ; ")" %{
     { text =
       `Token(_loc,
              %exp{({pred =
                     (function
                       | $vrn:v ({ level = $int:level; _}:Tokenf.op) -> true
                       | _ -> false);
                     descr = {tag = $vrn:v; word = Level $int:level; tag_name = $str:v}}
                      :Tokenf.pattern)});
       styp = %ctyp'{Tokenf.op};
       bounds = [(xloc,x)];
       pattern = [((xloc,x),Some "txt")];
     }}
                          
  | ("Inf" as v); "@"; Lid@lloc l; "("; Int level;","; Lid@xloc x ; ")" %{
     { text = `Token(_loc,
                     %exp{({pred =
                            (function
                              | $vrn:v ({ level = $int:level; _}:Tokenf.op) -> true
                              | _ -> false);
                            descr =  {tag = $vrn:v; word = Level $int:level; tag_name = $str:v}}:Tokenf.pattern)});
       styp = %ctyp'{Tokenf.op};
       bounds = [(xloc,x); (lloc,l)];
       pattern = [((lloc,l),Some "loc"); ((xloc,x),Some "txt")]}}
  ]
  simple_symbol@Inline:
  [  Str s %{
     {text = `Keyword (_loc,s);
      styp= %ctyp'{Tokenf.txt};
      pattern= [];
      bounds = []}}
  | Str s ; "@"; Lid@xloc i %{
     {text = `Keyword (_loc,s);
      styp = %ctyp'{Tokenf.txt};
      pattern =
      [((xloc,i),Some "loc")];
      (* Some %pat@xloc{({loc = $lid:i; _ } : Tokenf.txt )}; *)
      bounds  =[(xloc,i)] ;}}
  | name as n;  ? level_str as lev %{
    { text = `Nterm (_loc ,n, lev);
      styp = %ctyp'{'$lid{n.tvar}};
      bounds = [];
      pattern = []}}
  | "S" %{
    {text = `Self _loc;
     styp = `Self _loc;
     pattern = [];
     bounds = []}}]        
  single_symbol :
  [@simple_token 
  |@simple_symbol]
  
  or_strs@Local :
      [ L1  Str SEP "|" as xs %{(xs,None,None)}
      | L1  Str SEP "|" as xs; "as"; Lid@xloc s %{ (xs, None, Some (xloc,s))}
      | L1  Str SEP "|" as xs ; "@"; Lid@lloc l; "as"; Lid@xloc s %{(xs, Some (lloc,l), Some(xloc,s))} ]

  simple :
  [ @simple_token %{fun (txt :Gram_def.symbol) ->
    [ ({kind = Gram_def.KNormal; txt}:Gram_def.symbol Gram_def.decorate) ]}
  | @simple_symbol %{fun (txt : Gram_def.symbol) ->
      [({kind = KNormal; txt }:Gram_def.symbol Gram_def.decorate)]} 
  |  ("Ant" as v); "("; or_strs as ps;",";Lid@xloc s; ")" %{
      match ps with
      | (vs,loc,y) ->
          vs |>
          List.map
            (fun (x:Tokenf.txt) ->
           (** FIXME why $ is allowed to lex here, should
               be disallowed to provide better error message *)
            let (pattern,bounds) =
              match (loc, y) with
              | (None, None) ->
                  ([((xloc,s),None)],[(xloc,s)])
              | (Some(lloc,ll),None) ->
                  ([((lloc,ll),Some "loc");
                    ((xloc,s),None)] ,[(xloc,s);(lloc,ll)])
              | (None ,Some v) ->
                  ([(v,Some "kind"); ((xloc,s),None)],
                   [(xloc,s);v])
              | (Some(lloc,ll),Some v) ->
                  (
                   [(v,Some "kind");
                    ((lloc,ll),Some "loc");
                    ((xloc,s),None)], [(xloc,s);(lloc,ll);v]) in
            ({kind = KNormal;
              txt = {
              text = `Token(_loc,
                            %exp{({pred =
                                   (function
                                     | $vrn:v ({ kind = $str{x.txt}; _}:Tokenf.ant) -> true
                                     | _ -> false);
                                   descr = {tag = $vrn:v; word = A $str{x.txt}; tag_name = $str:v}}:Tokenf.pattern)});
              styp= %ctyp'{Tokenf.ant};
              pattern ;
              bounds}}:Gram_def.symbol Gram_def.decorate))}

  | "("; or_strs as v; ")" %{
    match v with
    | (vs,loc, None) -> (* ("a"|"b"@loc)*)
        vs |>
        List.map
          (fun (x:Tokenf.txt) ->
            let (bounds,pattern) =
              match loc with
              | Some(loc,l) ->
                  ([(loc,l)],[((loc,l),Some "loc")])
              | None -> ([],[]) in
            ({kind = KNormal;
              txt =
              {text = `Keyword(x.loc,x.txt);
               styp = %ctyp'{Tokenf.txt};
               bounds ;
               pattern}}:Gram_def.symbol Gram_def.decorate))
    | (vs, loc, Some  b) -> (* ("a"|"b"|"c"@loc as v)*)
        (* let p = %pat@xloc{$lid:v} in *)
        let (bounds,pattern) =
          match loc with
          | None -> ([b], [(b,Some "txt")])
          | Some(loc,l) ->
              (* let lp = %pat@loc{$lid:l} in  *)
              ([(loc,l);b],[((loc,l),Some "loc");(b,Some "txt")]) in
        vs |>
        List.map
          (fun (x:Tokenf.txt) ->
            ({kind = KNormal;
              txt  =
              {text = `Keyword (x.loc,x.txt);
               styp = %ctyp'{Tokenf.txt};
               bounds ;
               pattern}}:Gram_def.symbol Gram_def.decorate))}

  ]
  level_str@Local :  ["Level"; Str  s %{s} ]      
 
  sep_symbol@Local : [ "SEP"; single_symbol as t %{t}]
  symbol :
  (* be more precise, no recursive grammar? *)
  [("L0"|"L1" as l) ; single_symbol as s; ?sep_symbol as sep  %{
    let styp = %ctyp'{ ${s.styp} list   } in
    let text =
      `List(_loc, (if l = "L0" then false else true), s, sep) in
    [{kind =KNormal; (* FIXME More precise, or meaning full warning message *)
      txt = {text; styp; pattern= [];  bounds = [] }}]}
  | "?"; single_symbol as s  %{
    [{kind = KNone;
      txt =  s };
     {kind = KSome;
      txt = s}]}
  | "?"; "["; left_rule ; "]"%{  assert false}
  | ("TRY"|"PEEK" as p); single_symbol as s %{
    let v = (_loc, s.text) in
    let text = if p = "TRY" then `Try v else `Peek v  in
    (* FIXME more precise *)
    [{ kind = KNormal; txt = {text;styp=s.styp;pattern= []; bounds = s.bounds}}]}
  | simple as p %{ p}   ]
  psymbol :
  [ symbol as ss %{ {psymbols = ss; outer_pattern = None}}
  | symbol as ss; "as"; Lid@xloc i %{
                             {psymbols = ss; outer_pattern = Some(xloc,i)} }]}




%extend{(g:Gramf.t)

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

  extend@Inline:
 [ extend_header as rest;   L1 entry  as el %{
   fun safe -> 
   let (gram,old) = rest in
   let items = Listf.filter_map (fun x -> x) el in
   let res = make _loc {items ; gram; safe} in 
   let () = module_name := old in
   res}      ]
  extend_body :
  [@extend %{fun f -> f true}]
  unsafe_extend_body :
  [@extend %{fun f -> f false}]
      
  (* parse qualified [X.X] *)
  qualuid:
  [ Uid x; ".";  S as xs  %ident'{$uid:x.$xs}
  | Uid x %{ `Uid(_loc,x)}] 
  qualid:
  [ Uid x ; "."; S as xs %{ `Dot(_loc,`Uid(_loc,x),xs)}
  | Lid i %{ `Lid(_loc,i)}]

  t_qualid:
  [ Uid x; ".";  S as xs %ident'{$uid:x.$xs}
  | Uid x; "."; Lid "t" %{ `Uid(_loc,x)}] 

  (* stands for the non-terminal  *)
  name: [ qualid as il %{mk_name il}] 

  (* parse entry name, accept a quotation name setup (FIXME)*)
  entry_name:
  [ qualid as il; ?Str  as name %{
    let x =
      match (name:Tokenf.txt option) (* FIXME more type annotation needed? *) with
      | Some x ->
          let old = !Ast_quotation.default in
          begin 
            match Ast_quotation.resolve_name (`Sub [], x.txt)
            with
            | None -> Locf.failf x.loc "lang `%s' not resolved" x.txt 
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
      
  |  entry_name as rest; "@"; "Local"; ":";  ? position as pos; level_list as levels %{
     let (n,p) = rest in
      begin
        (match n with
        |`name old -> Ast_quotation.default := old
        | _ -> ());
        match (pos,levels) with
        |(Some %exp{ `Level $_ },`Group _) ->
            failwithf "For Group levels the position can not be applied to Level"
        | _ -> Some {name=p;local=true;pos;levels}
      end
  }
  | Lid x ; "@"; "Inline"; ":"; rule_list as rules %{
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
  [  ? Str label ;  ?assoc as assoc; rule_list as rules
       %{{label ;assoc;rules}} ]

  assoc :
  [ ("LA"|"RA"|"NA" as x) %exp{$vrn:x} ]

      
  rule_list :
  [ "["; "]" %{ []}
  | "["; L1 rule SEP "|" as ruless; "]" %{Listf.concat ruless}]

  rule :
  [ left_rule as prod; ? opt_action as action %{
    let rec cross (prod: outer_symbol list) :
        Gram_def.osymbol Gram_def.decorate list list =
      match prod with
      | [] -> [[]]
      | x:: xs ->
          Listf.concat_map
            (fun (y:Gram_def.osymbol Gram_def.decorate  list) ->
              List.map
                (fun (z: Gram_def.symbol Gram_def.decorate) ->
                  (({kind = z.kind;
                     txt = {
                     text = z.txt.text ;
                     styp = z.txt.styp;
                     pattern = z.txt.pattern;
                     bounds = z.txt.bounds;
                     outer_pattern = x.outer_pattern}}
                       :
                     Gram_def.osymbol Gram_def.decorate)::y)) x.psymbols
                ) (cross xs) in
    List.map (fun prod -> mk_prule ~prod ~action) @@  cross prod}

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
            | Some b -> {x with action = Some %exp{ $a $b}}) rules}]
   left_rule:
   [ psymbol as x %{[x]}
   | psymbol as x;";" ;S as xs %{ x::xs }
   |    %{[]}]   

   opt_action@Local :
   [ Quot x %{
     if x.name = Tokenf.empty_name then 
       let expander loc _ s = Parsef.exp loc s  in
       Tokenf.quot_expand expander x
     else
       Ast_quotation.expand x Dyn_tag.exp}]

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






  (* |  ("Uid" as v) ; "("; or_words as p; ")" %{ *)
  (*   match p with *)
  (*   | (vs,None) -> *)
  (*       List.map (fun x -> token_of_simple_pat %pat'{$vrn:v $x}) vs *)
  (*   | (vs,Some x) -> *)
  (*       List.map (fun a -> token_of_simple_pat %pat'{$vrn:v ($a as $lid:x)}) vs} *)    

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






(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/parse_parse.cmo" *)
(* end: *)
