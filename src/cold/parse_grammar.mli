

(** The front-end of Fan's gram DDSL *)

  
open FAst





(**  parse the header, return the current [grammar] and
     previous module name, it has side effect, and can not
     be used alone
     {[
     with str t extend_header %{ U.M };
     - : FAst.ident option * FAst.ident = (None, `Uid (, "Fgram"))
     with str t extend_header %{ U };
     - : FAst.ident option * FAst.ident =
     (None, `Dot (, `Uid (, "U"), `Uid (, "M")))
     with str t extend_header %{ (g:U.t) };
     - : FAst.ident option * FAst.ident = (Some (`Lid (, "g")), `Uid (, "U"))
     ]}
     It should be fixed by introducing more advanced grammar features *)    
val extend_header : (vid option * vid) Fgram.t
    
val qualuid : vid Fgram.t


(** parse qualified  [X.Y.g]
     {[
     with str t qualid %{ A.B.g };
     - : FAst.ident = `Dot (, `Uid (, "A"), `Dot (, `Uid (, "B"), `Lid (, "g")))
     ]} *)

val qualid : vid Fgram.t

(** parse qualified path ending with [X.t]
     {[
     with str t t_qualid %{ A.U.t };
     - : FAst.ident = `Dot (, `Uid (, "A"), `Uid (, "U"))
     ]} *)
val t_qualid : vid Fgram.t
    
(* val entry_name : *)
(*     ([ `name of Ftoken.name option | `non ] * Gram_def.name) Fgram.t *)


(** return an entry [Gram_def.entry]
  {[with str t entry {| entry:
    [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
     -> begin 
     match n with
     |`name old -> Ast_quotation.default := old
     | _ -> () ;  
    mk_entry ~name:p ~pos ~levels
    end] |}]}
   *)

val entry : Gram_def.entry Fgram.t




(** parse [position] and translate into [exp] node, fixme,
    delay the translation *)    
val position : exp Fgram.t

(** parse association, and translate into [exp] node. FIXME  *)    
val assoc : exp Fgram.t
val name : Gram_def.name Fgram.t
val string : exp Fgram.t






(** return symbol with patterns (may override inferred patterns) *)
val psymbol : Gram_def.symbol Fgram.t
    
(** return symbol with pattern(inferred) or None  *)    
val symbol :  Gram_def.symbol Fgram.t


val simple_meta : Gram_def.symbol Gentry.t     
(** return a [rule]
    {[with str t rule %{  `Uid ("LA"|"RA"|"NA" as x)   };
    - : Gram_def.rule =
     {prod =
     [{text =
     `Stok
     (,
     `Fun
     (,
     `Bar
     (,
     `Case
     (,
     `App
     (, `Vrn (, "Uid"),
     `Bar
     (, `Bar (, `Str (, "LA"), `Str (, "RA")), `Str (, "NA"))),
     `Nil , `Id (, `Lid (, "true"))),
     `Case (, `Any , `Nil , `Id (, `Lid (, "false"))))),
     "Normal", "`Uid (\"LA\"|\"RA\"|\"NA\")");
     styp = `Tok ;
     pattern =
     Some
     (`App
     (, `Vrn (, "Uid"),
     `Alias
     (, `Bar (, `Bar (, `Str (, "LA"), `Str (, "RA")), `Str (, "NA")),
     `Lid (, "x"))))}];
     action = None}
     ]} *)
val rule :  Gram_def.rule Fgram.t
val rule_list : Gram_def.rule list Fgram.t

val level :  Gram_def.level Fgram.t
val level_list :
    ([ `Group of (Gram_def.level list )
     | `Single of Gram_def.level ]) Fgram.t


(** the main entrance
     return an already converted expession
     {[
     with str t extend_body  {|
     nonterminalsclear:
     [ qualuid{t}; L0 [a_lident{x}->x ]{ls} -> ()] |} |> Ast2pt.print_exp f;

     Fgram.extend (nonterminalsclear : 'nonterminalsclear Fgram.t )
     (None,
     [(None, None,
     [([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ));
     `Slist0
     (Fgram.srules nonterminalsclear
     [([`Snterm (Fgram.obj (a_lident : 'a_lident Fgram.t ))],
     (Fgram.mk_action
     (fun (x : 'a_lident)  (_loc : Locf.t)  -> (x : 'e__7 ))))])],
     (Fgram.mk_action
     (fun (ls : 'e__7 list)  (t : 'qualuid)  (_loc : Locf.t)  ->
     (() : 'nonterminalsclear ))))])])
     ]}

     the function [text_of_functorial_extend] is the driving force
     it has type
     {[ FAst.loc ->
     FAst.ident option ->
     Gram_def.name list option -> Gram_def.entry list -> FAst.exp
     ]} *) 
val extend_body : exp Fgram.t


    
