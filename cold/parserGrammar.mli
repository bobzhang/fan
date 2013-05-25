open FAst
(**
   {[
    with str t nonterminals {| U a b c d|}
    |> Ast2pt.print_stru f;
    let a = U.mk "a"
    let b = U.mk "b"
    let c = U.mk "c"
    let d = U.mk "d"]}
    It is very simple, may be improved to a depend on a simple engine
    It is used by DDSL [create]
 *)  
val nonterminals : stru Gram.t

(** {[
     with str t nonterminalsclear {| U a b c d|} |> Ast2pt.print_exp f;
     U.clear a; U.clear b; U.clear c; U.clear d
    ]}
    It's used by DDSL [clear]   
 *)    
val nonterminalsclear : exp Gram.t
val delete_rule_header : vid Gram.t


(**  parse the header, return the current [grammar] and
     previous module name, it has side effect, and can not
     be used alone
     {[
     with str t extend_header {| U.M |};
     - : FAst.ident option * FAst.ident = (None, `Uid (, "Gram"))
     with str t extend_header {| U |};
     - : FAst.ident option * FAst.ident =
     (None, `Dot (, `Uid (, "U"), `Uid (, "M")))
     with str t extend_header {| (g:U.t) |};
     - : FAst.ident option * FAst.ident = (Some (`Lid (, "g")), `Uid (, "U"))
     ]}
     It should be fixed by introducing more advanced grammar features *)    
val extend_header : (vid option * vid) Gram.t
    
val qualuid : vid Gram.t


(** parse qualified  [X.Y.g]
     {[
     with str t qualid {| A.B.g |};
     - : FAst.ident = `Dot (, `Uid (, "A"), `Dot (, `Uid (, "B"), `Lid (, "g")))
     ]} *)

val qualid : vid Gram.t

(** parse qualified path ending with [X.t]
     {[
     with str t t_qualid {| A.U.t |};
     - : FAst.ident = `Dot (, `Uid (, "A"), `Uid (, "U"))
     ]} *)
val t_qualid : vid Gram.t
    
val entry_name :
    ([ `name of FanToken.name | `non ] * FanGrammar.name) Gram.t

(* get local name entry list *)
val locals : FanGrammar.name list Gram.t

(** return an entry [FanGrammar.entry]
  {[with str t entry {| entry:
    [ entry_name{(n,p)}; ":";  OPT position{pos}; level_list{levels}
     -> begin 
     match n with
     |`name old -> AstQuotation.default := old
     | _ -> () ;  
    mk_entry ~name:p ~pos ~levels
    end] |}]}
   *)

val entry : FanGrammar.entry Gram.t



(** the main entrance
     return an already converted expession
     {[
     with str t extend_body  {|
     nonterminalsclear:
     [ qualuid{t}; L0 [a_lident{x}->x ]{ls} -> ()] |} |> Ast2pt.print_exp f;

     Gram.extend (nonterminalsclear : 'nonterminalsclear Gram.t )
     (None,
     [(None, None,
     [([`Snterm (Gram.obj (qualuid : 'qualuid Gram.t ));
     `Slist0
     (Gram.srules nonterminalsclear
     [([`Snterm (Gram.obj (a_lident : 'a_lident Gram.t ))],
     (Gram.mk_action
     (fun (x : 'a_lident)  (_loc : FanLoc.t)  -> (x : 'e__7 ))))])],
     (Gram.mk_action
     (fun (ls : 'e__7 list)  (t : 'qualuid)  (_loc : FanLoc.t)  ->
     (() : 'nonterminalsclear ))))])])
     ]}

     the function [text_of_functorial_extend] is the driving force
     it has type
     {[ FAst.loc ->
     FAst.ident option ->
     FanGrammar.name list option -> FanGrammar.entry list -> FAst.exp
     ]} *) 
val extend_body : exp Gram.t
val delete_rule_body : exp Gram.t


(** parse [position] and translate into [exp] node, fixme,
    delay the translation *)    
val position : exp Gram.t

(** parse association, and translate into [exp] node. FIXME  *)    
val assoc : exp Gram.t
val name : FanGrammar.name Gram.t
val string : exp Gram.t

val simple_exp : exp Gram.t
val delete_rules : exp Gram.t

val pattern : FanGrammar.action_pattern Gram.t



val simple_pat : FanGrammar.simple_pat Gram.t

val internal_pat : FanGrammar.simple_pat Gram.t

val symbol :  FanGrammar.symbol Gram.t

(** return a [rule]
    {[with str t rule {|  `Uid ("LA"|"RA"|"NA" as x)   |};
    - : FanGrammar.rule =
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
val rule :  FanGrammar.rule Gram.t
val rule_list : FanGrammar.rule list Gram.t
val psymbol : FanGrammar.symbol Gram.t
val level :  FanGrammar.level Gram.t
val level_list :
    ([ `Group of (FanGrammar.level list )
     | `Single of FanGrammar.level ]) Gram.t
val entry: FanGrammar.entry Gram.t
    
(* val d : [> `Absolute of string list ] *)
