open FAst
(* open StdFan *)
(* {:fans| keep on;  derive (MetaObj);|};; *)
{:ocaml|

type name = {(* every entry has a name *)  
    exp : exp;
    tvar : string;
    loc : loc
  }

(* we need to define a new ADT only because
   we did not find a way to expess `STself and `STtok yet  *)
type styp =
 [ (* ident' *) vid'
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag *  alident)
 (* | `Self of (loc * string) *)
 | `Self of loc
 | `Tok of loc
 | `Type of ctyp ]

type entry   = {
  name : name ;
  (*position expession node *)    
  pos : exp option ;
  local : bool ;  (* mark whether the grammar is local or not*)
  levels : levels(* list level *);
}
and levels =
 [ `Group of (level list ) | `Single of level]   
and level  ={
  (* mainly used for indexing *)  
  label : string option ;
  assoc : exp option ;
  rules : rule list
}
and rule = {
  prod : symbol list ;
  action : exp option 
}
      
and symbol ={
  text : text;
  styp : styp;
  (* the inferred type of the result parsed by the current symbol *)
  pattern : pat option 
}
and text =
 [
   `Slist of (loc * bool * symbol * symbol option )
 | `Snterm of (loc * name  * string option )
 | `Sopt of (loc * text )
 | `Stry of (loc * text )
 | `Speek of (loc * text)
 | `Sself of loc
 | `Skeyword of (loc * string)
 | `Stok of (loc * exp * FAstN.pat )
(** The first is the match function exp(predicate),
    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well comparable. *) ]
  |};;

type used =
  | Unused | UsedScanned | UsedNotScanned 

type simple_pat =
  [
   `Vrn of (loc * string)
  |`App of (loc * simple_pat * simple_pat )
  | vid 
  |`Com of (loc * simple_pat * simple_pat)
  |`Alias of (loc * simple_pat * alident)
  |`Bar of (loc * simple_pat * simple_pat)
  |`Str of (loc * string)
  |`Any of loc
  | ant
   ]

(* make [S] a keyword ? *) 
type action_pattern =
  [ vid
  |`Com of (loc * action_pattern * action_pattern)
  |`Par of (loc * action_pattern )
  |`Any of loc ];;

FConfig.antiquotations := true;;
open Fsyntax;;

{:create| Fgram (simple_pat : simple_pat Fgram.t) |};;

{:extend|
  simple_pat "pat'":
  ["`"; luident{s}  ->  {|$vrn:s|}
  |"`"; luident{v}; `Ant (("" | "anti" as n) ,s) ->
    {| $vrn:v $(FanUtil.mk_anti _loc ~c:"pat" n s)|}
  |"`"; luident{s}; `STR(_,v) -> {| $vrn:s $str:v|}
  |"`"; luident{s}; `Lid x  -> {| $vrn:s $lid:x |}
  |"`"; luident{s}; "_" -> {|$vrn:s _|}
  |"`"; luident{s}; "("; L1 internal_pat SEP ","{v}; ")" ->
      (AstLib.appl_of_list ({:pat'|$vrn:s|} :: v))
        (* here
           we have to guarantee
           {[
           {:pat-|`a(a,b,c)|};;
           - : FAstN.pat = `App (`App (`App (`Vrn "a", `Lid "a"), `Lid "b"), `Lid "c")
           ]}
           is dumped correctly
         *)
 ]

  let internal_pat "pat'": (* FIXME such grammar should be deprecated soon*)
  {
   "as"
     [S{p1} ; "as";a_lident{s} -> {| ($p1 as $s) |} ]
     "|"
     [S{p1}; "|"; S{p2}  -> {|$p1 | $p2 |} ]
     "simple"
     [ `STR(_,s) -> {| $str:s|}
     | "_" -> {| _ |}
     | `Lid x   ->  {| $lid:x|}
     | "("; S{p}; ")" -> p] }
|};;
