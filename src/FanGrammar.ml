open Ast;

(* {:fans| keep on;  derive (MetaExpr);|}; *)
{:ocaml|

type name = {(* every entry has a name *)  
    exp : exp;
    tvar : string;
    loc : loc
  };

(* we need to define a new ADT only because
   we did not find a way to expess `STself and `STtok yet  *)
type styp =
 [ ident'
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag * (* meta_option *) alident)
 | `Self of (loc * string)
 | `Tok of loc
 | `Type of ctyp ];
(* Normal, Antiquot, etc. translated to
   `Normal `Antiquot *)
type attr = string;
type entry   = {
  name : name ;
  (*position expession node *)    
  pos : exp option ;
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
 [ `Smeta of (loc * string list  * text list  * exp * styp)
 | `Slist of (loc * bool * symbol * symbol option )
 | `Snterm of (loc * name  * string option )
 | `Sopt of (loc * text )
 | `Stry of (loc * text )
 | `Speek of (loc * text)
 | `Srules of (loc * (text list  * exp) list )
 | `Sself of loc
 | `Snext of loc       
 | `Skeyword of (loc * string)
 | `Stok of (loc * exp * attr * string)
(** The first is the match function exp(predicate),
    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well comparable. *) ]
  |};

type used =
  | Unused | UsedScanned | UsedNotScanned ;

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
   ];

(* make [S] a keyword ? *) 
type action_pattern =
  [ vid
  |`Com of (loc * action_pattern * action_pattern)
  |`Par of (loc * action_pattern )
  |`Any of loc ];  
