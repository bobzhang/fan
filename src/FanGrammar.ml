open Ast;
(* open FanOps; *)
(* open FanAst; *)


(* {:fans| keep on;  derive (MetaExpr);|}; *)
{:ocaml|
(* every entry has a name *)  
type name  = {
    exp : exp;
    tvar : string;
    loc : loc
  };

(* we need to define a new ADT only because
   we did not find a way to expess `STself and `STtok yet  *)
type styp =
 [= `Id of (loc * ident )
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag * (* meta_option *) alident)
 | `Self of (loc * string)
 | `Tok of loc
 | `Type of ctyp ];

(* Normal, Antiquot, etc. translated to
   `Normal `Antiquot
 *)
type attr = string;


type entry   = {
  name : name ;

  (*position expession node *)    
  pos : option exp;
    
  levels : levels(* list level *);
}
and levels =
 [= `Group of (list level) | `Single of level]   
and level  ={

  (* mainly used for indexing *)  
  label : option string;
    
  assoc : option exp;
    
  rules : list rule
}

and rule = {
  prod : list symbol;
    
  action : option exp
}
and symbol ={
  text : text;
    
  styp : styp;
  (* the inferred type of the result parsed by the current symbol *)
    
  pattern : option pat
}
and text =
 [= `Smeta of (loc * list string * list text * exp * styp)
 | `Slist of (loc * bool * symbol * option symbol)
 | `Snterm of (loc * name  * option string)
 | `Sopt of (loc * text )
 | `Stry of (loc * text )
 | `Speek of (loc * text)
 | `Srules of (loc * list (list text * exp))
 | `Sself of loc
 | `Snext of loc       
 | `Skeyword of (loc * string)
       
 | `Stok of (loc * exp * attr * string)
(** The first is the match function exp(predicate),

    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well comparable. *) 

];
  |};

(* module Expr = struct *)
(*   open Filters.ME; *)
(*   __MetaExpr__; *)
(* end; *)
(* module Patt = struct *)
(*   open Filters.MP; *)
(*   __MetaExpr__; *)
(* end; *)
type used = [ Unused | UsedScanned | UsedNotScanned ];

type simple_pat =
  [=
   `Vrn of (loc * string)
  |`App of (loc * simple_pat * simple_pat )
  |`Id of (loc * ident)
  |`Com of (loc * simple_pat * simple_pat)
  |`Alias of (loc * simple_pat * alident)
  |`Or of (loc * simple_pat * simple_pat)
  |`Str of (loc * string)
  |`Any of loc
  | ant
   ];

(* make [S] a keyword ? *) 
type action_pattern =
  [=
   `Id of (loc * ident)
  |`Com of (loc * action_pattern * action_pattern)
  |`Tup of (loc * action_pattern )
  |`Any of loc

 ];  
