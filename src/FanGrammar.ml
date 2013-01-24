open FanAst;


{:fans| keep on;  derive (MetaExpr MetaPatt);|};
{:ocaml|
(* every entry has a name *)  
type name  = {
    expr : expr;
    tvar : string;
    loc : loc
  };

(* we need to define a new ADT only because
   we did not find a way to express `STself and `STtok yet  *)
type styp =
 [= `Id of (loc * ident )
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag * meta_option alident)
 | `Self of (loc * string)
 | `Tok of loc
 | `Type of ctyp ];

(* Normal, Antiquot, etc. translated to
   `Normal `Antiquot
 *)
type attr = string;


type entry   = {
  name : name ;

  (*position expression node *)    
  pos : option expr;
    
  levels : levels(* list level *);
}
and levels =
 [= `Group of (list level) | `Single of level]   
and level  ={

  (* mainly used for indexing *)  
  label : option string;
    
  assoc : option expr;
    
  rules : list rule
}

and rule = {
  prod : list symbol;
    
  action : option expr
}
and symbol ={
  text : text;
    
  styp : styp;
  (* the inferred type of the result parsed by the current symbol *)
    
  pattern : option patt
}
and text =
 [= `Smeta of (loc * list string * list text * expr * styp)
 | `Slist of (loc * bool * symbol * option symbol)
 | `Snterm of (loc * name  * option string)
 | `Sopt of (loc * text )
 | `Stry of (loc * text )
 | `Speek of (loc * text)
 | `Srules of (loc * list (list text * expr))
 | `Sself of loc
 | `Snext of loc       
 | `Skeyword of (loc * string)
       
 | `Stok of (loc * expr * attr * string)
(** The first is the match function expr(predicate),

    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well comparable. *) 

];
  |};

module Expr = struct
  open FanAst.MExpr;
  open Filters.ME;
  __MetaExpr__;
end;
module Patt = struct
  open FanAst.MPatt;
  open Filters.MP;
  __MetaPatt__;

end;

type used = [ Unused | UsedScanned | UsedNotScanned ];

type simple_patt =
  [=
   `Vrn of (loc * string)
  |`App of (loc * simple_patt * simple_patt )
  |`Id of (loc * ident)
  |`Com of (loc * simple_patt * simple_patt)
  |`Alias of (loc * simple_patt * alident)
  |`Or of (loc * simple_patt * simple_patt)
  |`Str of (loc * string)
  |`Any of loc
  |`Nil of loc 
  | ant
   ];

(* make [S] a keyword ? *) 
type action_pattern =
  [=
   `Id of (loc * ident)
  |`Com of (loc * action_pattern * action_pattern)
  |`Tup of (loc * action_pattern )
  |`Any of loc
  |`Nil of loc 
 ];  
