open Ast;
type loc = FanLoc.t;

(* every entry has a name *)  
type name  = { expr : expr; tvar : string; loc : loc };

(* we need to define a new ADT only because
   we did not find a way to express `STself and `STtok yet  *)
(*
type styp =
 [= `STlid of (loc * string)
 | `STapp of (loc * styp * styp)
 | `STquo of (loc * string)
 | `STself of (loc * string)
 | `STtok of loc
 | `STtyp of ctyp ];

*)
type styp =
 [=
  `Id of (loc * ident )
 | `TyApp of (loc * styp * styp)
 | `TyQuo of (loc * string)
 | `Self of (loc * string)
 | `Tok of loc
 | `Type of ctyp ];

(* and patt = *) (* FIXME subtyping soon*)
(*  [= `PaApp of (loc * patt) *)
(*  | `PaVrn of (loc * string) *)
(*  | ] *)  



(* Normal, Antiquot, etc. translated to
   `Normal `Antiquot
 *)
type attr = string;

type entry   = {
  name : name ;
  pos : option expr;
  levels : list level;
}
and level  ={
  label : option string;
  assoc : option expr;
  rules : list rule
}
and rule ={
  prod : list symbol;
  action : option expr
}
and symbol ={
  text : text;
  styp : styp;
  pattern : option patt
}
and text =
 [= `TXmeta of (loc * list string * list text * expr * styp)
 | `TXlist of (loc * bool * symbol * option symbol)
 | `TXnterm of (loc * name  * option string)
 | `TXopt of (loc * text )
 | `TXtry of (loc * text )
 | `TXpeek of (loc * text)
 | `TXrules of (loc * list (list text * expr))
 | `TXself of loc
 | `TXnext of loc       
 | `TXkwd of (loc * string)
 | `TXtok of (loc * expr * attr * string)];
(** The first is the match function expr,
    the second is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string normalized and well comparable. *) 
  
type used = [ Unused | UsedScanned | UsedNotScanned ];

