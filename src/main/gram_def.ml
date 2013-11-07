open FAst

type name = {
    id : vid ;
    tvar : string;
    loc : loc
  }

(* we need to define a new ADT only because
   we did not find a way to expess `STself and `STtok yet  *)
type styp =
 [ vid'
 | `App of (loc * styp * styp)
 | `Quote of (loc * position_flag *  alident)
 | `Self of loc
 | `Type of ctyp ]


type entry   = {
  name : name ;
  (*position expession node *)    
  pos : exp option ;
  local : bool ;  (* mark whether the grammar is local or not*)
  levels : levels;
}
and levels =
 [ `Group of (level list ) | `Single of level]   
and level  = {
  (* mainly used for indexing *)  
  label : string option ;
  assoc : exp option ;
  rules : rule list
}
and rule = {
    prod : osymbol list ;

    action : exp option ;

    env : (locid * exp) list;
}
and kind =
  | KNone
  | KSome
  | KNormal
and locid = (loc * string)
and symbol = {
    text : text;
    styp : styp;
    pattern : pat option; (* inner destruction *)
    bounds : locid list; (* inner bounded variables *) 
}
and 'a decorate = {
    kind : kind;
    txt : 'a;
  }      
and osymbol = {
    text : text;
    styp : styp;
    pattern : pat option;
    bounds : locid list ;
    outer_pattern : locid option;
  }

and text =
 [
   `List of (loc * bool * symbol * symbol option )
 | `Nterm of (loc * name  * string option )
 | `Try of (loc * text )
 | `Peek of (loc * text)
 | `Self of loc
 | `Keyword of (loc * string)
 | `Token of (loc * exp )
(** The first is the match function exp(predicate),
    the second and the third  is the string description.
    The description string will be used for
    grammar insertion and left factoring.
    Keep this string [normalized] and well *comparable*. *) ]

type entries = {
    items : entry list;
    gram : vid option;
    safe : bool
  }

      




(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gram_def.cmo" *)
(* end: *)
