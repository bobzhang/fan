open FAst

class primitive = object
  method string _loc (i:string) :ep = %ep{$`str:i} 
end
type word  =
  [ `Any
  | `A of string
  | `Empty (* no token associated *)]

and data = (string * word)
   with ("MetaObj")

let meta_data = new meta  ;;

%ocaml{

type name = {(* every entry has a name *)  
    exp : exp;
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
 | `Tok of loc
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
 | `Stoken of (loc * exp * exp * string )
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
      
  };;



(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/gram_def.cmo" *)
(* end: *)
