

    (* [ `NA|`RA|`LA] *)
      
type position = int 




(* the [location] and the parsed value *)
type 'a cont_parse  = Locf.t -> Gaction.t -> 'a Tokenf.parse 
    
type gram = {
    annot : string;
    gfilter         : Tokenf.filter_plugin;
  }



    
type entry = {
    gram     : gram;
    name     : string;
    mutable start    :  int -> Gaction.t Tokenf.parse ;
    mutable continue : int -> Gaction.t cont_parse ;
    mutable levels     :  level list  ; (* sorted list *)
    mutable freezed :  bool;}

(* level is the runtime which is not used by the compiler *)      
and level = {
    level   : int;
    assoc   : bool ;
    productions : production list ; (* the raw productions stored in the level*)
    lsuffix : tree ;
    lprefix : tree}
and asymbol =
  | Nterm of entry
  | Snterml of (entry * int (* string *)) (* the second argument is the level name *)
  | List0 of symbol
  | List1 of symbol
  | Try of symbol
  | Peek of symbol
  | Self
  | List0sep of (symbol * symbol)        
  | List1sep of (symbol * symbol)      
  | Token of Tokenf.pattern 
and symbol =
  | Nterm of entry
  | Snterml of (entry * int(* string *)) (* the second argument is the level name *)
  | List0 of symbol
  | List0sep of (symbol * symbol)
  | List1 of symbol
  | List1sep of (symbol * symbol)
  | Try of symbol
  | Peek of symbol
  | Self
  | Token of Tokenf.pattern

      
and tree = (* internal struccture *)
  | Node of node
  | LocAct of anno_action (* *  anno_action list *)
  | DeadEnd 
and node = {
    node    : symbol ;
    son     : tree   ;
    brother : tree   }

and production= {
    symbols : symbol list;
    annot : string;
    fn : Gaction.t
   }

and anno_action =
    {arity : int ;
     symbols : symbol list;
     annot : string;
     fn : Gaction.t
   }








(**
   [olevel] is the [processed output] from the Fgram DDSL, the runtime representation
   is [level], there is a function [Ginsert.level_of_olevel] which converts the
   processed output into the runtime
   BOOTSTRAPING
 *)
type label = int option
      
type olevel  = {
    label : label ;
    assoc : bool;
    productions : production list
  }      

type extend_statment = {
    pos : position option ;
    olevels : olevel list
  }
type single_extend_statement =  olevel
      
type delete_statment = symbol list 



  

(* local variables: *)
(* compile-command: "cd .. &&  pmake treeparser/gdefs.cmo" *)
(* end: *)
