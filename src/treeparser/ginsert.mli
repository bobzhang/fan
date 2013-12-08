
(** Internal: Handling functional insertion for Fan's entry *)
open Gdefs

val higher : symbol -> symbol -> bool 
    (* [> Tokenf.terminal ] -> [> Tokenf.terminal ] -> bool *)

(** {[ symbol -> bool ]}*)      
val derive_eps : symbol -> bool

(* val tree_derive_eps : tree -> bool *)


(** create an empty level *)
val empty_lev : label  -> bool  -> level





(* given [entry] [position] and [levs]  return [levs* (label name * assoc ) -> level  *levs]*)  
(* val find_level : *)
(*     ?position:position -> entry -> level list *)
(*       -> level list * (level * string) option * level list *)


(** make sure the [entry] share the same gram with the symbol *)          
(* val check_gram : entry -> symbol -> unit *)

(** Scan the symbol to get the keyword list, the second argument is acc, which is empty  
    in most cases *)
val using_symbol : symbol -> string list -> string list
    
val get_initial : symbol list -> bool * symbol list


(** given an  [production] and  a [tree], return a new [tree]
    The [tree] is used to merge the [production]

    {[

    Ginsert.add_production ([self;`Keyword "x";`Keyword "y"],
    ("",Gaction.mk (fun _ -> ""))) DeadEnd;;
    - : Gdefs.tree = `-S---"x"---"y"---.
    without pretty printer 
    - : Grammar.Gdefs.tree =
    Node
    {node = self;
      son =
     Node
    {node = `Keyword "x";
       son =
    Node
    {node = `Keyword "y"; son = LocAct (<abstr>, []); brother = DeadEnd};
    brother = DeadEnd};
    brother = DeadEnd}
   ]}
 *)
val add_production : production -> tree -> tree

(** the production is added in reverse order
    from the Fgram DDSL *)    
val add_production_in_level : production -> level -> level


val merge_level : level -> olevel -> level
    
val level_of_olevel : olevel -> level
    

(** It's mainly for the side effects
    check whether the [gram]  is identical
    or  introducing any new [keywords] here, also it normalizes
    nontermial to S if possible *)                
val scan_olevels : entry -> olevel list -> olevel list

val scan_olevel : entry ->  olevel -> olevel 





(**
   it calls [merge_level] or [level_of_olevel]
 *)    
val insert_olevel : entry -> int option -> olevel -> level list      




(** see {extend}, it would promote the keywords automatically *)    
(* val unsafe_extend : entry -> position option * olevel list -> unit     *)

(* val extend_single : 'a entry ->  Gdefs.single_extend_statement -> unit     *)


(** change the entry behavior temporarily *)
(* val protect : entry -> Gdefs.single_extend_statement -> (entry -> 'a) -> 'a *)

    
(** see {extend_single} and {unsafe_extend} *)    
(* val unsafe_extend_single : entry ->  Gdefs.single_extend_statement -> unit     *)



    
(** duplciate a grammar which the user can freely mutate without
    worring about the side effect  *)
(* val copy : entry -> entry *)

(** [FIXME] the annotation seems to be inconsistent *)    
(* val eoi_entry : entry -> entry *)
