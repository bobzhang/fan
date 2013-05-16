open Gstructure

val higher :
    [> terminal ] -> [> terminal ] -> bool

(** {[ symbol -> bool ]}*)      
val derive_eps : symbol -> bool

val tree_derive_eps : tree -> bool


(** create an empty level *)
val empty_lev : label  -> assoc  -> level

val levels_of_entry : entry -> level list option



(* given [entry] [position] and [levs]  return [levs* (label name * assoc ) -> level  *levs]*)  
val find_level :
    ?position:position -> entry -> level list
      -> level list * (level * string) option * level list


(** make sure the [entry] share the same gram with the symbol *)          
val check_gram : entry -> symbol -> unit

(** Scan the symbol to get the keyword list, the second argument is acc, which is empty  
    in most cases *)
val using_symbol : symbol -> string list -> string list
    
val get_initial : ([> `Sself ] as 'a) list -> bool * 'a list


(** given an  [production] and  a [tree], return a new [tree]
    The [tree] is used to merge the [production]

    {[

    Ginsert.add_production ([`Sself;`Skeyword "x";`Skeyword "y"],
    ("",Gaction.mk (fun _ -> ""))) DeadEnd;;
    - : Gstructure.tree = `-S---"x"---"y"---.
    without pretty printer 
    - : Grammar.Gstructure.tree =
    Node
    {node = `Sself;
      son =
     Node
    {node = `Skeyword "x";
       son =
    Node
    {node = `Skeyword "y"; son = LocAct (<abstr>, []); brother = DeadEnd};
    brother = DeadEnd};
    brother = DeadEnd}
   ]}
 *)
    
val add_production : production -> tree -> tree

val add_production_in_level : suffix:bool -> production -> level -> level


val merge_level : level -> olevel -> level
    
val level_of_olevel : olevel -> level
    

(** It's mainly for the side effects
    check whether the [gram]  is identical
    or  introducing any new [keywords] here, also it normalizes
    nontermial to S if possible *)                
val scan_olevels : entry -> olevel list -> olevel list

val scan_olevel : entry ->  olevel -> olevel 



val insert_olevels_in_levels : entry -> position option -> olevel list -> level list

(**
   it calls [merge_level] or [level_of_olevel]
 *)    
val insert_olevel : entry -> position option -> olevel -> level list      

val extend : entry -> position option * olevel list -> unit    

val extend_single : entry ->  position option * olevel -> unit    
