
open FAstN


(** mapping an ident to  a type variable *)  
val tvar_of_ident : vid -> string


(** {[
    map_to_string <:ident< A.B.f.g>>;
    a_b_f_g
  ]}
  see ident_map 
 *)  

val map_to_string : vid -> string

val to_string : ident -> string

val to_vid : ident -> vid

(** For qualified identifier, we only use the last qulifier.
    {[
    ident_map (fun x -> "meta_" ^ x ) %ident-'{ A.B.g };; 
    `Dot (`Uid "B", `Lid "meta_g")
    ident_map (fun x -> "meta_" ^ x ) %ident-'{ g };; 
   `Lid "meta_g"
   ]} *)
val ident_map :
    (string -> string) ->
      vid -> [> `Dot of vid * [> `Lid of string ] | `Lid of string ]

(* the same as [ident_map] except f is of type [string -> ident ] *)

val ident_map_of_ident : (string -> vid) -> vid -> vid

