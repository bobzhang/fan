(** A util module but not generic enough to be moved into LibUtil   *)


(** Either dump to a file or stdout *)    
val with_open_out_file : string option -> (out_channel -> 'a) -> unit

(** dump ocaml compatible marshallized ast  *)        
val dump_pt : string -> string -> 'a -> out_channel -> unit



(* val wrap : ('a -> 'a option) -> *)
(*   (FLoc.t -> 'b -> 'a list * FLoc.t option) -> FLoc.t -> 'b -> 'a list     *)

val simple_wrap :
    Location.t -> 'a ->
      (FLoc.t -> 'a -> 'b list * FLoc.t option) ->
        'b list      
