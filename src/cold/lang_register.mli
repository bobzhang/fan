



(** Exported for reuse *)  
val make_register : Astf.exp Gramf.t ->
  (Locf.t ->
    ((string * Locf.t) * (string * Locf.t)) list -> Astf.exp)
  ->  unit    
