

(** a simple encapsulation for dynamic loading in Fan *)    



(** [load f] Load the file [f]. If [f] is not an absolute path name,
    the load path list used to find the directory of [f]. It searches
    in [Configf.dynload_dirs] *)
val load : string -> unit
    
val objext : string

val libext : string    
