module Make(U:sig end) : sig

  type t
  exception Error of string * string


  (** [mk ?ocaml_stdlib]
      The stdlib flag is true by default.
      To disable it use: [mk ~ocaml_stdlib:false] *)
  val mk : ?ocaml_stdlib: bool  -> unit -> t
      
  (** Fold over the current load path list. *)
  val fold_load_path : t -> (string -> 'a -> 'a) -> 'a -> 'a
      
  (** [load f] Load the file [f]. If [f] is not an absolute path name,
      the load path list used to find the directory of [f]. *)
  val load : t -> string -> unit
      
  (** [include_dir d] Add the directory [d] in the current load path
     list (like the common -I option). *)
  val include_dir : t -> string -> unit
      
  (** [find_in_path f] Returns the full path of the file [f] if
     [f] is in the current load path, raises [Not_found] otherwise. *)
  val find_in_path : t -> string -> string
      
  val instance : (unit -> t) ref
end
