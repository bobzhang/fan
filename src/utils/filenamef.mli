
(** Simple extension to the {!Filename} module  *)
include module type of Filename



(** Search a file in a list of directories. The direction is from the head to tail *)          
val find_in_path : path:string list -> string -> string option


    (** Same, but search also for uncapitalized name, i.e.
        if name is Foo.ml, allow /path/Foo.ml and /path/foo.ml
        to match. *)
val find_in_path_uncap : path:string list -> string -> string

    (** Expand a -I option: if it starts with +, make it relative to the standard
        library directory *)
val expand_directory : std:string -> string -> string
