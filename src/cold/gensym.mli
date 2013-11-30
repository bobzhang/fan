

(** The runtime for [gensym]. It's recommended to use [lang_fresh] to achieve
    a fresh id, since we may change this API in the future
  [%fresh{x}]
  or
  [%fresh{$x}]
*)
val fresh: ?prefix:string -> unit -> string
