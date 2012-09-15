(** Signature for errors modules, an Error modules can be registred with
    the {!ErrorHandler.Register} functor in order to be well printed. *)
module type Error = sig
  type t
  exception E of t
  val to_string : t -> string
  val print : Format.formatter -> t -> unit
end




















