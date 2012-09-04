module Make :
 functor (Token : Sig.Camlp4Token) ->
  sig
   open Token

   type t

   val mk : (unit -> t)

   val define : (Token.Filter.t -> (t -> unit))

   val filter :
    (t -> ((Token.t * Loc.t) Stream.t -> (Token.t * Loc.t) Stream.t))

   val take_list : (t -> (string * Loc.t) list)

   val take_stream : (t -> (string * Loc.t) Stream.t)

  end
