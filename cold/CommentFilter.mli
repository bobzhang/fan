module Make :
 functor (Token : FanSig.Camlp4Token) ->
  sig
   type t

   val mk : (unit -> t)

   val define : (Token.Filter.t -> (t -> unit))

   val filter :
    (t -> ((Token.t * FanLoc.t) Stream.t -> (Token.t * FanLoc.t) Stream.t))

   val take_list : (t -> (string * FanLoc.t) list)

   val take_stream : (t -> (string * FanLoc.t) Stream.t)

  end
