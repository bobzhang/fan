module Make :
 functor (Loc : FanSig.Loc) -> (FanSig.Camlp4Token with module Loc = Loc)


module Eval :
 sig
  val char : (string -> char)

  val string : (?strict : unit -> (string -> string))

 end
