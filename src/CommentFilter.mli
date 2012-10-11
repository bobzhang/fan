
  type t;

  val mk : unit -> t;

  val define : FanToken.Filter.t -> t -> unit;

  val filter : t -> Stream.t (FanToken.t * FanLoc.t) -> Stream.t (FanToken.t * FanLoc.t);

  val take_list : t -> list (string * FanLoc.t);

  val take_stream : t -> Stream.t (string * FanLoc.t);

