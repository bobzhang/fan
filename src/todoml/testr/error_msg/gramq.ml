
(* forget a colon after LOCAL *)
{:extend| (fu:MGram.t) LOCAL a b;
  expr:
  [SELF;"+";SELF]
|};
