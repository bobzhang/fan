
open FAst
  
val setup_op_parser : FAst.exp Gram.t -> (string -> bool) -> unit

val infix_kwds_filter :
  (FanToken.t * 'b) XStream.t ->  (FanToken.t * 'b) XStream.t
