

  
val setup_op_parser : FAst.exp Fgram.t -> (string -> bool) -> unit

val infix_kwds_filter :
  (FToken.t * 'b) XStream.t ->  (FToken.t * 'b) XStream.t


(* all chars in string from [i] are symbol chars*)      
val symbolchar : string -> int -> bool      
