
open FAst
  
val exp : Locf.t -> string -> exp
val pat : Locf.t -> string -> pat
val ident : Locf.t -> string -> ident



    
val exp_filter : ep -> exp
val pat_filter : ep -> pat

val exp_filter_n : ep -> exp
val pat_filter_n : ep -> pat


val expand_exp : Tokenf.quot -> exp
