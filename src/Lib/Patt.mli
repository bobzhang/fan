open Ast



val mklist : loc -> patt list -> patt
val mkarray : loc -> patt array -> patt
    
val of_str : string -> patt

val of_ident_number : ident -> int -> patt

val ( +> ) : patt -> string list -> patt

val gen_tuple_first : number:int -> off:int -> patt

val gen_tuple_second : number:int -> off:int -> patt

val tuple_of_number : patt -> int -> patt

val of_vstr_number : string -> int -> patt
    
val gen_tuple_n :
    ?cons_transform:(string->string) -> arity:int -> string -> int -> patt

val tuple : loc -> patt list -> patt

val mk_record : ?arity:int -> FSig.(* record_ *)col list -> patt

val mk_tuple : arity:int -> number:int -> patt


      
