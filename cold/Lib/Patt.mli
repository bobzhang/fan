open Ast
val app : patt -> patt -> patt

val comma : patt -> patt -> patt

val ( <$ ) : patt -> patt -> patt

val sem : patt -> patt -> patt
val list_of_app : patt -> patt list
val list_of_com : patt -> patt list
val list_of_sem : patt -> patt list
val view_app : patt list -> patt -> patt * patt list
val app_of_list : patt list -> patt
val com_of_list : patt list -> patt
val sem_of_list : patt list -> patt
val mklist : loc -> patt list -> patt
val apply : patt -> patt list -> patt
val mkarray : loc -> patt array -> patt
val of_str : string -> patt
val of_ident_number : ident -> int -> patt
val ( +> ) : patt -> string list -> patt
val gen_tuple_first : number:int -> off:int -> patt
val gen_tuple_second : number:int -> off:int -> patt
val tuple_of_number : patt -> int -> patt
val tuple_of_list : patt list -> patt

val of_vstr_number : string -> int -> patt
    
val gen_tuple_n : arity:int -> string -> int -> patt
val tuple : loc -> patt list -> patt
val mk_record : ?arity:int -> FSig.col list -> patt
val mk_tuple : arity:int -> number:int -> patt
