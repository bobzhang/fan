open AstN
val tuple_of_number : ep -> int -> ep
val of_vstr_number : string -> int -> ep
val gen_tuple_n :
  ?cons_transform:(string -> string) -> arity:int -> string -> int -> ep
val mk_record : ?arity:int -> CtypN.col list -> ep
val mk_tuple : arity:int -> number:int -> ep
val of_str: string -> ep
