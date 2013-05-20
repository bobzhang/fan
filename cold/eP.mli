val tuple_of_number : Ast.ep -> int -> Ast.ep
val of_vstr_number : string -> int -> Ast.ep
val gen_tuple_n :
  ?cons_transform:(string -> string) -> arity:int -> string -> int -> Ast.ep
val mk_record : ?arity:int -> FSig.col list -> Ast.ep
val mk_tuple : arity:int -> number:int -> Ast.ep
val of_str: string -> Ast.ep
