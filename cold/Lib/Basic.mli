val unit_literal : Ast.expr

val x : (?off : int -> (int -> string))


val xid : (?off : int -> (int -> Ast.ident))

val allx :
                                               (?off : int ->
                                                (int -> string))

val allxid :
                                                                   (?off :
                                                                    int ->
                                                                    (int ->
                                                                    Ast.ident))


val check_valid : (string -> unit)

val error_report :
                                     ((FanLoc.t * string) -> unit)
