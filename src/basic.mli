open Ast
exception Unhandled of ctyp
exception Finished of exp

val unit_literal : [> `Uid of loc * string ]
    
val x : ?off:int -> int -> string
val xid : ?off:int -> int -> [> `Lid of loc * string ]
val allx : ?off:int -> int -> string
val allxid : ?off:int -> int -> [> `Lid of loc * string ]
val check_valid : string -> unit
val conversion_table : (string, string) Hashtbl.t
