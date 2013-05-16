exception Unhandled of Ast.ctyp
exception Finished of Ast.exp

val unit_literal : [> `Uid of FanLoc.t * string ]
    
val x : ?off:int -> int -> string
val xid : ?off:int -> int -> [> `Lid of FanLoc.t * string ]
val allx : ?off:int -> int -> string
val allxid : ?off:int -> int -> [> `Lid of FanLoc.t * string ]
val check_valid : string -> unit
val conversion_table : (string, string) Hashtbl.t
