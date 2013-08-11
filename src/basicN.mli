(** Basic module for Fan's deriving mechanism *)



  

val x : ?off:int -> int -> string

val xid : ?off:int -> int -> [> `Lid of string ]

val allx : ?off:int -> int -> string

val allxid : ?off:int -> int -> [> `Lid of string ]


val conversion_table : (string, string) Hashtbl.t
  
