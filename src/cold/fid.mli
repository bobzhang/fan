(** Basic module for Fan's deriving mechanism *)



  
(**
   {[
   x ~off:2 3 -> _c3
   x 3 -> _a3
   ]}

   The default offset is zero
 *)
val x : ?off:int -> int -> string

val xid : ?off:int -> int -> [> `Lid of string ]

(**
   see {!x}, add a prefix ["all"].
   {[
   BasicN.allx ~off:2 3 ;;
   - : string = "all_c3"
   ]}
 *)

    
val allx : ?off:int -> int -> string

val allxid : ?off:int -> int -> [> `Lid of string ]


  
