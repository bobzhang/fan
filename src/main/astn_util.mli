
(** A module for handling abstract syntax without locations *)


open Astfn

val sem : 'a -> 'b -> [> `Sem of 'a * 'b ]
val com : 'a -> 'b -> [> `Com of 'a * 'b ]
val app : 'a -> 'b -> [> `App of 'a * 'b ]
val apply : 'a -> 'b -> [> `Apply of 'a * 'b ]
val sta : 'a -> 'b -> [> `Sta of 'a * 'b ]
val bar : 'a -> 'b -> [> `Bar of 'a * 'b ]
val anda : 'a -> 'b -> [> `And of 'a * 'b ]
val dot : 'a -> 'b -> [> `Dot of 'a * 'b ]
val par : 'a -> [> `Par of 'a ]
val seq : 'a -> [> `Seq of 'a ]
val arrow : 'a -> 'b -> [> `Arrow of 'a * 'b ]
val typing : 'a -> 'b -> [> `Constraint of 'a * 'b ]
val bar_of_list : ([> `Bar of 'a * 'a ] as 'a) list -> 'a
val and_of_list : ([> `And of 'a * 'a ] as 'a) list -> 'a
val sem_of_list : ([> `Sem of 'a * 'a ] as 'a) list -> 'a
val com_of_list : ([> `Com of 'a * 'a ] as 'a) list -> 'a
val sta_of_list : ([> `Sta of 'a * 'a ] as 'a) list -> 'a
val dot_of_list : ([> `Dot of 'a * 'a ] as 'a) list -> 'a
val appl_of_list : ([> `App of 'a * 'a ] as 'a) list -> 'a

val seq_sem : ([> `Sem of 'a * 'a ] as 'a) list -> [> `Seq of 'a ]

val binds : bind list -> exp -> exp

val lid : string -> [> `Lid of string ]

val uid : string -> [> `Uid of string ]

val unit : ep



val tuple_com_unit : ep list -> ep

val tuple_com : ([> `Com of 'a * 'a | `Par of 'a ] as 'a) list -> 'a

val tuple_sta : ([> `Par of 'a | `Sta of 'a * 'a ] as 'a) list -> 'a




(**
   Function application
   {[
   %exp-{f} +> ["x";"y"] ) = %exp-{f x y} ;;
   - : bool = true 
   ]}
   
 *)
    
  
val (+>) :
    ([> `App of 'a * 'a | `Lid of string ] as 'a) -> string list -> 'a



(**
   A very naive lifting. It does not do any parsing at all
   It is applied to both exp and pat

   {[
   of_str "`A" =   `Vrn  "A" 
   
   of_str "A" = `Uid  "A"

   of_str "abs" = `Lid  "abs"

   of_str "&&" = `Lid  "&&"
      
   of_str "::" = `Uid "::"
   ]}
 *)

val of_str: string -> ep
