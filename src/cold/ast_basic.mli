

val of_listr : ('a -> 'a -> 'a) -> 'a list -> 'a
val of_listl : ('a -> 'a -> 'a) -> 'a list -> 'a

val list_of :
  ([< `And of 'b * 'a * 'a
    | `App of 'c * 'a * 'a
    | `Bar of 'd * 'a * 'a
    | `Com of 'e * 'a * 'a
    | `Dot of 'f * 'a * 'a
    | `Sem of 'g * 'a * 'a
    | `Sta of 'h * 'a * 'a ]
   as 'a) ->
  'i -> 'i

val list_of :
    ([> `And of 'b * 'a * 'a
     | `App of 'c * 'a * 'a
     | `Bar of 'd * 'a * 'a
     | `Com of 'e * 'a * 'a
     | `Dot of 'f * 'a * 'a
     | `Sem of 'g * 'a * 'a
     | `Sta of 'h * 'a * 'a ]
       as 'a) ->
         'a list -> 'a list
      
val list_of_and : ([> `And of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list

val fold_and_right :
    (([> `And of 'b * 'a * 'a ] as 'a) -> 'c -> 'c) -> 'a -> 'c -> 'c

val fold_bar_right :
    (([> `Bar of 'b * 'a * 'a ] as 'a) -> 'c -> 'c) -> 'a -> 'c -> 'c
        
val list_of_com : ([> `Com of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_star : ([> `Sta of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_bar : ([> `Bar of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_sem : ([> `Sem of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_dot : ([> `Dot of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_app : ([> `App of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val listr_of_arrow :
  ([> `Arrow of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val view_app : 'a list -> ([> `App of 'c * 'b * 'a ] as 'b) -> 'b * 'a list


module N : sig
  val list_of_and : ([> `And of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_com : ([> `Com of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_star : ([> `Sta of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_bar : ([> `Bar of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_or : ([> `Bar of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_sem : ([> `Sem of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_dot : ([> `Dot of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val list_of_app : ([> `App of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val listr_of_arrow :
      ([> `Arrow of 'a * 'a ] as 'a) -> 'a list -> 'a list
  val view_app : 'a list -> ([> `App of 'b * 'a ] as 'b) -> 'b * 'a list          
end
    
