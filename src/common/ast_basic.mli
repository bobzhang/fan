



val list_of_and : ([> `And of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_com : ([> `Com of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_star : ([> `Sta of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_bar : ([> `Bar of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_or : ([> `Bar of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_sem : ([> `Sem of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_dot : ([> `Dot of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_app : ([> `App of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val list_of_arrow_r :
  ([> `Arrow of 'b * 'a * 'a ] as 'a) -> 'a list -> 'a list
val view_app : 'a list -> ([> `App of 'c * 'b * 'a ] as 'b) -> 'b * 'a list
