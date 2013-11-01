


(**  the output is reversed, you have to reverse the list output
   if you care about the order *)  
val slist0 :
  f:('a list -> 'b) ->
  ('c Streamf.t -> 'a) -> 'c Streamf.t -> 'b


      
val slist1 :
  f:('a list -> 'b) ->
  ('c Streamf.t -> 'a) -> 'c Streamf.t -> 'b
val slist0sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d Streamf.t -> 'b) ->
  ('d Streamf.t -> 'a) -> 'd Streamf.t -> 'c
val slist1sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d Streamf.t -> 'b) ->
  ('d Streamf.t -> 'a) -> 'd Streamf.t -> 'c

val tryp : ('a Streamf.t -> 'b) -> 'a Streamf.t -> 'b

val peek : ('a Streamf.t -> 'b) -> 'a Streamf.t -> 'b

val orp :
  ?msg:string ->
  ('a Streamf.t -> 'b) ->
  ('a Streamf.t -> 'b) -> 'a Streamf.t -> 'b
