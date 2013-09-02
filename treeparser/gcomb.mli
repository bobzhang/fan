


(**  the output is reversed, you have to reverse the list output
   if you care about the order *)  
val slist0 :
  f:('a list -> 'b) ->
  ('c XStream.t -> 'a) -> 'c XStream.t -> 'b


      
val slist1 :
  f:('a list -> 'b) ->
  ('c XStream.t -> 'a) -> 'c XStream.t -> 'b
val slist0sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d XStream.t -> 'b) ->
  ('d XStream.t -> 'a) -> 'd XStream.t -> 'c
val slist1sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d XStream.t -> 'b) ->
  ('d XStream.t -> 'a) -> 'd XStream.t -> 'c
val opt :
  ('a XStream.t -> 'b) ->
  f:('b option -> 'c) -> 'a XStream.t -> 'c
val tryp : ('a XStream.t -> 'b) -> 'a XStream.t -> 'b
val peek : ('a XStream.t -> 'b) -> 'a XStream.t -> 'b
val orp :
  ?msg:string ->
  ('a XStream.t -> 'b) ->
  ('a XStream.t -> 'b) -> 'a XStream.t -> 'b
