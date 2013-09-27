


(**  the output is reversed, you have to reverse the list output
   if you care about the order *)  
val slist0 :
  f:('a list -> 'b) ->
  ('c Fstream.t -> 'a) -> 'c Fstream.t -> 'b


      
val slist1 :
  f:('a list -> 'b) ->
  ('c Fstream.t -> 'a) -> 'c Fstream.t -> 'b
val slist0sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d Fstream.t -> 'b) ->
  ('d Fstream.t -> 'a) -> 'd Fstream.t -> 'c
val slist1sep :
  err:('a -> string) ->
  f:('b list -> 'c) ->
  ('d Fstream.t -> 'b) ->
  ('d Fstream.t -> 'a) -> 'd Fstream.t -> 'c
val opt :
  ('a Fstream.t -> 'b) ->
  f:('b option -> 'c) -> 'a Fstream.t -> 'c
val tryp : ('a Fstream.t -> 'b) -> 'a Fstream.t -> 'b
val peek : ('a Fstream.t -> 'b) -> 'a Fstream.t -> 'b
val orp :
  ?msg:string ->
  ('a Fstream.t -> 'b) ->
  ('a Fstream.t -> 'b) -> 'a Fstream.t -> 'b
