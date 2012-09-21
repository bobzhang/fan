module Make :
 functor (Structure : Structure.S) ->
  sig
   open Structure

   val sfold0 : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) fold))

   val sfold1 : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) fold))

   val sfold0sep : (('a -> ('b -> 'b)) -> ('b -> (_, 'a, 'b) foldsep))

  end
