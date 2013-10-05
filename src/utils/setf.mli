module type S = sig
  include Set.S
  val of_list: elt list -> t
  val add_list: t ->  elt list -> t 
  val of_array: elt array -> t
  val add_array: t -> elt array -> t 
end

module Make :
  functor (S : Set.OrderedType) -> S with type elt = S.t

module Int : S with type elt = int
      
module String : S with type elt = string

val mk : cmp:('a -> 'a -> int) -> (module S with type elt = 'a)

