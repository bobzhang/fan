module type S = sig
  include Map.S
  val of_list: (key * 'a) list -> 'a t 
  val of_hashtbl: (key, 'a) Hashtbl.t -> 'a t 
  val elements: 'a t  -> (key * 'a) list 
  val add_list: (key * 'a) list  -> 'a t  ->  'a t
  val find_default: default :'a -> key -> 'a t  -> 'a
  val find_opt : key -> 'a t -> 'a option
  val add_with: f :('a -> 'a -> 'a) -> key -> 'a ->  'a t  ->
    ('a t*[`NotExist | `Exist])
    (* FIXME  [~default:] [~default :] *)

  val unsafe_height: 'a t  -> int
  val unsafe_node:  'a t  -> (key * 'a) ->  'a t  ->  'a t 
      
end
      
module Make :
  functor (S : Map.OrderedType) -> S with type key= S.t

module String : S with type key = string
module Int : S with type key = int

val mk : cmp:('a -> 'a -> int) -> (module S with type key = 'a)      
        
