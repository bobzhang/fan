module type S = sig
  include Map.S
  val of_list: (key * 'a) list -> 'a t
  val of_hashtbl:(key,'a) Hashtbl.t  -> 'a t
  val elements: 'a t -> (key * 'a) list 
  val add_list: (key * 'a) list  -> 'a t -> 'a t
  val find_default: default :'a -> key -> 'a t -> 'a 
  val find_opt: key -> 'a t -> 'a option
    (* FIXME  [~default:] [~default :] *)
  val add_with: f :('a -> 'a -> 'a) -> key ->
    'a ->  'a t ->
      ('a t * [ `NotExist | `Exist])

  val unsafe_height: 'a t -> int
  val unsafe_node:  'a t -> (key * 'a) ->  'a t ->  'a t
end

module Make(S:Map.OrderedType) : S with type key = S.t = struct
  include Map.Make (S) (* TODO: the same syntax with original *)
  let of_list lst =
    List.fold_left (fun acc (k,v)  -> add k v acc) empty lst
  let add_list lst base =
    List.fold_left (fun acc (k,v) -> add k v acc) base lst
  let of_hashtbl tbl =
    Hashtbl.fold (fun k v acc -> add k v acc) tbl empty
  let elements map =
    fold (fun k v acc ->  (k,v) :: acc ) map [] 
  let find_default ~default k m =
    try find k m with Not_found -> default

  (* can be more efficient if we break the abstraction *)    
  let add_with ~f k v s =
     try (add k (f  (find k s) v) s, `Exist) with Not_found -> (add k v s,`NotExist)

  let unsafe_height (l:'a t) : int =
    if l = empty then 0
    else (Obj.magic (Obj.field (Obj.repr l)  4) :int)  

  (* this is unsafe, since the difference between the height of [l] and
     the height of [r] may exceed 1
   *)  
  let unsafe_node (l:'a t) ((k:key),(v:'a)) (r:'a t) =
    let h = max (unsafe_height l) (unsafe_height  r) + 1 in
    let o = Obj.new_block 0 4 in begin 
      Obj.set_field o 0 (Obj.repr l);
      Obj.set_field o 1 (Obj.repr k);
      Obj.set_field o 2 (Obj.repr v);
      Obj.set_field o 3 (Obj.repr r);
      Obj.set_field o 4 (Obj.repr h);
      (Obj.magic o : 'a t)
    end
    
  let find_opt k m =
    try Some (find k m) with Not_found -> None

end 

module String = Make (struct
  type t = string
  let compare (x:string) y = Pervasives.compare x y
end)

  
module Int = Make (struct
  type t = int
  let compare (x:int) y = Pervasives.compare x y
end)



let mk (type s) ~cmp=
  let module M = struct type t = s let compare = cmp end in
  (module Make (M) : S with type key = s)

    
(* local variables: *)
(* compile-command: "pmake mapf.cmo" *)
(* end: *)
