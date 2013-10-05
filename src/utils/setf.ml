module type S = sig
  include Set.S
  val of_list: elt list -> t
  val add_list: t ->  elt list -> t 
  val of_array: elt array -> t
  val add_array: t -> elt array -> t 
end

module Make(S:Set.OrderedType) : S with type elt = S.t =
  struct
    include Set.Make (S)
    let of_list = List.fold_left (fun x y ->  add y x ) empty
    let add_list c = List.fold_left (fun x y -> add y x) c
    let of_array = Array.fold_left (fun x y -> add y x ) empty
    let add_array c = Array.fold_left (fun x y -> add y x) c
  end

module String =Make (struct
  type t = string
  let compare (x:string) y = Pervasives.compare x y
end)


module Int = Make
    (struct
      type t = int
      let compare (x:int) y = Pervasives.compare x y 
    end)      

let mk (type s) ~cmp =
  let module M = struct type t = s let compare = cmp end in
  (module Make (M) :S with type elt = s)    


(* local variables: *)
(* compile-command: "pmake setf.cmo" *)
(* end: *)
