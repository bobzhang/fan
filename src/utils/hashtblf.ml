include Hashtbl

let keys tbl = fold (fun k _ acc -> k::acc) tbl []

let values tbl = fold (fun _ v acc -> v::acc ) tbl []

let find_default ~default tbl k =
  try find tbl k with Not_found -> default 

let find_opt tbl k =
  try Some (find tbl k) with Not_found -> None

  
let mk (type s) ~eq ~hash =
  let module M =
    struct type t = s let equal = eq let hash = hash end in
  (module Hashtbl.Make (M)  : S with type key = s)

let add_list tbl kvs =
  List.iter
    (fun (k,v) ->
      add tbl k v) kvs

let of_list kvs =
  let t = Hashtbl.create 0 in
  begin
    add_list t kvs ;
    t ;
  end
    
let memoize f =
  let cache = create 101 in
  fun v ->
    try find cache v
    with Not_found -> 
      let r = f v in
      (replace cache v r; r)
  
      

(* local variables: *)
(* compile-command: "pmake hashtblf.cmo" *)
(* end: *)
