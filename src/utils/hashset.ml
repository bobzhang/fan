
type 'a t  =  ('a,unit)Hashtbl.t

let create = Hashtbl.create

let add set x = Hashtbl.replace set x ()

let remove = Hashtbl.remove

let mem = Hashtbl.mem

let iter f = Hashtbl.iter (fun v () -> f v)

let fold f = Hashtbl.fold (fun v () st -> f v st)

let elements = Hashtbl.length

let clear = Hashtbl.clear

let of_list ?(size=100) vs = 
  let set = create size in 
  List.iter (add set) vs;
  set


let add_list set vs =
  List.iter (add set) vs

let to_list set = fold (fun x y -> x::y) set []



(* local variables: *)
(* compile-command: "pmake hashset.cmo" *)
(* end: *)
