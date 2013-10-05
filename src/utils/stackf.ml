
type 'a t  = {
    mutable elts : 'a list;
    mutable length :  int;
  }

exception Empty

let invariant t =
  assert (t.length = List.length t.elts)

let create () = { elts = []; length = 0; }

    (* We always want to set elts and length at the same time.  Having a function
     * to do so helps us to remember. *)
let set t elts length =
  begin
    t.elts <- elts;
    t.length <- length
  end

let push x t = set t (x :: t.elts) (t.length + 1)

let pop_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: l -> (set t l (t.length - 1); x)

let pop t = try Some (pop_exn t) with |Empty -> None

let top_exn t =
  match t.elts with
  | [] -> raise Empty
  | x :: _ -> x

let top t = try Some (top_exn t) with |Empty -> None

let clear t = set t [] 0

let copy t = { elts = t.elts; length = t.length; }

let length t = t.length

let is_empty t = t.length = 0

let iter t ~f = List.iter f t.elts 

let fold t ~init ~f = List.fold_left f init t.elts

    (* let fold_n_pop n ~init ~f = *)
    (*   let aux n *)

let topn_rev n t =
  Flist.take_rev n t.elts
    
let exists t ~f = List.exists f t.elts 

let for_all t ~f = List.for_all f t.elts 

let find_map t ~f = Flist.find_map f t.elts 

let to_list t = t.elts

let of_list l = { elts = l; length = List.length l }

let to_array t = Array.of_list t.elts

let until_empty t f =
  let rec loop () = if t.length > 0 then (f (pop_exn t); loop ()) in
  loop ()



(* local variables: *)
(* compile-command: "pmake stackf.cmo" *)
(* end: *)
