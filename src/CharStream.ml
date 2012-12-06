


(* The fields of type t are not mutable to preserve polymorphism of
   the empty stream. This is type safe because the empty stream is never
   patched. *)

type t = { count : int; data :  data }
and  data =
    Sempty
  | Scons of char *  data
  | Sapp of  data *  t
  | Slazy of  t Lazy.t
  | Sgen of  gen
  | Sbuffio of buffio
and  gen = { mutable curr : char option option; func : int -> char option }
and buffio =
  { ic : in_channel; buff : string; mutable len : int; mutable ind : int }
;;

exception Failure;;

exception Error of string;;

external count :  t -> int = "%field0";;

external set_count : t -> int -> unit = "%setfield0";;

let set_data (s :  t) (d : data) = 
  Obj.(set_field (repr s) 1 (repr d))
;;

let fill_buff b =
  b.len <- input b.ic b.buff 0 (String.length b.buff); b.ind <- 0



(* Only return a "forced stream", that is either Sempty or
   Scons(a,_). If d is a generator or a buffer, the item a is seen as
   extracted from the generator/buffer.
   Forcing also updates the "count" field of the delayed stream,
   in the Sapp and Slazy cases (see slazy/lapp implementation below). *)
let (* get_data *)peek s =
  let rec aux d = 
    match d with
    | Sempty -> None,d
    | Scons (a, _) -> Some a, d
    | Sapp (d1, s2) ->
        (match aux d1 with
        | _, Scons (a, d11) ->
            let v = Scons (a, Sapp (d11, s2)) in
            set_data s v;
            (Some a ,v)
        | _, Sempty ->
            set_count s s2.count; (* FIXME *)
            let (_,v) as res = aux s2.data in
            set_data s v;
            res
        | _ -> assert false)
    | Sgen {curr = Some None; _ } -> set_data s Sempty; (None,Sempty)
    | Sgen ({curr = Some(Some a); _ } as g) ->
        g.curr <- None; let v = Scons(a, d) in set_data s v; (Some a, v)
    | Sgen ({curr = None; _} as g) ->
        (* Warning: anyone using g thinks that an item has been read *)
        (match g.func s.count with
          None -> g.curr <- Some(None); set_data s Sempty; (None,Sempty)
        | Some a ->
            (* One must not update g.curr here, because there Scons(a,d)
               result of get_data, if the outer stream s was a Sapp, will
               be used to update the outer stream to Scons(a,s): there is
               already a memoization process at the outer layer. If g.curr
               was updated here, the saved element would be produced twice,
               once by the outer layer, once by Sgen/g.curr.
               [Don't set here], to keep side effect happens next time 
             *)
            (Some a,Scons(a, d)))
    | Sbuffio b ->
        if b.ind >= b.len then fill_buff b;
        if b.len == 0 then (None,Sempty) else
        let r = Obj.magic (String.unsafe_get b.buff b.ind) in
        (* Warning: anyone using g thinks that an item has been read *)
        b.ind <- succ b.ind; let v = Scons (r,d) in set_data s v ; (Some r, v)
    | Slazy f ->
        let s2 = Lazy.force f in
        set_count s s2.count;
        let (_,v) as res  = aux  s2.data in
        set_data s v; res in
  (fst (aux s.data))
;;

(* let rec peek s = *)
(*  (\* consult the first item of s *\) *)
(*  match s.data with *)
(*    Sempty -> None *)
(*  | Scons (a, _) -> Some a *)
(*  | Sapp (_, _) -> *)
(*      ( match get_data s  with *)
(*      | Scons(a, _) as d -> set_data s d; Some a (\* get and set to cache*\) *)
(*      | Sempty -> None *)
(*      | _ -> assert false) *)

(*  | Slazy f -> *)
(*    let s2 = Lazy.force f in *)
(*    set_count s s2.count; *)
(*    set_data s s2.data; *)
(*    peek s *)
(*  | Sgen {curr = Some a; _ } -> a *)
(*  | Sgen ({curr = None; _ } as g) -> *)
(*      let x = g.func s.count in *)
(*      g.curr <- Some x; x *)
(*  | Sbuffio b -> *)
(*      if b.ind >= b.len then fill_buff b; *)
(*      if b.len == 0 then begin set_data s Sempty; None end *)
(*      else Some (Obj.magic (String.unsafe_get b.buff b.ind)) *)
(* ;; *)

let rec junk  s =
  match s.data with
    Scons (_, d) -> set_count s (succ s.count); set_data s d
  | Sgen ({curr = Some _;_} as g) -> set_count s (succ s.count); g.curr <- None
  | Sbuffio b -> set_count s (succ s.count); b.ind <- succ b.ind
  | _ ->
      match peek s with
        None -> ()
      | Some _ -> junk s
;;

let rec nget n s =
  if n <= 0 then [], s.data, 0
  else
    match peek s with
      Some a ->
        junk s;
        let (al, d, k) = nget (pred n) s in a :: al, Scons (a, d), succ k
    | None -> [], s.data, 0
;;

let npeek n s =
  let (al, d, len) = nget n s in set_count s (s.count - len); set_data s d; al
;;

let next s =
  match peek s with
    Some a -> junk s; a
  | None -> raise Failure
;;

let empty s =
  match peek s with
    Some _ -> raise Failure
  | None -> ()
;;

let iter f strm =
  let rec do_rec () =
    match peek strm with
      Some a -> junk strm; ignore(f a); do_rec ()
    | None -> ()
  in
  do_rec ()
;;

(* Stream building functions *)

let from f = {count = 0; data = Sgen {curr = None; func = f}};;

let of_list l =
  {count = 0; data = List.fold_right (fun x l -> Scons (x, l)) l Sempty}
;;

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)
;;

let of_channel ic =
  {count = 0;
   data = Sbuffio {ic = ic; buff = String.create 4096; len = 0; ind = 0}}
;;

(* Stream expressions builders *)

(* In the slazy and lapp case, we can't statically predict the value
   of the "count" field. We put a dummy 0 value, which will be updated
   when the parameter stream is forced (see update code in [get_data]
   and [peek]). *)

let ising i = {count = 0; data = Scons (i, Sempty)};;
let icons i s = {count = s.count - 1; data = Scons (i, s.data)};;
let iapp i s = {count = i.count; data = Sapp (i.data, s)};;

let sempty = {count = 0; data = Sempty};;
let slazy f = {count = 0; data = Slazy (lazy (f()))};;

let lsing f = {count = 0; data = Slazy (lazy (ising (f())))};;
let lcons f s = {count = 0; data = Slazy (lazy (icons (f()) s))};;
let lapp f s = {count = 0; data = Slazy (lazy(iapp (f()) s))};;


(* [< '(f a); 'b; 'c >]
   [< '(f a)>]
   [< print_int 3 >]
   let s =[<'u ;'u>];
 *)
(* For debugging use *)

let rec dump f s =
  print_string "{count = ";
  print_int s.count;
  print_string "; data = ";
  dump_data f s.data;
  print_string "}";
  print_newline ()
and dump_data f =
  function
    Sempty -> print_string "Sempty"
  | Scons (a, d) ->
      print_string "Scons (";
      f a;
      print_string ", ";
      dump_data f d;
      print_string ")"
  | Sapp (d1, s2) ->
      print_string "Sapp (";
      dump_data f d1;
      print_string ", ";
      dump f s2;
      print_string ")"
  | Slazy _ -> print_string "Slazy"
  | Sgen _ -> print_string "Sgen"
  | Sbuffio _b -> print_string "Sbuffio"
;;


(* class ['a] stream ~data:(data:'a t) = object *)
(*   val mutable data = data  *)
(*   method iter (f:'a ->unit) =  iter f data *)
(*   method next = next data *)
(*   method empty = empty data *)
(*   method peek = peek data *)
(*   method junk = junk data *)
(*   method count = count data *)
(*   method npeek i = npeek i data *)
(*   method data = data *)
(*   method iapp (s2: 'a stream) = *)
(*     data <- iapp data s2#data *)
(*   method icons v = *)
(*     data <- icons v data *)
(*   method ising v = *)
(*     data <- ising v *)
(*   (\* method lapp  *\) *)
(* (\* new stream ~data:(iapp s1#data s2#data) *\) *)
(*   (\* method icons v s = new stream (icons v s#data) *\) *)
(*   (\* method lapp f s = new stream (lapp f s#data) *\) *)
(*   (\* method from = XStream.from *\) *)
(* end;; *)
