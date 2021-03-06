(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*        Daniel de Rauglaudre, projet Cristal, INRIA Rocquencourt     *)
(*                                                                     *)
(*  Copyright 1997 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the GNU Library General Public License, with    *)
(*  the special exception on linking described in file ../LICENSE.     *)
(*                                                                     *)
(***********************************************************************)

(* The fields of type t are not mutable to preserve polymorphism of
   the empty stream. This is type safe because the empty stream is never
   patched. *)

type 'a t = { count : int; data : 'a data; (* mutable *) last: 'a option }
and 'a data =
  |  Sempty
  | Scons of 'a * 'a data
  | Sapp of 'a data * 'a t
  | Slazy of 'a t Lazy.t
  | Sgen of 'a gen
  | Sbuffio of buffio
and 'a gen = { mutable curr : 'a option option; func : int -> 'a option }
and buffio =
  { ic : in_channel; buff : string; mutable len : int; mutable ind : int }
;;
exception NotConsumed;;
exception Error of string;;

external count : 'a t -> int = "%field0";;
external set_count : 'a t -> int -> unit = "%setfield0";;
let set_last (s: 'a t) (v:'a option) =
  Obj.set_field (Obj.repr s) 2 (Obj.repr v);;
let set_data (s : 'a t) (d : 'a data) =
  Obj.set_field (Obj.repr s) 1 (Obj.repr d)
;;

let fill_buff b =
  (b.len <- input b.ic b.buff 0 (String.length b.buff); b.ind <- 0)
;;

let rec get_data s d = match d with
 (* Only return a "forced stream", that is either Sempty or
    Scons(a,_). If d is a generator or a buffer, the item a is seen as
    extracted from the generator/buffer.

    Forcing also updates the "count" field of the delayed stream,
    in the Sapp and Slazy cases (see slazy/lapp implementation below). *)
 | Sempty | Scons (_, _) -> d
 | Sapp (d1, s2) ->
     begin match get_data s d1 with
     | Scons (a, d11) -> Scons (a, Sapp (d11, s2))
     | Sempty ->
       (set_count s s2.count;
       get_data s s2.data)
     | _ -> assert false
     end
 | Sgen {curr = Some None; _ } -> Sempty
 | Sgen ({curr = Some(Some a); _ } as g) ->
     g.curr <- None; Scons(a, d)
 | Sgen ({curr = None; _} as g) ->
     (* Warning: anyone using g thinks that an item has been read *)
     begin match g.func s.count with
       None -> g.curr <- Some(None); Sempty
     | Some a ->
       (* One must not update g.curr here, because there Scons(a,d)
          result of get_data, if the outer stream s was a Sapp, will
          be used to update the outer stream to Scons(a,s): there is
          already a memoization process at the outer layer. If g.curr
          was updated here, the saved element would be produced twice,
          once by the outer layer, once by Sgen/g.curr. *)
       Scons(a, d)
     end
 | Sbuffio b ->
     if b.ind >= b.len then fill_buff b;
     if b.len == 0 then Sempty else
       let r = Obj.magic (String.unsafe_get b.buff b.ind) in
       (* Warning: anyone using g thinks that an item has been read *)
       b.ind <- succ b.ind; Scons(r, d)
 | Slazy f ->
   let s2 = Lazy.force f in
   set_count s s2.count;
   get_data s s2.data
;;

let rec peek s =
 (* consult the first item of s *)
 match s.data with
   Sempty -> None
 | Scons (a, _) -> Some a
 | Sapp (_, _) ->
     begin match get_data s s.data with
     | Scons(a, _) as d -> set_data s d; Some a
     | Sempty -> None
     | _ -> assert false
     end
 | Slazy f ->
   let s2 = Lazy.force f in
   set_count s s2.count;
   set_data s s2.data;
   peek s
 | Sgen {curr = Some a; _ } -> a
 | Sgen ({curr = None; _ } as g) ->
     let x = g.func s.count in
     g.curr <- Some x; x
 | Sbuffio b ->
     if b.ind >= b.len then fill_buff b;
     if b.len == 0 then begin set_data s Sempty; None end
     else Some (Obj.magic (String.unsafe_get b.buff b.ind))
;;
(* FIXME we need peek first then junk for Sbuffio *)
let rec junk s =
  match s.data with
    Scons (a, d) -> set_count s (succ s.count); set_data s d; set_last s (Some a) ;
  | Sgen ({curr = Some a; _ } as g) ->
      set_count s (succ s.count); g.curr <- None; set_last s a;
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then
        ()
      else begin 
        set_count s (succ s.count);
        set_last s (Some (Obj.magic b.buff.[b.ind(* s.count - 1 *)]));
        b.ind <- succ b.ind;
      end

  | _ ->
      match peek s with
        None -> ()
      | Some _ -> junk s
;;

(* don't set last field *)
let rec fake_junk s =
  match s.data with
    Scons (_a, d) -> set_count s (succ s.count); set_data s d; (* set_last s (Some a) ; *)
  | Sgen ({curr = Some _a; _ } as g) ->
      set_count s (succ s.count); g.curr <- None; (* set_last s a; *)
  | Sbuffio b ->
      if b.ind >= b.len then fill_buff b;
      if b.len == 0 then
        ()
      else begin 
        set_count s (succ s.count);
        (* set_last s (Some (Obj.magic b.buff.[s.count - 1])); *)
        b.ind <- succ b.ind;
      end
  | _ ->
      match peek s with
        None -> ()
      | Some _ -> fake_junk s
;;

(* Used only in npeek, and call [fake_junk] *)
let rec nget n s =
  if n <= 0 then [], s.data, 0
  else
    match peek s with
      Some a ->
        fake_junk s;
        let (al, d, k) = nget (pred n) s in a :: al, Scons (a, d), succ k
    | None -> [], s.data, 0
;;

let npeek n s =
  let (al, d, len) = nget n s in set_count s (s.count - len); set_data s d; al
;;

let next s =
  match peek s with
    Some a -> junk s; a
  | None -> raise NotConsumed
;;

let empty s =
  match peek s with
    Some _ -> raise NotConsumed
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

let from f = {count = 0; data = Sgen {curr = None; func = f}; last = None};;

let of_list l =
  {count = 0; data = List.fold_right (fun x l -> Scons (x, l)) l Sempty; last = None}
;;
(* add a generator for array *)
let of_array l =
  {count = 0; data = Array.fold_right (fun x l -> Scons (x, l)) l Sempty; last = None};;

let of_string s =
  from (fun c -> if c < String.length s then Some s.[c] else None)
;;

let of_channel ic =
  {count = 0;
   data = Sbuffio {ic = ic; buff = String.create 4096; len = 0; ind = 0};
   last = None
 }
;;

(* Stream expessions builders *)

(* In the slazy and lapp case, we can't statically predict the value
   of the "count" field. We put a dummy 0 value, which will be updated
   when the parameter stream is forced (see update code in [get_data]
   and [peek]). *)

let ising i = {count = 0; data = Scons (i, Sempty); last = None};;
let icons i s = {count = s.count - 1; data = Scons (i, s.data); last = None};;
let iapp i s = {count = i.count; data = Sapp (i.data, s); last = None};;

let sempty = {count = 0; data = Sempty; last = None};;
let slazy f = {count = 0; data = Slazy (lazy (f())); last = None};;

let lsing f = {count = 0; data = Slazy (lazy (ising (f()))); last = None};;
let lcons f s = {count = 0; data = Slazy (lazy (icons (f()) s)); last = None};;
let lapp f s = {count = 0; data = Slazy (lazy(iapp (f()) s)); last = None};;

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

let get_last v = v.last;;



let njunk  n strm  =
  for _i = 1 to n do
    junk strm
  done (* FIXME unsed  index i*)
    
let tail s = 
  match peek s with
  | Some _ -> (junk s; s)
  | _ -> sempty
        

let rev strm=
  let rec aux s =
    match peek s with
    | Some x ->
        (junk s;
         (let xs = s in
         lapp (fun _  -> aux xs) (ising x)))
    | _ -> sempty in aux strm
    
let rec map f  s = 
  match peek s with
  | Some x ->
      (junk s;
       (let xs = s in
       lcons (fun _  -> f x) (slazy (fun _  -> map f xs))))
  | _ -> sempty

        (* the minimual [n] is 0 *)
let peek_nth strm n   =
  let rec loop i = function
    | x :: xs -> if i = 0 then Some x else loop (i - 1) xs
    | [] -> None  in
  if n < 0 then
    invalid_arg "Streamf.peek_nth"
  else loop n (npeek (n+1) strm)

      (*  Used by [try_parser], very in-efficient 
          This version of peek_nth is off-by-one from Streamf.peek_nth *)      
let dup strm = from (peek_nth strm)
    
let rec filter f s =
  match peek s with
  | Some x ->
      (junk s;
       (let xs = s in
       if f x
       then icons x (slazy (fun _  -> filter f xs))
       else slazy (fun _  -> filter f xs)))
  | _ -> sempty
        

