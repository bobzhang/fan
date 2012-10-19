open Format
let failwithf = fun fmt -> (ksprintf failwith fmt)
module MapMake =
 functor (S : Map.OrderedType) ->
  struct
   include (Map.Make)(S)
  let of_list =
   fun lst ->
    (List.fold_left ( fun acc -> fun (k , v) -> (add k v acc) ) empty lst)
  let of_hashtbl =
   fun tbl ->
    (Hashtbl.fold ( fun k -> fun v -> fun acc -> (add k v acc) ) tbl empty)
  let elements =
   fun map ->
    (fold ( fun k -> fun v -> fun acc -> ( (k , v) ) :: acc  ) map [] )
  end
module SSet = (Set.Make)(String)
module SMap = (MapMake)(String)
module IMap =
 (Set.Make)(struct type t = int let compare = Pervasives.compare end)
module ISet =
 (Set.Make)(struct type t = int let compare = Pervasives.compare end)
module Hashset =
 struct
  type 'a t = ('a , unit) Hashtbl.t
 let create = Hashtbl.create
 let add = fun set -> fun x -> (Hashtbl.replace set x () )
 let remove = Hashtbl.remove
 let mem = Hashtbl.mem
 let iter = fun f -> (Hashtbl.iter ( fun v -> fun ()  -> (f v) ))
 let fold =
  fun f -> (Hashtbl.fold ( fun v -> fun ()  -> fun st -> (f v st) ))
 let elements = Hashtbl.length
 let clear = Hashtbl.clear
 let of_list =
  fun size ->
   fun vs -> let set = (create size) in ( (List.iter ( (add set) ) vs) ); set
 let to_list = fun set -> (fold ( fun x -> fun y -> ( x ) :: y  ) set [] )
 end
let mk_set =
 fun (type s)
 ->fun ~cmp ->
    let module M = struct type t = s let compare = cmp end in
    ((module (Set.Make)(M)) : (module Set.S  with type elt = s))
let mk_map =
 fun (type s)
 ->fun ~cmp ->
    let module M = struct type t = s let compare = cmp end in
    ((module (Map.Make)(M)) : (module Map.S  with type key = s))
let mk_hashtbl =
 fun (type s)
 ->fun ~eq ->
    fun ~hash ->
     let module M = struct type t = s let equal = eq let hash = hash end in
     ((module (Hashtbl.Make)(M)) : (module Hashtbl.S  with type key = s))
let ( |> ) = fun x -> fun f -> (f x)
let ( <| ) = fun f -> fun x -> (f x)
let ( |- ) = fun f -> fun g -> fun x -> (g ( (f x) ))
let ( -| ) = fun f -> fun g -> fun x -> (f ( (g x) ))
let flip = fun f -> fun x -> fun y -> (f y x)
let ( *** ) = fun f -> fun g -> fun (x , y) -> (( (f x) ) , ( (g y) ))
let ( &&& ) = fun f -> fun g -> fun x -> (( (f x) ) , ( (g x) ))
let curry = fun f -> fun x -> fun y -> (f (x , y))
let uncurry = fun f -> fun (x , y) -> (f x y)
let const = fun x -> fun _ -> x
let tap = fun f -> fun x -> ( (f x) ); x
let is_even = fun x -> (( (x mod 2) ) == 0)
let to_string_of_printer =
 fun printer ->
  fun v ->
   let buf = (Buffer.create 30) in
   let () = (Format.bprintf buf "@[%a@]" printer v) in (Buffer.contents buf)
let zfold_left =
 fun ?(start = 0) ->
  fun ~until ->
   fun ~acc ->
    fun f ->
     let v = (ref acc) in
     for x = start to until do (v := ( (f ( v.contents ) x) )) done;
     v.contents
type 'a cont = ('a -> exn)
let callcc =
 fun (type u)
 ->fun (f :
     (u cont -> u)) ->
    let module M = struct exception Return of u end in
    (try (f ( fun x -> (raise ( (M.Return (x)) )) )) with
     M.Return (u) -> u)
module List = struct include List include BatList end
module Char = struct include BatChar end
module String = struct include String include BatString end
module Ref = struct include BatRef end
module Option = struct include BatOption end
module Buffer =
 struct
  include BatBuffer
 let ( +> ) = fun buf -> fun chr -> ( (Buffer.add_char buf chr) ); buf
 let ( +>> ) = fun buf -> fun str -> ( (Buffer.add_string buf str) ); buf
 end
module Hashtbl =
 struct
  include BatHashtbl
 let keys =
  fun tbl -> (fold ( fun k -> fun _ -> fun acc -> ( k ) :: acc  ) tbl [] )
 let values =
  fun tbl -> (fold ( fun _ -> fun v -> fun acc -> ( v ) :: acc  ) tbl [] )
 end
module Stream =
 struct
  include BatStream
 include Stream
 let rev =
  fun strm ->
   let rec aux =
    fun (__strm :
      _ Stream.t) ->
     (match (Stream.peek __strm) with
      | Some (x) ->
         (
         (Stream.junk __strm)
         );
         let xs = __strm in
         (Stream.lapp ( fun _ -> (aux xs) ) ( (Stream.ising x) ))
      | _ -> Stream.sempty) in
   (aux strm)
 let tail =
  fun (__strm :
    _ Stream.t) ->
   (match (Stream.peek __strm) with
    | Some (_) -> ( (Stream.junk __strm) ); __strm
    | _ -> Stream.sempty)
 let rec map =
  fun f ->
   fun (__strm :
     _ Stream.t) ->
    (match (Stream.peek __strm) with
     | Some (x) ->
        (
        (Stream.junk __strm)
        );
        let xs = __strm in
        (Stream.lcons ( fun _ -> (f x) ) (
          (Stream.slazy ( fun _ -> (map f xs) )) ))
     | _ -> Stream.sempty)
 end
module ErrorMonad =
 struct
  type log = string
 type 'a result = Left of 'a | Right of log
 let return = fun x -> (Left (x))
 let fail = fun x -> (Right (x))
 let ( >>= ) =
  fun ma ->
   fun f ->
    (match ma with | Left (v) -> (f v) | Right (x) -> (Right (x)))
 let bind = (>>=)
 let map =
  fun f -> function | Left (v) -> (Left (f v)) | Right (s) -> (Right (s))
 let ( >>| ) =
  fun ma ->
   fun (str , f) ->
    (match ma with | Left (v) -> (f v) | Right (x) -> (Right (x ^ str)))
 let ( >>? ) =
  fun ma ->
   fun str ->
    (match ma with | Left (_) -> ma | Right (x) -> (Right (x ^ str)))
 let ( <|> ) =
  fun fa ->
   fun fb ->
    fun a ->
     (match (fa a) with
      | (Left (_) as x) -> x
      | Right (str) -> (( (fb a) ) >>? str))
 let unwrap =
  fun f ->
   fun a ->
    (match (f a) with | Left (res) -> res | Right (msg) -> (failwith msg))
 let mapi_m =
  fun f ->
   fun xs ->
    let rec aux =
     fun acc ->
      fun xs ->
       (match xs with
        | [] -> (return [] )
        | (x :: xs) ->
           (( (f x acc) ) >>= (
             fun x ->
              (( (aux ( (acc + 1) ) xs) ) >>= (
                fun xs -> (return ( ( x ) :: xs  )) )) ))) in
    (aux 0 xs)
 end