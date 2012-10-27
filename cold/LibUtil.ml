open Format
let failwithf (fmt) = (ksprintf failwith fmt)
module MapMake(S:Map.OrderedType) = struct
  include Map.Make(S)
  let of_list (lst) =
    (List.fold_left ( (fun (acc) -> (fun ((k,v)) -> (add k v acc))) ) empty
      lst)
  let of_hashtbl (tbl) =
    (Hashtbl.fold ( (fun (k) -> (fun (v) -> (fun (acc) -> (add k v acc)))) )
      tbl empty)
  let elements (map) =
    (fold ( (fun (k) -> (fun (v) -> (fun (acc) -> (k,v)::acc))) ) map [] )
  end
module SSet = Set.Make(String)
module SMap = MapMake(String)
module IMap = Set.Make(struct
  type t =  int   let compare = Pervasives.compare
  end)
module ISet = Set.Make(struct
  type t =  int   let compare = Pervasives.compare
  end)
module Hashset = struct
  type 'a t = ('a, unit ) Hashtbl.t   let create = Hashtbl.create
  let add (set) (x) = (Hashtbl.replace set x () ) let remove = Hashtbl.remove
  let mem = Hashtbl.mem
  let iter (f) = (Hashtbl.iter ( (fun (v) -> (fun (() ) -> (f v))) ))
  let fold (f) =
    (Hashtbl.fold ( (fun (v) -> (fun (() ) -> (fun (st) -> (f v st)))) ))
  let elements = Hashtbl.length let clear = Hashtbl.clear
  let of_list (size) (vs) =
    let set = (create size) in begin
      (List.iter ( (add set) ) vs);
      set
      end
  let to_list (set) = (fold ( (fun (x) -> (fun (y) -> x::y)) ) set [] )
  end
let mk_set (type s) ~cmp  =
  let module M = struct
    type t =  s   let compare = cmp
    end in ((module Set.Make(M)) :(module Set.S with type elt =  s ) )
let mk_map (type s) ~cmp  =
  let module M = struct
    type t =  s   let compare = cmp
    end in ((module Map.Make(M)) :(module Map.S with type key =  s ) )
let mk_hashtbl (type s) ~eq  ~hash  =
  let module M = struct
    type t =  s   let equal = eq let hash = hash
    end in
    ((module Hashtbl.Make(M)) :(module Hashtbl.S with type key =  s ) )
let (|>) (x) (f) = (f x)
let (<|) (f) (x) = (f x)
let (|-) (f) (g) (x) = (g ( (f x) ))
let (-|) (f) (g) (x) = (f ( (g x) ))
let flip (f) (x) (y) = (f y x)
let ( *** )  (f) (g) ((x,y)) = (( (f x) ),( (g y) ))
let (&&&) (f) (g) (x) = (( (f x) ),( (g x) ))
let curry (f) (x) (y) = (f (x,y))
let uncurry (f) ((x,y)) = (f x y)
let const (x) (_) = x
let tap (f) (x) = begin
  (f x);
  x
  end
let is_even (x) = (( (x mod 2) ) == 0)
let to_string_of_printer (printer) (v) =
  let buf = (Buffer.create 30) in
  let ()  = (Format.bprintf buf "@[%a@]" printer v) in (Buffer.contents buf)
let zfold_left ?(start=0)  ~until  ~acc  (f) =
  let v = (ref acc) in
  begin
    for x = start to  until do (v := ( (f ( v.contents ) x) )) done;
    v.contents
    end
type 'a cont = ('a ->  exn )  
let callcc (type u) ((f : ( u  cont  ->  u ) )) =
  let module M = struct
    exception Return of  u 
    end in begin try
    (f ( (fun (x) -> (raise ( M.Return (x) ))) ))
    with
    | M.Return(u) ->   u end
module List = struct
  include List include BatList
  end
module Char = struct
  include BatChar
  end
module String = struct
  include String include BatString
  end
module Ref = struct
  include BatRef
  end
module Option = struct
  include BatOption
  end
module Buffer = struct
  include BatBuffer
  let (+>) (buf) (chr) = begin
    (Buffer.add_char buf chr);
    buf
    end let (+>>) (buf) (str) = begin
          (Buffer.add_string buf str);
          buf
          end
  end
module Hashtbl = struct
  include BatHashtbl
  let keys (tbl) =
    (fold ( (fun (k) -> (fun (_) -> (fun (acc) -> k::acc))) ) tbl [] )
  let values (tbl) =
    (fold ( (fun (_) -> (fun (v) -> (fun (acc) -> v::acc))) ) tbl [] )
  end
module Stream = struct
  include BatStream include Stream
  let rev (strm) =
    let rec aux ((__strm : _ Stream.t )) = begin match (Stream.peek __strm)
      with
      | Some(x) ->
          begin
          (Stream.junk __strm);
          let xs = __strm in
          (Stream.lapp ( (fun (_) -> (aux xs)) ) ( (Stream.ising x) ))
          end
      | _ ->   Stream.sempty end in
    (aux strm)
  let tail ((__strm : _ Stream.t )) = begin match (Stream.peek __strm) with
    | Some(_) ->   begin
                   (Stream.junk __strm);
                   __strm
                   end
    | _ ->   Stream.sempty end
  let rec map (f) ((__strm : _ Stream.t )) = begin match (Stream.peek __strm)
    with
    | Some(x) ->
        begin
        (Stream.junk __strm);
        let xs = __strm in
        (Stream.lcons ( (fun (_) -> (f x)) ) (
          (Stream.slazy ( (fun (_) -> (map f xs)) )) ))
        end
    | _ ->   Stream.sempty end
  end
module ErrorMonad = struct
  type log =  string   type 'a result =  
                         | Left of 'a
                         | Right of  log   let return (x) = Left (x)
  let fail (x) = Right (x)
  let (>>=) (ma) (f) = begin match ma with
    | Left(v) ->   (f v)
    | Right(x) ->   Right (x) end let bind = (>>=)
  let map (f) =
    (function
    | Left(v) ->   Left ((f v))
    | Right(s) ->   Right (s))
  let (>>|) (ma) ((str,f)) = begin match ma with
    | Left(v) ->   (f v)
    | Right(x) ->   Right ((x ^ str)) end
  let (>>?) (ma) (str) = begin match ma with
    | Left(_) ->   ma
    | Right(x) ->   Right ((x ^ str)) end
  let (<|>) (fa) (fb) (a) = begin match (fa a) with
    | (Left(_) as x) ->   x
    | Right(str) ->   (( (fb a) ) >>? str) end
  let unwrap (f) (a) = begin match (f a) with
    | Left(res) ->   res
    | Right(msg) ->   (failwith msg) end
  let mapi_m (f) (xs) =
    let rec aux (acc) (xs) = begin match xs with
      | []  ->   (return [] )
      | x::xs ->
          (( (f x acc) ) >>= (
            (fun (x) ->
              (( (aux ( (acc + 1) ) xs) ) >>= (
                (fun (xs) -> (return ( x::xs ))) ))) ))
      end in
    (aux 0 xs)
  end