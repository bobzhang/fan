
open Format;
let failwithf fmt = ksprintf failwith fmt  ;
let prerr_endlinef fmt = ksprintf prerr_endline fmt  ;
module MapMake(S:Map.OrderedType) = struct
  include Map.Make S;
  let of_list lst =
    List.fold_left (fun acc (k,v)  -> add k v acc) empty lst;
  let of_hashtbl tbl =
    Hashtbl.fold (fun k v acc -> add k v acc) tbl empty;
  let elements map =
    fold (fun k v acc ->  [ (k,v) :: acc] ) map [] ;
end ;

module SSet = Set.Make String;
module SMap = MapMake String;
module IMap = Set.Make (struct
  type t = int;
  let compare = Pervasives.compare ; 
end);

module ISet = Set.Make(struct
 type t = int;
 let compare = Pervasives.compare;
end);


module Hashset = struct
  type t 'a =  Hashtbl.t 'a unit;
  let create = Hashtbl.create;
  let add set x = Hashtbl.replace set x ();
  let remove = Hashtbl.remove;
  let mem = Hashtbl.mem;
  let iter f = Hashtbl.iter (fun v () -> f v);
  let fold f = Hashtbl.fold (fun v () st -> f v st);
  let elements = Hashtbl.length;
  let clear = Hashtbl.clear;
  let of_list ?(size=100) vs = 
    let set = create size in begin
      List.iter (add set) vs;
      set
    end;
  let add_list set vs =
    List.iter (add set) vs;
  let to_list set = fold (fun x y -> [x::y]) set [];
end ;

let mk_set (type s) ~cmp =
  let module M = struct type t = s; let compare = cmp; end in
  (module Set.Make M :Set.S with type elt = s);

let mk_map (type s) ~cmp=
  let module M = struct type t = s; let compare = cmp; end in
  (module Map.Make M : Map.S with type key = s);
  
let mk_hashtbl (type s) ~eq ~hash =
  let module M=struct type t = s; let equal = eq; let hash = hash; end
  in  (module Hashtbl.Make M  :Hashtbl.S with type key = s);
  
(** {6 Operators}*)
let ( |> ) x f = f x;

let ( <| ) f x = f x;

let ( |- ) f g x = g (f x);

let ( -| ) f g x = f (g x);

let flip f x y = f y x;

let ( *** ) f g = fun (x,y) -> (f x, g y);

let ( &&& ) f g = fun x -> (f x, g x);

let curry f x y = f (x,y);

let uncurry f (x,y) = f x y;

let const x _ = x;

let tap f x = begin f x; x end ;


let is_even x = x mod 2 == 0;


let pp = fprintf;  
(*
  {[
  to_string_of_printer pp_print_int 32;
  "32"]}
 *)
    
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf;

(*
  closed interval 
  {[
  zfold_left ~until:3 ~acc:0 (fun acc i -> acc + i);
  int = 6
  ]}
 *)
let zfold_left ?(start = 0) ~until ~acc f =
  let v = ref acc in
  (for x = start to until do v := f !v x done; !v);



(* we put the return value exn, so we don't need to work around type system later *)    
type cont 'a = 'a -> exn;

let callcc  (type u) (f: cont u-> u)  =
  let module M = struct exception Return of u ; end in
  try f (fun x -> raise (M.Return x))
  with [M.Return u -> u];
  

module List = struct
  include List;
  include BatList;
  let fold_lefti f acc ls =
    fold_left (fun (i,acc) x -> (i+1,f i acc x) ) (0,acc) ls;
end;

module Char = struct
  include BatChar;
end;

module String = struct
  include String;
  include BatString;
end;
  
module Ref = struct
  include BatRef;
end;
module Option = struct
  include BatOption;
end;

module Buffer = struct
  include BatBuffer ;
  let (+>) buf chr = begin Buffer.add_char buf chr; buf end;
  let (+>>) buf str = begin Buffer.add_string buf str; buf end;  
end;

module Hashtbl = struct
  include BatHashtbl;
  let keys tbl = fold (fun k _ acc -> [k::acc]) tbl [];
  let values tbl = fold (fun _ v acc -> [v::acc] ) tbl [];
end;


module type STREAM = sig
  type  t 'a;
  exception Failure;
  exception Error of string;
  val from: (int -> option 'a) -> t 'a;
  val of_list: list 'a-> t 'a;
  val of_string: string ->  t char;
  val of_channel: in_channel -> t char;
  val iter: ('a -> unit) -> t 'a -> unit;
  val next: t 'a -> 'a;
  val empty: t 'a -> unit;
  val peek: t 'a -> option 'a;
  val junk: t 'a -> unit;
  val count: t 'a -> int;
  val npeek: int -> t 'a ->  list 'a;
  val iapp: t 'a -> t 'a -> t 'a;
  val icons: 'a -> t 'a -> t 'a;
  val ising: 'a -> t 'a;
  val lapp: (unit -> t 'a) -> t 'a -> t 'a;
  val lcons: (unit -> 'a) -> t 'a -> t 'a;
  val lsing: (unit -> 'a) -> t 'a;
  val sempty: t 'a;
  val slazy: (unit -> t 'a) -> t 'a;
  val dump: ('a -> unit) -> t 'a -> unit;
    
  val to_list: t 'a ->  list 'a;
  val to_string: t char -> string;
  val to_string_fmt:
     format ('a -> string) unit string -> t 'a -> string;
  val to_string_fun: ('a -> string) -> t 'a -> string;
  val of_fun: (unit -> 'a) -> t 'a;
  val foldl: ('a -> 'b -> ('a * option bool )) -> 'a -> t 'b -> 'a;
  val foldr: ('a ->  lazy_t 'b -> 'b) -> 'b -> t 'a -> 'b;
  val fold: ('a -> 'a -> ('a *  option bool)) -> t 'a -> 'a;
  val filter: ('a -> bool) -> t 'a -> t 'a;
  val map2: ('a -> 'b -> 'c) -> t 'a -> t 'b -> t 'c;
  val scanl: ('a -> 'b -> 'a) -> 'a -> t 'b -> t 'a;
  val scan: ('a -> 'a -> 'a) -> t 'a -> t 'a;
  val concat: t (t 'a)  -> t 'a;
  val take: int -> t 'a -> t 'a;
  val drop: int -> t 'a -> t 'a;
  val take_while: ('a -> bool) -> t 'a -> t 'a;
  val drop_while: ('a -> bool) -> t 'a -> t 'a;
  val comb: (t 'a * t 'b) ->  t ('a * 'b);
  val split:  t ('a * 'b) -> (t 'a * t 'b);
  val merge:
    (bool -> 'a -> bool) -> (t 'a * t 'a) -> t 'a;
  val switch: ('a -> bool) -> t 'a -> (t 'a * t 'a);
  val cons: 'a -> t 'a -> t 'a;
  val apnd: t 'a -> t 'a -> t 'a;
  val is_empty: t 'a -> bool;
  val rev: t 'a -> t 'a;
  val tail: t 'a -> t 'a;
  val map: ('a -> 'b) -> t 'a -> t 'b;
  val dup: t 'a -> t 'a;
  val peek_nth: Stream.t 'a -> int ->   option 'a;
  val njunk: int -> Stream.t 'a -> unit;
end;
  
module Stream (* : STREAM with type t 'a = Stream.t 'a *) = struct
  include BatStream;
  include Stream;
  let rev strm=
    let rec aux = parser
    [ [< x ; 'xs>] -> [< 'aux xs ; x >]
    | [< >] ->  [< >] ] in
    aux strm;
  let tail = parser
    [ [< _; 'xs >] -> [< 'xs >]
    | [< >] -> [<>]];
  let rec map f = parser
    [ [< x; 'xs >] -> [< f x; 'map f xs >]
    | [< >] -> [< >] ];

  (* the minimual [n] is 0 *)
  let peek_nth strm n   =
    let rec loop i = fun
      [ [x :: xs] -> if i = 0 then Some x else loop (i - 1) xs
      | [] -> None ] in
    if n < 0 then
      invalid_arg "Stream.peek_nth"
    else loop n (Stream.npeek (n+1) strm);

  (*  Used by [try_parser], very in-efficient 
      This version of peek_nth is off-by-one from Stream.peek_nth *)      
  let dup strm = 
    Stream.from (peek_nth strm);
  
  let njunk  n strm  =
    for _i = 1 to n do Stream.junk strm done; (* FIXME unsed  index i*)
  let rec filter f = parser
    [ [< x; 'xs >] -> if f x then [< x ; 'filter f xs >] else [< 'filter f xs >]
    | [< >] -> [<>] ]; 
   (* let rec filter f = parser *)
   (*  [ [< x; 'xs>] -> [<>]]    *)
  (* value rec map f = parser *)
  (*   [ [: `x; xs :] -> [: `f x ; map f xs :] *)
  (*   | [: :] -> [: :] ];   *)
end;
(* ugly module, removed it later *)  
module ErrorMonad = struct     
  type log = string;

  type result 'a= [ Left of 'a | Right of log];

  let return x = Left x;
      
  let fail x = Right x;

  let ( >>= ) ma f = match ma with
  [ Left v -> f v
  | Right x -> Right x ];

  let bind = (>>=);
      
  let map f = fun
    [ Left v -> Left (f v)
    | Right s -> Right s];
          
  let ( >>| ) ma (str, f) =  match ma with
    [Left v -> f v
    | Right x -> Right (x ^ str)];

        (*  append error message later *)
  let ( >>? ) ma str =  match ma with
    [ Left _ -> ma
    | Right x -> Right (x ^ str)];
        
  let ( <|> ) fa fb a =  match fa a with
    [ (Left _ as x) -> x
    | Right str -> (fb a) >>? str];

        (* raise an exception to make type system simple  *)      
  let unwrap f a =  match f a with
    [ Left res -> res | Right msg -> failwith msg];

  let mapi_m f xs =
    let rec aux acc xs =
      match xs with
      [ [] -> return  []
      | [x :: xs] ->
          (f x acc) >>=
          (fun x -> (aux (acc + 1) xs) >>= (fun xs -> return [x :: xs]))]
    in aux 0 xs;
end;
  
