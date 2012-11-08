open Format
let failwithf fmt = ksprintf failwith fmt
module MapMake(S:Map.OrderedType) = struct
  include Map.Make(S)
  let of_list lst = List.fold_left (fun acc  (k,v)  -> add k v acc) empty lst
  let of_hashtbl tbl = Hashtbl.fold (fun k  v  acc  -> add k v acc) tbl empty
  let elements map = fold (fun k  v  acc  -> (k, v) :: acc) map []
  end
module SSet = Set.Make(String)
module SMap = MapMake(String)
module IMap = Set.Make(struct
  type t = int  let compare = Pervasives.compare
  end)
module ISet = Set.Make(struct
  type t = int  let compare = Pervasives.compare
  end)
module Hashset = struct
  type 'a t = ('a,unit) Hashtbl.t  let create = Hashtbl.create
  let add set x = Hashtbl.replace set x () let remove = Hashtbl.remove
  let mem = Hashtbl.mem let iter f = Hashtbl.iter (fun v  ()  -> f v)
  let fold f = Hashtbl.fold (fun v  ()  st  -> f v st)
  let elements = Hashtbl.length let clear = Hashtbl.clear
  let of_list size vs = let set = create size in List.iter (add set) vs; set
  let to_list set = fold (fun x  y  -> x :: y) set []
  end
let mk_set (type s) ~cmp  =
  let module M = struct
    type t = s  let compare = cmp
    end in ((module Set.Make(M)) : (module Set.S with type elt = s) )
let mk_map (type s) ~cmp  =
  let module M = struct
    type t = s  let compare = cmp
    end in ((module Map.Make(M)) : (module Map.S with type key = s) )
let mk_hashtbl (type s) ~eq  ~hash  =
  let module M = struct
    type t = s  let equal = eq let hash = hash
    end in ((module Hashtbl.Make(M)) : (module Hashtbl.S with type key = s) )
let (|>) x f = f x
let (<|) f x = f x
let (|-) f g x = g (f x)
let (-|) f g x = f (g x)
let flip f x y = f y x
let ( *** )  f g (x,y) = ((f x), (g y))
let (&&&) f g x = ((f x), (g x))
let curry f x y = f (x, y)
let uncurry f (x,y) = f x y
let const x _ = x
let tap f x = f x; x
let is_even x = (x mod 2) == 0
let pp = fprintf
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf
let zfold_left ?(start= 0)  ~until  ~acc  f =
  let v = ref acc in
  for x = start to until do v := (f v.contents x) done; v.contents
type 'a cont = 'a -> exn 
let callcc (type u) (f : u cont -> u) =
  let module M = struct
    exception Return of u
    end in try f (fun x  -> raise (M.Return x)) with | M.Return u -> u
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
  include BatBuffer let (+>) buf chr = Buffer.add_char buf chr; buf
  let (+>>) buf str = Buffer.add_string buf str; buf
  end
module Hashtbl = struct
  include BatHashtbl let keys tbl = fold (fun k  _  acc  -> k :: acc) tbl []
  let values tbl = fold (fun _  v  acc  -> v :: acc) tbl []
  end
module type STREAM =
  sig
    type 'a t  
    exception Failure
    exception Error of string
    val from : (int -> 'a option) -> 'a t
    val of_list : 'a list -> 'a t
    val of_string : string -> char t
    val of_channel : in_channel -> char t
    val iter : ('a -> unit) -> 'a t -> unit
    val next : 'a t -> 'a
    val empty : 'a t -> unit
    val peek : 'a t -> 'a option
    val junk : 'a t -> unit
    val count : 'a t -> int
    val npeek : int -> 'a t -> 'a list
    val iapp : 'a t -> 'a t -> 'a t
    val icons : 'a -> 'a t -> 'a t
    val ising : 'a -> 'a t
    val lapp : (unit -> 'a t) -> 'a t -> 'a t
    val lcons : (unit -> 'a) -> 'a t -> 'a t
    val lsing : (unit -> 'a) -> 'a t
    val sempty : 'a t
    val slazy : (unit -> 'a t) -> 'a t
    val dump : ('a -> unit) -> 'a t -> unit
    val to_list : 'a t -> 'a list
    val to_string : char t -> string
    val to_string_fmt : ('a -> string,unit,string) format -> 'a t -> string
    val to_string_fun : ('a -> string) -> 'a t -> string
    val of_fun : (unit -> 'a) -> 'a t
    val foldl : ('a -> 'b -> ('a* bool option)) -> 'a -> 'b t -> 'a
    val foldr : ('a -> 'b lazy_t -> 'b) -> 'b -> 'a t -> 'b
    val fold : ('a -> 'a -> ('a* bool option)) -> 'a t -> 'a
    val filter : ('a -> bool) -> 'a t -> 'a t
    val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t
    val scanl : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
    val scan : ('a -> 'a -> 'a) -> 'a t -> 'a t
    val concat : 'a t t -> 'a t
    val take : int -> 'a t -> 'a t
    val drop : int -> 'a t -> 'a t
    val take_while : ('a -> bool) -> 'a t -> 'a t
    val drop_while : ('a -> bool) -> 'a t -> 'a t
    val comb : ('a t* 'b t) -> ('a* 'b) t
    val split : ('a* 'b) t -> ('a t* 'b t)
    val merge : (bool -> 'a -> bool) -> ('a t* 'a t) -> 'a t
    val switch : ('a -> bool) -> 'a t -> ('a t* 'a t)
    val cons : 'a -> 'a t -> 'a t
    val apnd : 'a t -> 'a t -> 'a t
    val is_empty : 'a t -> bool
    val rev : 'a t -> 'a t
    val tail : 'a t -> 'a t
    val map : ('a -> 'b) -> 'a t -> 'b t
    val dup : 'a t -> 'a t
    val peek_nth : 'a Stream.t -> int -> 'a option
    val njunk : int -> 'a Stream.t -> unit
  end
module Stream =
  (struct
    include BatStream include Stream
    let rev strm =
      let rec aux (__strm : _ Stream.t) =
        match Stream.peek __strm with
        | Some x ->
            (Stream.junk __strm;
             (let xs = __strm in
              Stream.lapp (fun _  -> aux xs) (Stream.ising x)))
        | _ -> Stream.sempty in
      aux strm
    let tail (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some _ -> (Stream.junk __strm; __strm)
      | _ -> Stream.sempty
    let rec map f (__strm : _ Stream.t) =
      match Stream.peek __strm with
      | Some x ->
          (Stream.junk __strm;
           (let xs = __strm in
            Stream.lcons (fun _  -> f x) (Stream.slazy (fun _  -> map f xs))))
      | _ -> Stream.sempty
    let peek_nth strm n =
      let rec loop i =
        function
        | x::xs -> if i = 0 then Some x else loop (i - 1) xs
        | [] -> None in
      if n < 0
      then invalid_arg "Stream.peek_nth"
      else loop n (Stream.npeek (n + 1) strm)
    let dup strm = Stream.from (peek_nth strm)
    let njunk n strm = for _i = 1 to n do Stream.junk strm done
    end : (STREAM with type 'a t = 'a Stream.t ))
module ErrorMonad = struct
  type log = string  type 'a result =  
                       | Left of 'a
                       | Right of log  let return x = Left x
  let fail x = Right x
  let (>>=) ma f = match ma with | Left v -> f v | Right x -> Right x
  let bind = ( >>= )
  let map f = function | Left v -> Left (f v) | Right s -> Right s
  let (>>|) ma (str,f) =
    match ma with | Left v -> f v | Right x -> Right (x ^ str)
  let (>>?) ma str =
    match ma with | Left _ -> ma | Right x -> Right (x ^ str)
  let (<|>) fa fb a =
    match fa a with | Left _ as x -> x | Right str -> (fb a) >>? str
  let unwrap f a =
    match f a with | Left res -> res | Right msg -> failwith msg
  let mapi_m f xs =
    let rec aux acc xs =
      match xs with
      | [] -> return []
      | x::xs ->
          (f x acc) >>=
            (fun x  -> (aux (acc + 1) xs) >>= (fun xs  -> return (x :: xs))) in
    aux 0 xs
  end