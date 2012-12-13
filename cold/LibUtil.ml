open Format
let failwithf fmt = ksprintf failwith fmt
let prerr_endlinef fmt = ksprintf prerr_endline fmt
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
  let of_list ?(size= 100)  vs =
    let set = create size in List.iter (add set) vs; set
  let add_list set vs = List.iter (add set) vs
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
let (&) f x = f x
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
  include List
  let rec drop n = function | _::l when n > 0 -> drop (n - 1) l | l -> l
  let lastbut1 ls =
    match ls with
    | [] -> failwith "lastbut1 empty"
    | _ -> let l = List.rev ls in ((List.tl l), (List.hd l))
  let split_at n xs =
    let rec aux n acc xs =
      match xs with
      | [] ->
          if n = 0 then (acc, []) else invalid_arg "Index past end of list"
      | h::t as l -> if n = 0 then (acc, l) else aux (n - 1) (h :: acc) t in
    if n < 0
    then invalid_arg "split_at n< 0"
    else (let (a,b) = aux n [] xs in ((rev a), b))
  let rec find_map f =
    function
    | [] -> raise Not_found
    | x::xs -> (match f x with | Some y -> y | None  -> find_map f xs)
  let fold_lefti f acc ls =
    fold_left (fun (i,acc)  x  -> ((i + 1), (f i acc x))) (0, acc) ls
  let rec remove x =
    function
    | (y,_)::l when y = x -> l
    | d::l -> d :: (remove x l)
    | [] -> []
  let iteri f lst =
    let i = ref 0 in
    List.iter (fun x  -> let () = f i.contents x in incr i) lst
  type dir = [ `Left | `Right] 
  let reduce_left f lst =
    match lst with
    | [] -> invalid_arg "reduce_left length zero"
    | x::xs ->
        let rec loop x xs =
          match xs with | [] -> x | y::ys -> loop (f x y) ys in
        loop x xs
  let reduce_right_with ~compose  ~f  lst =
    match lst with
    | [] -> invalid_arg "reduce_right length zero"
    | xs ->
        let rec loop xs =
          match xs with
          | [] -> assert false
          | y::[] -> f y
          | y::ys -> compose (f y) (loop ys) in
        loop xs
  let reduce_right compose = reduce_right_with ~compose ~f:(fun x  -> x)
  let init n f = let open Array in to_list & (init n f)
  end
module Char = struct
  include Char
  let is_whitespace =
    function | ' '|'\n'|'\r'|'\t'|'\026'|'\012' -> true | _ -> false
  let is_newline = function | '\n'|'\r' -> true | _ -> false
  let is_digit = function | '0'..'9' -> true | _ -> false
  let is_uppercase c = ('A' <= c) && (c <= 'Z')
  let is_lowercase c = ('a' <= c) && (c <= 'z')
  end
module Return = struct
  type 'a t = 'a -> exn  let return label v = raise (label v)
  let label (type u) (f : u t -> u) =
    (let module M = struct
       exception Return of u
       end in try f (fun x  -> M.Return x) with | M.Return u -> u : u )
  let with_label = label
  end
module String = struct
  include String
  let init len f =
    let s = create len in
    for i = 0 to len - 1 do unsafe_set s i (f i) done; s
  let starts_with str p =
    let len = length p in
    if (length str) < len
    then false
    else
      Return.label
        (fun label  ->
           for i = 0 to len - 1 do
             if (unsafe_get str i) <> (unsafe_get p i)
             then Return.return label false
             else ()
           done;
           true)
  let ends_with str p =
    let el = length p and sl = length str in
    let diff = sl - el in
    if diff < 0
    then false
    else
      Return.label
        (fun label  ->
           for i = 0 to el - 1 do
             if (get str (diff + i)) <> (get p i)
             then Return.return label false
             else ()
           done;
           true) let of_char = make 1
  let drop_while f s =
    let len = length s in
    let found = ref false in
    let i = ref 0 in
    while (i.contents < len) && (not found.contents) do
      if not (f (s.[i.contents])) then found := true else incr i done;
    String.sub s i.contents (len - i.contents)
  let neg n =
    let len = String.length n in
    if (len > 0) && ((n.[0]) = '-')
    then String.sub n 1 (len - 1)
    else "-" ^ n
  end
module Ref =
  struct
  let protect r v body =
    let old = r.contents in
    try r := v; (let res = body () in r := old; res)
    with | x -> (r := old; raise x)
  let protect2 (r1,v1) (r2,v2) body =
    let o1 = r1.contents and o2 = r2.contents in
    try r1 := v1; r2 := v2; (let res = body () in r1 := o1; r2 := o2; res)
    with | e -> (r1 := o1; r2 := o2; raise e)
  let protects refs vs body =
    let olds = List.map (fun x  -> x.contents) refs in
    try
      List.iter2 (fun ref  v  -> ref := v) refs vs;
      (let res = body () in
       List.iter2 (fun ref  v  -> ref := v) refs olds; res)
    with | e -> (List.iter2 (fun ref  v  -> ref := v) refs olds; raise e)
  end
module Option = struct
  let may f = function | None  -> () | Some v -> f v
  let map f = function | None  -> None | Some v -> Some (f v)
  let bind f = function | None  -> None | Some v -> f v
  let apply = function | None  -> (fun x  -> x) | Some f -> f
  let filter f = function | Some x when f x -> Some x | _ -> None
  let default v = function | None  -> v | Some v -> v
  let is_some = function | None  -> false | _ -> true
  let is_none = function | None  -> true | _ -> false
  let get_exn s e = match s with | None  -> raise e | Some v -> v
  let get s = get_exn s (Invalid_argument "Option.get")
  let map_default f v = function | None  -> v | Some v2 -> f v2
  let compare ?(cmp= Pervasives.compare)  a b =
    match a with
    | None  -> (match b with | None  -> 0 | Some _ -> (-1))
    | Some x -> (match b with | None  -> 1 | Some y -> cmp x y)
  let eq ?(eq= ( = ))  x y =
    match (x, y) with
    | (None ,None ) -> true
    | (Some a,Some b) -> eq a b
    | _ -> false
  end
module Buffer = struct
  include Buffer let (+>) buf chr = Buffer.add_char buf chr; buf
  let (+>>) buf str = Buffer.add_string buf str; buf
  end
module Hashtbl = struct
  include Hashtbl let keys tbl = fold (fun k  _  acc  -> k :: acc) tbl []
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
    val peek_nth : 'a t -> int -> 'a option
    val njunk : int -> 'a t -> unit
  end
module XStream = struct
  include XStream
  let rev strm =
    let rec aux (__strm : _ XStream.t) =
      match XStream.peek __strm with
      | Some x ->
          (XStream.junk __strm;
           (let xs = __strm in
            XStream.lapp (fun _  -> aux xs) (XStream.ising x)))
      | _ -> XStream.sempty in
    aux strm
  let tail (__strm : _ XStream.t) =
    match XStream.peek __strm with
    | Some _ -> (XStream.junk __strm; __strm)
    | _ -> XStream.sempty
  let rec map f (__strm : _ XStream.t) =
    match XStream.peek __strm with
    | Some x ->
        (XStream.junk __strm;
         (let xs = __strm in
          XStream.lcons (fun _  -> f x) (XStream.slazy (fun _  -> map f xs))))
    | _ -> XStream.sempty
  let peek_nth strm n =
    let rec loop i =
      function
      | x::xs -> if i = 0 then Some x else loop (i - 1) xs
      | [] -> None in
    if n < 0
    then invalid_arg "XStream.peek_nth"
    else loop n (XStream.npeek (n + 1) strm)
  let dup strm = XStream.from (peek_nth strm)
  let njunk n strm = for _i = 1 to n do XStream.junk strm done
  let rec filter f (__strm : _ XStream.t) =
    match XStream.peek __strm with
    | Some x ->
        (XStream.junk __strm;
         (let xs = __strm in
          if f x
          then XStream.icons x (XStream.slazy (fun _  -> filter f xs))
          else XStream.slazy (fun _  -> filter f xs)))
    | _ -> XStream.sempty
  end
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
            ((fun x  -> (aux (acc + 1) xs) >>= (fun xs  -> return (x :: xs)))) in
    aux 0 xs
  end