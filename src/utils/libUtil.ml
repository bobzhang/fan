
open Format

let id x = x

let cons x xs = x::xs
    
let failwithf fmt = Format.ksprintf failwith fmt
    
let prerr_endlinef fmt = Format.ksprintf prerr_endline fmt
    
let invalid_argf fmt = kprintf invalid_arg fmt
    
(* let undefined = failwith "undefined"; *)
let some x = Some x
    
let none = None
    
let memoize f =
  let cache = Hashtbl.create 101 in
  fun v ->
    try Hashtbl.find cache v
    with Not_found -> 
      let r = f v in
      (Hashtbl.replace cache v r; r)
  
let finally ~action x f   =
  try 
    let res = f x in
    (action (); res )
  with e -> begin action (); raise e end
    
let with_dispose ~dispose x f  =
  finally ~action:(fun () -> dispose x) x f 

(** {6 Operators}*)
external id : 'a -> 'a = "%identity"
external (!&) : _ -> unit = "%ignore"



    

let ( <| ) f x = f x

let ( |- ) f g x = g (f x)

let ( -| ) f g x = f (g x)

let flip f x y = f y x

let ( *** ) f g = fun (x,y) -> (f x, g y)

let ( &&& ) f g = fun x -> (f x, g x)

let curry f x y = f (x,y)

let uncurry f (x,y) = f x y

let const x _ = x

let tap f x = begin f x; x end 


let is_even x = x mod 2 == 0


let pp = fprintf
(*
  {[
  to_string_of_printer pp_print_int 32;
  "32"]}
 *)
    
let to_string_of_printer printer v =
  let buf = Buffer.create 30 in
  let () = Format.bprintf buf "@[%a@]" printer v in Buffer.contents buf

(*
  closed interval 
  {[
  zfold_left ~until:3 ~acc:0 (fun acc i -> acc + i);
  int = 6
  ]}
 *)
let zfold_left ?(start = 0) ~until ~acc f =
  let v = ref acc in
  (for x = start to until do v := f !v x done; !v)



(* we put the return value exn, so we don't need to work around type system later *)    
type 'a cont  = 'a -> exn

let callcc  (type u) (f: u cont  -> u)  =
  let module M = struct exception Return of u end in
  try f (fun x -> raise (M.Return x))
  with | M.Return u -> u
  


type  'a return  = { return :  'b. 'a -> 'b }

let with_return f =
  let module M = struct
    (* Raised to indicate ~return was called.  Local so that the exception is tied to a
       particular call of [with_return]. *)
    exception Return
  end in
  let r = ref None in                   (* stores the return value *)
  let return = {                        (* closure passed to f *)
    return = (fun x ->
      (r := Some x; raise M.Return));
  } in
  try
    let rval = f return in
    begin match !r with
    | None -> rval
    | Some _ -> failwith "with_return exited normally despite return being called"
    end
  with M.Return ->                      (* allows other exceptions through *)
    match !r with
    | None -> assert false
    | Some x -> x



(* type 'a id  = 'a -> 'a *)



module Filename = struct
  include Filename

  let find_in_path ~path name =
    if not (Filename.is_implicit name) then
      if Sys.file_exists name then name else raise Not_found
    else begin
      let rec try_dir = function
        | [] -> raise Not_found
        | dir::rem ->
            let fullname = Filename.concat dir name in
            if Sys.file_exists fullname then fullname else try_dir rem
      in try_dir path
    end

  let find_in_path_uncap ~path name =
    let uname = String.uncapitalize name in
    let rec try_dir =
      function
      | [] -> raise Not_found
      | dir::rem ->
          let fullname = Filename.concat dir name
          and ufullname = Filename.concat dir uname in
          if Sys.file_exists ufullname then ufullname
          else if Sys.file_exists fullname then fullname
          else try_dir rem
    in try_dir path

  let expand_directory ~std s =
    if String.length s > 0 && s.[0] = '+'
    then Filename.concat std
        (String.sub s 1 (String.length s - 1))
    else s

end
module Queue = struct
  include Queue
      
  let find t ~f =
    with_return (fun r -> (iter(fun x -> if f x then r.return (Some x)) t; None))
      
  let find_map t ~f =
    with_return (fun r ->
      (iter (fun x -> match f x with | None -> () | Some _ as res -> r.return res) t;
       None))
      (* the first element is in the bottom *)  
  let to_list_rev q =
    fold (fun acc v -> v::acc) [] q 

  let of_list l =
    let q = create () in
    let _ = List.iter (fun x -> push x q) l in 
    q
      
  let rev q=
    of_list (to_list_rev q )
      
end



    

module type MAP = sig
  include Map.S
  val of_list: (key * 'a) list -> 'a t
  val of_hashtbl:(key,'a) Hashtbl.t  -> 'a t
  val elements: 'a t -> (key * 'a) list 
  val add_list: (key * 'a) list  -> 'a t -> 'a t
  val find_default: default :'a -> key -> 'a t -> 'a 
  val find_opt: key -> 'a t -> 'a option
    (* FIXME  [~default:] [~default :] *)
  val add_with: f :('a -> 'a -> 'a) -> key ->
    'a ->  'a t ->
      ('a t * [ `NotExist | `Exist])

  val unsafe_height: 'a t -> int
  val unsafe_node:  'a t -> (key * 'a) ->  'a t ->  'a t
end

      
module MapMake(S:Map.OrderedType) : MAP with type key = S.t = struct
  include Map.Make (S) (* TODO: the same syntax with original *)
  let of_list lst =
    List.fold_left (fun acc (k,v)  -> add k v acc) empty lst
  let add_list lst base =
    List.fold_left (fun acc (k,v) -> add k v acc) base lst
  let of_hashtbl tbl =
    Hashtbl.fold (fun k v acc -> add k v acc) tbl empty
  let elements map =
    fold (fun k v acc ->  (k,v) :: acc ) map [] 
  let find_default ~default k m =
    try find k m with Not_found -> default

  (* can be more efficient if we break the abstraction *)    
  let add_with ~f k v s =
     try (add k (f  (find k s) v) s, `Exist) with Not_found -> (add k v s,`NotExist)

  let unsafe_height (l:'a t) : int =
    if l = empty then 0
    else (Obj.magic (Obj.field (Obj.repr l)  4) :int)  

  (* this is unsafe, since the difference between the height of [l] and
     the height of [r] may exceed 1
   *)  
  let unsafe_node (l:'a t) ((k:key),(v:'a)) (r:'a t) =
    let h = max (unsafe_height l) (unsafe_height  r) + 1 in
    let o = Obj.new_block 0 4 in begin 
      Obj.set_field o 0 (Obj.repr l);
      Obj.set_field o 1 (Obj.repr k);
      Obj.set_field o 2 (Obj.repr v);
      Obj.set_field o 3 (Obj.repr r);
      Obj.set_field o 4 (Obj.repr h);
      (Obj.magic o : 'a t)
    end
    
  let find_opt k m =
    try Some (find k m) with Not_found -> None
end 



module type SET = sig
  include Set.S
  val of_list: elt list  -> t 
  val add_list: t ->  elt list -> t 
  val of_array: elt array  -> t
  val add_array: t ->  elt array -> t 
end

module SetMake(S:Set.OrderedType) : SET with type elt = S.t = struct
  include Set.Make (S)
  let of_list = List.fold_left (flip add) empty
  let add_list c = List.fold_left (flip add) c
  let of_array = Array.fold_left (flip add) empty
  let add_array c = Array.fold_left (flip add) c
end

module SSet =SetMake (String)

module SMap = MapMake (String)

  
module IMap = MapMake (struct
  type t = int
  let compare = Pervasives.compare  
end)

module ISet = SetMake(struct
 type t = int
 let compare = Pervasives.compare
end)


module Hashset = struct
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
    let set = create size in begin
      List.iter (add set) vs;
      set
    end
  let add_list set vs =
    List.iter (add set) vs
  let to_list set = fold (fun x y -> x::y) set []
  (* let empty = Hashtbl.create 30 ; *)
end 

let mk_set (type s) ~cmp =
  let module M = struct type t = s let compare = cmp end in
  (module Set.Make (M) :Set.S with type elt = s)

let mk_map (type s) ~cmp=
  let module M = struct type t = s let compare = cmp end in
  (module Map.Make (M) : Map.S with type key = s)
  
let mk_hashtbl (type s) ~eq ~hash =
  let module M =
    struct type t = s let equal = eq let hash = hash end in
  (module Hashtbl.Make (M)  :Hashtbl.S with type key = s)
  


module LStack = struct
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

end

    
  
module Ref = struct
  (* treat [r]'s state as [v] in [body] *)
  let protect r v body =
    let old = !r in
    try begin 
      r := v;
      let res = body() in
      (r := old;
      res)
    end with x -> (r := old; raise x)
        
  let safe r body =
    let old = !r in
    finally ~action:(fun () -> r:=old) () body 
    
  let protect2 (r1,v1) (r2,v2) body =
    let o1 = !r1 and o2 = !r2 in
    try begin
      r1:= v1; r2:=v2;
      let res = body () in
      (r1:=o1; r2:=o2;
      res)
    end
    with  e -> begin
      r1:=o1; r2:=o2;
      raise e
    end

        
  let save2 r1 r2 body =
      let o1 = !r1 and o2 = !r2 in
      finally ~action:(fun () -> (r1:=o1; r2:=o2)) () body 
      
  let protects refs vs body =
    let olds = List.map (fun x-> !x ) refs in 
    try begin
      List.iter2 (fun ref v -> ref:=v) refs vs;
      let res = body () in
      (List.iter2 (fun ref v -> ref:=v) refs olds;
      res)   
    end
      with e -> 
        (List.iter2 (fun ref v -> ref:=v) refs olds;
        raise e)

  (* The state [itself] should be [persistent],
     otherwise it does not make sense to restore
   *)      
  let saves (refs: 'a ref list ) body =
    let olds = List.map (fun x -> !x) refs in
    finally ~action:(fun () ->   List.iter2 (fun ref x -> ref :=x ) refs olds) () body 


  let post r f =
    let old = !r in 
    (r := f old; old)

   let pre r f =
     (r := f !r; !r)

  let swap a b =
    let buf = !a in
    (a := !b; b := buf)
    
  let modify x f =
    x := f !x

end
    

    
module Option = struct

  let may f = function
    | None -> ()
    | Some v -> f v
  (*$T may
    let x = ref 3 in may incr (Some x); !x = 4 *)


   let map f = function
     | None -> None
     | Some v -> Some (f v)
  (*$T map
    map succ None = None
    map succ (Some 3) = (Some 4)
   *)


   let bind f = function
     | None -> None
     | Some v -> f v
  (*$T bind
    bind (fun s -> Some s) None = None
    bind (fun s -> Some s) (Some ()) = Some ()
   *)


    let apply = function
      | None -> (fun x -> x)
      | Some f -> f
   (*$T apply
     apply None 3 = 3
     apply (Some succ) 3 = 4
    *)


    let filter f = function
      | Some x when f x -> Some x
      | _ -> None
    (*$T filter
      filter (fun _ -> true) None = None
      filter (fun _ -> true) (Some 3) = Some 3
      filter (fun _ -> false) (Some 3) = None
     *)


     let default v = function
       |None -> v
       | Some v -> v
     (*$T default
       default 3 None = 3
       default 3 (Some 4) = 4
      *)

      let is_some = function
	|None -> false
	| _ -> true
      (*$T is_some
        not (is_some None)
        is_some (Some ())
       *)

      let is_none = function
	| None -> true
	| _ -> false
       (*$T is_none
         is_none None
         not (is_none (Some ()))
        *)

       let get_exn s e =
         match s with
         | None   -> raise e
	 | Some v -> v
       (*$T get_exn
         try get_exn None Exit with Exit -> true
         try get_exn (Some true) Exit with Exit -> false
        *)

       let get s = get_exn s (Invalid_argument "Option.get")
       (*$T get
         try get None with Invalid_argument _ -> true
         try get (Some true) with Invalid_argument _ -> false
        *)

       let map_default f v = function
	 | None -> v
	 | Some v2 -> f v2
        (*$T map_default
          map_default succ 2 None = 2
          map_default succ 2 (Some 3) = 4
         *)

        let compare ?(cmp=Pervasives.compare) a b =
          match a with
          |None ->
              (match b with
              |None -> 0
              | Some _ -> -1)
          | Some x ->
              (match b with
              |None -> 1
              | Some y -> cmp x y)
       (*$T compare
         compare (Some 0) (Some 1) < 0
         compare (Some 0) (Some 0) = 0
         compare (Some 0) (Some (-1)) > 0
         compare None (Some ()) < 0
         compare None None = 0
         compare (Some ()) None > 0
         compare ~cmp:(fun _ _ -> 0) (Some (fun x -> x)) (Some (fun y -> y)) = 0
        *)


         let eq ?(eq=(=)) x y =
           match (x,y) with
           | (None, None) -> true
           | (Some a, Some b) -> eq a b
           | _ -> false

end

module Buffer = struct
  include Buffer
  let (+>) buf chr = begin Buffer.add_char buf chr; buf end
  let (+>>) buf str = begin Buffer.add_string buf str; buf end  
end

module Hashtbl = struct
  include Hashtbl
  let keys tbl = fold (fun k _ acc -> k::acc) tbl []
  let values tbl = fold (fun _ v acc -> v::acc ) tbl []
  let find_default ~default tbl k =
    try find tbl k with Not_found -> default 
  let find_opt tbl k =
    try Some (find tbl k) with Not_found -> None
end

module Array = struct
  include Array
  let fold_left2 f acc  a1 a2 =
    let l1 = Array.length a1
    and l2 = Array.length a2 in
    if l1 <> l2 then invalid_arg "Array.fold_left2 length is not equal"
    else
      let acc = ref acc in
      let rec loop i =
        if i < l1 then begin 
          acc := f !acc a1.(i) a2.(i);
          loop (i+1);
        end 
        else
          !acc in
      loop 0 
   (* let of_stream s = *)
     
   let stream a =
     Fstream.of_array a
       
   (* let filter_map f arr = *)
  let filter_opt t = begin 
    let n = length t in
    let res_size = ref 0 in
    let first_some = ref None in
    (for i = 0 to n - 1 do
      match t.(i) with
      | None -> ()
      | Some _ as s -> begin 
          if !res_size = 0 then first_some := s else () ;
          incr res_size
      end
    done;
     match !first_some with
     | None -> [||]
     | Some el ->
         let result = create (!res_size) el in
         let pos = ref 0 in
         let _ =
           for i = 0 to n - 1 do
             match t.(i) with
             | None -> ()
             | Some x -> begin 
                 result.(!pos) <- x;
                 incr pos
             end
           done in 
         result)
  end
  let filter_map f a = filter_opt  (map f a)
  let filter_mapi f a = filter_opt (mapi f a)
  let for_all2 p xs ys =
    let n = length xs in
    let _ = if length ys <> n then raise (Invalid_argument "Array.for_all2") in
    let rec loop i =
      if i = n then true
      else if p xs.(i) ys.(i) then loop (succ i)
      else false  in
  loop 0
    
end


module type STREAM = sig
  type  'a t 
  exception Failure
  exception Error of string
  val from: (int -> 'a option ) -> 'a t
  val of_list: 'a list-> 'a t
  val of_string: string ->  char t
  val of_channel: in_channel -> char t 
  val iter: ('a -> unit) -> 'a t -> unit
  val next: 'a t -> 'a
  val empty: 'a t -> unit
  val peek: 'a t -> 'a option 
  val junk: 'a t -> unit
  val count: 'a t -> int
  val npeek: int -> 'a t ->  'a list 
  val iapp: 'a t -> 'a t -> 'a t
  val icons: 'a -> 'a t -> 'a t
  val ising: 'a -> 'a t
  val lapp: (unit -> 'a t) -> 'a t -> 'a t
  val lcons: (unit -> 'a) -> 'a t -> 'a t
  val lsing: (unit -> 'a) -> 'a t
  val sempty: 'a t
  val slazy: (unit -> 'a t) -> 'a t
  val dump: ('a -> unit) -> 'a t -> unit
    
  val to_list: 'a t ->  'a list 
  val to_string: char t -> string
  val to_string_fmt:
     (('a -> string),unit,string) format   -> 'a t -> string
  val to_string_fun: ('a -> string) -> 'a t -> string
  val of_fun: (unit -> 'a) -> 'a t
  val foldl: ('a -> 'b -> ('a * bool option )) -> 'a -> 'b t -> 'a
  val foldr: ('a ->  'b lazy_t  -> 'b) -> 'b -> 'a t -> 'b
  val fold: ('a -> 'a -> ('a *   bool option)) -> 'a t -> 'a
  val filter: ('a -> bool) -> 'a t -> 'a t
  val map2: ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t 
  val scanl: ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a t
  val scan: ('a -> 'a -> 'a) -> 'a t -> 'a t
  val concat: 'a t t   -> 'a t
  val take: int -> 'a t -> 'a t
  val drop: int -> 'a t -> 'a t
  val take_while: ('a -> bool) -> 'a t -> 'a t
  val drop_while: ('a -> bool) -> 'a t -> 'a t
  val comb: ('a t * 'b t) ->  ('a * 'b) t 
  val split:  ('a * 'b) t  -> ('a t * 'b t)
  val merge:
    (bool -> 'a -> bool) -> ('a t * 'a t) -> 'a t
  val switch: ('a -> bool) -> 'a t -> ('a t * 'a t)
  val cons: 'a -> 'a t -> 'a t
  val apnd: 'a t -> 'a t -> 'a t
  val is_empty: 'a t -> bool
  val rev: 'a t -> 'a t
  val tail: 'a t -> 'a t
  val map: ('a -> 'b) -> 'a t -> 'b t
  val dup: 'a t -> 'a t
  val peek_nth: 'a t -> int ->   'a option 
  val njunk: int -> 'a t  -> unit
end

  

    
(* ugly module, removed it later *)  
module ErrorMonad = struct     
  type log = string

  type 'a result = | Left of 'a | Right of log

  let return x = Left x
      
  let fail x = Right x

  let ( >>= ) ma f =
    match ma with
    | Left v -> f v
    | Right x -> Right x

  let bind = (>>=)
      
  let map f = function
    | Left v -> Left (f v)
    | Right s -> Right s
          
  let ( >>| ) ma (str, f) =
    match ma with
    | Left v -> f v
    | Right x -> Right (x ^ str)

        (*  append error message later *)
  let ( >>? ) ma str =
    match ma with
    | Left _ -> ma
    | Right x -> Right (x ^ str)
        
  let ( <|> ) fa fb a =
    match fa a with
    | (Left _ as x) -> x
    | Right str -> (fb a) >>? str

        (* raise an exception to make type system simple  *)      
  let unwrap f a =
    match f a with
    | Left res -> res
    | Right msg -> failwith msg

  let mapi_m f xs =
    let rec aux acc xs =
      match xs with
      | [] -> return  []
      | x :: xs ->
          (f x acc) >>=
          (fun x -> (aux (acc + 1) xs) >>= (fun xs -> return (x :: xs)))
    in aux 0 xs
end

type space_formatter =  (unit, Format.formatter, unit )format     
(** duplicated *)      
let pp_list ?sep ?first  ?last fu f xs = 

      let first = Option.default ("":space_formatter) first in
      let last = Option.default ("":space_formatter) last in
      let sep = Option.default ("@ ":space_formatter) sep in
      let aux f = function
        | [] -> ()
        | [x] -> fu f x
        | xs ->
            let rec loop  f = function
              | [x] -> fu f x
              | x::xs ->  pp f "%a%(%)%a" fu x sep loop xs 
              | _ -> assert false  in 
            pp f "%(%)%a%(%)" first loop xs last in
      aux f xs
let pp_option :
    ?first:space_formatter -> ?last:space_formatter ->
    (Format.formatter -> 'a -> unit) -> Format.formatter ->  'a option -> unit
        = fun  ?first  ?last fu f a ->
     let first =
       match first with
       | Some x -> x
       | None -> ""
     and last =
       match last with
       | Some x -> x
       | None -> ""  in
     match a with
     | None -> ()
     | Some x -> pp f "%(%)%a%(%)" first fu x last
  


(** Format enhancement *)
module Format = struct
  include Format
  let pp_print_list mf_a  fmt  lst =
    let open List in 
    fprintf fmt "@[<1>[%a]@]"
      (fun fmt  -> iter (fun x ->
        fprintf fmt "%a@ " mf_a x )) lst

  let pp_print_option mf_a fmt v =
    match v with
    | None -> fprintf fmt "None"
    | Some v -> fprintf fmt "Some @[%a@]" mf_a v 
  let pp_print_int32: Format.formatter -> int32 -> unit =
    fun fmt  a  -> Format.fprintf fmt "%ld" a
  let pp_print_int64: Format.formatter -> int64 -> unit =
    fun fmt  a  -> Format.fprintf fmt "%Ld" a
  let pp_print_nativeint: Format.formatter -> nativeint -> unit =
    fun fmt  a  -> Format.fprintf fmt "%nd" a
  let pp_print_float = pp_print_float
  let pp_print_string: Format.formatter -> string -> unit =
    fun fmt  a  -> Format.fprintf fmt "%S" a
  let pp_print_bool = pp_print_bool
  let pp_print_char = pp_print_char
  let pp_print_unit: Format.formatter -> unit -> unit =
    fun fmt  _  -> Format.fprintf fmt "()"
end

(* local variables: *)
(* compile-command: "pmake libUtil.cmo" *)
(* end: *)
