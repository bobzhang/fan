



let cons x xs = x::xs
    
let failwithf fmt = Format.ksprintf failwith fmt
    
let prerr_endlinef fmt = Format.ksprintf prerr_endline fmt
    
let invalid_argf fmt = Format.kprintf invalid_arg fmt
    
let undef () = failwith "undefined"
    
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


let pp = Format.fprintf
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
