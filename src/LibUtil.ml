
open Format;
let failwithf fmt = ksprintf failwith fmt  ;

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
  let of_list size vs = 
    let set = create size in begin
      List.iter (add set) vs;
      set
    end;
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
  include BatList;
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
  include BatBuffer;
  let (+>) buf chr = begin Buffer.add_char buf chr; buf end;
  let (+>>) buf str = begin Buffer.add_string buf str; buf end;  
end;

module Hashtbl = struct
  include BatHashtbl;
  let keys tbl = fold (fun k _ acc -> [k::acc]) tbl [];
  let values tbl = fold (fun _ v acc -> [v::acc] ) tbl [];
end;

module Stream = struct
  include BatStream;
  include Stream;
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
  
