



let cons x xs = x::xs
    
let failwithf fmt = Format.ksprintf failwith fmt
    
let prerr_endlinef fmt = Format.ksprintf prerr_endline fmt
    
let invalid_argf fmt = Format.kprintf invalid_arg fmt
    
let undef () = failwith "undefined"
    
let some x = Some x
    
let none = None
    

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

let tap x f  = begin ignore (f x); x end 


(* To be deleted once the OCaml team fixes Mantis issue #4751.
   This function is copied from the compiler, function hash_variant
   in typing/btype.ml. *)
let hash_variant s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let () = assert (Obj.magic `Latency_stats = hash_variant "Latency_stats")
    

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





(* local variables: *)
(* compile-command: "pmake util.cmo" *)
(* end: *)
