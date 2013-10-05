



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
(* compile-command: "pmake libUtil.cmo" *)
(* end: *)
