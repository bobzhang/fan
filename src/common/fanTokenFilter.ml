
type t = {
    mutable kwds : Setf.String.t;
    mutable filter : Ftoken.filter;
  }

type error =  
  | Illegal_token of string
  | Keyword_as_label of string

exception TokenError of  error


let err error loc =
  raise (FLoc.Exc_located (loc, TokenError error))

let pp_print_error: Format.formatter -> error -> unit =
  fun fmt  ->
    function
    | Illegal_token _a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token@ %a)@]" Format.pp_print_string _a0
    | Keyword_as_label _a0 ->
        Format.fprintf fmt "@[<1>(Keyword_as_label@ %a)@]" Format.pp_print_string
          _a0
let string_of_error_msg = Formatf.to_string pp_print_error;;



(* [Sym] should all be filtered into keywords *)  
let keyword_conversion tok kwds =
  match tok with
  | `Sym s | `Lid s | `Uid s when Setf.String.mem s  kwds -> `Key s
  | `Eident s -> `Lid s 
  | _ -> tok 

let check_keyword_as_label tok loc kwds =
  match tok with
  |`Label s | `Optlabel s when Setf.String.mem s kwds -> err (Keyword_as_label s) loc 
  | _               -> ()  

        
let check_unknown_keywords tok loc =
  match tok with
  | `Sym s -> err (Illegal_token s) loc
  | _        -> () 


let filter x =
  let f (tok, loc) = 
    let tok = keyword_conversion tok x.kwds in begin 
      check_keyword_as_label tok loc x.kwds ;
      (* if !error_on_unknown_keywords  then *)
      (*   check_unknown_keywords tok loc *)
      (* else (); *)
      (tok, loc)
    end in

  fun strm -> x.filter (Fstream.map f strm)

let set_filter x f = x.filter <- f x.filter



let () =
  Printexc.register_printer (function
  |TokenError e -> Some (string_of_error_msg e)
  | _ -> None);;
