
type t = {
    mutable kwds : Setf.String.t;
    mutable filter : Tokenf.filter;
  }

type error =  
  | Illegal_token of string
  | Keyword_as_label of string

exception TokenError of  error


let err error loc =
  raise (Locf.Exc_located (loc, TokenError error))

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
let keyword_conversion (tok:Tokenf.t) kwds =
  match tok with
  | `Sym u  | `Lid u
  | `Uid u when Setf.String.mem u.txt  kwds -> `Key u
  | `Eident u -> `Lid u
  | _ -> tok 

let check_keyword_as_label (tok:Tokenf.t)  kwds =
  match tok with
  |`Label u | `Optlabel u when Setf.String.mem u.txt kwds
    -> err (Keyword_as_label u.txt) u.loc 
  | _               -> ()  

        
let check_unknown_keywords (tok:Tokenf.t) loc =
  match tok with
  | `Sym s -> err (Illegal_token s.txt) loc
  | _        -> () 


let filter x =
  let f (tok:Tokenf.t) = 
    let tok = keyword_conversion tok x.kwds in begin 
      check_keyword_as_label tok  x.kwds ;
      (* if !error_on_unknown_keywords  then *)
      (*   check_unknown_keywords tok loc *)
      (* else (); *)
      tok
    end in
  fun strm -> x.filter (Streamf.map f strm)

let set_filter x f = x.filter <- f x.filter



let () =
  Printexc.register_printer (function
  |TokenError e -> Some (string_of_error_msg e)
  | _ -> None);;

(* local variables: *)
(* compile-command: "cd .. && pmake common/fanTokenFilter.cmo" *)
(* end: *)
