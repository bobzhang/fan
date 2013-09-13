
type t = {
    is_kwd : string -> bool;
      mutable filter : FToken.filter;
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
let string_of_error_msg = LibUtil.to_string_of_printer pp_print_error;;



(* [Sym] should all be filtered into keywords *)  
let keyword_conversion tok is_kwd =
  match tok with
  | `Sym s | `Lid s | `Uid s when is_kwd s -> `KEYWORD s
  | `Eident s -> `Lid s (* Eident *)
  | _ -> tok 

let check_keyword_as_label tok loc is_kwd =
  match tok with
  |`LABEL s | `OPTLABEL s when is_kwd s -> err (Keyword_as_label s) loc 
  | _               -> ()  

        
let check_unknown_keywords tok loc =
  match tok with
  | `Sym s -> err (Illegal_token s) loc
  | _        -> () 


let filter x =
  let f (tok, loc) = 
    let tok = keyword_conversion tok x.is_kwd in begin 
      check_keyword_as_label tok loc x.is_kwd ;
      (* if !error_on_unknown_keywords  then *)
      (*   check_unknown_keywords tok loc *)
      (* else (); *)
      (tok, loc)
    end in

  fun strm -> x.filter (XStream.map f strm)

let set_filter x f = x.filter <- f x.filter



let () =
  Printexc.register_printer (function
  |TokenError e -> Some (string_of_error_msg e)
  | _ -> None);;
