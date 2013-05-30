open LibUtil
open StdFan

type domains =
    [ `Absolute of string list | `Sub of string list]  with ("Print")

type name = (domains*string) with ("Print")


type t =
  [  `KEYWORD of string
  | `SYMBOL of string
  | `Lid of string
  | `Uid of string
  | `ESCAPED_IDENT of string (* (+)*)
  | `INT of (int * string )
  | `INT32 of (int32 * string )
  | `INT64 of (int64 * string )
  | `NATIVEINT of (nativeint * string )
  | `Flo of (float * string )
  | `CHAR of (char * string )
  | `STR of (string * string )
  | `LABEL of string
  | `OPTLABEL of string
        (* (name,loc,shift,contents) *)
  | `QUOTATION of (name*string*int*string)(* quotation *)
        
  | `Ant of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int * string option )
  | `EOI] with ("Print")
type error = 
  | Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string * string)
  | Illegal_constructor of string with ("Print")


type 'a token  = [> t] as 'a


type stream = (t * FLoc.t) XStream.t 

type 'a estream  = ('a token  * FLoc.t) XStream.t 

type 'a parse  = stream -> 'a

type filter = stream -> stream
  
exception TokenError of  error

let string_of_error_msg = to_string_of_printer pp_print_error;;

Printexc.register_printer (function
  |TokenError e -> Some (string_of_error_msg e)
  | _ -> None);;

let token_to_string = to_string_of_printer pp_print_t

let to_string = function
  | #t as x -> token_to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token" (* FIXME*)
  
let err error loc =
  raise (FLoc.Exc_located loc (TokenError error))

let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern p_con p_prm))

let check_keyword _ = true
  (* FIXME let lb = Lexing.from_string s in
     let next () = token default_context lb in
     try
     match next () with
     [ SYMBOL _ | UidENT _ | LidENT _ -> (next () = EOI)
     | _ -> false ]
     with [ XStream.Error _ -> false ];                        *)

let error_on_unknown_keywords = ref false

let rec ignore_layout  = parser
  | (`COMMENT _ | `BLANKS _ | `NEWLINE | `LINE_DIRECTIVE _ , _); 's  ->
      ignore_layout s
  |  x; 's  -> {:stream| x; 'ignore_layout s |}
  |  -> {:stream||}
      
let print ppf x = pp_print_string ppf (to_string x)
    
let match_keyword kwd =  function
  | `KEYWORD kwd' when kwd = kwd' -> true
  | _ -> false 

let extract_string : [> t] -> string = function
  | `KEYWORD s | `SYMBOL s | `Lid s | `Uid s | `INT (_, s) | `INT32 (_, s) |
  `INT64 (_, s) | `NATIVEINT (_ ,s) | `Flo (_, s) | `CHAR (_, s) | `STR (_, s) |
  `LABEL s | `OPTLABEL s | `COMMENT s | `BLANKS s | `ESCAPED_IDENT s-> s
  | tok ->
      invalid_argf "Cannot extract a string from this token: %s" (to_string tok)

(* [SYMBOL] should all be filtered into keywords *)  
let keyword_conversion tok is_kwd =
  match tok with
  | `SYMBOL s | `Lid s | `Uid s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `Lid s (* ESCAPED_IDENT *)
  | _ -> tok 

let check_keyword_as_label tok loc is_kwd =
  let s =
    match tok with
    |`LABEL s         -> s
    | `OPTLABEL s      -> s
    | _               -> ""  in
  if s <> "" && is_kwd s then err (Keyword_as_label s) loc else ()
    
let check_unknown_keywords tok loc =
  match tok with
  | `SYMBOL s -> err (Illegal_token s) loc
  | _        -> () 

    
let string_of_domains  = function
  |`Absolute xs -> "." ^ String.concat "." xs
  |`Sub ls -> String.concat "." ls 
  

let string_of_name (x,y) =
  string_of_domains x ^ "." ^ y  
  
(* [only absolute] domains can be stored *)  
let paths :  domains list ref  =
  ref [ `Absolute ["Fan";"Lang"];
        `Absolute ["Fan";"Lang";"Meta"];
        `Absolute ["Fan";"Lang";"Filter"]]

let concat_domain = function
  |(`Absolute xs,`Sub ys) -> `Absolute (xs@ys)
  | _ -> invalid_arg "concat_domain"

let empty_name : name = (`Sub [],"")

let name_of_string s : name =
  match s.[0] with
  | '.' ->
    (match List.rev (List.filter String.not_empty (String.nsplit s "." ) )with
    | x::xs -> (`Absolute (List.rev xs),x)
    | _ -> assert false )
      
  |'A' .. 'Z' ->
      (match List.rev (List.filter String.not_empty (String.nsplit s ".")) with
      | x::xs -> (`Sub (List.rev xs),x )
      | _ -> assert false) 
  | _ -> (`Sub [],s)



let names_tbl : (domains,SSet.t) Hashtbl.t =
  Hashtbl.create 30 
    
(**  when no qualified path is given , it uses [Sub []] *)
let resolve_name (n:name) : name =
  match n with
  |((`Sub _ as x) ,v) ->
      (let try r =
        List.find
          (fun path ->
            let try set = Hashtbl.find names_tbl (concat_domain (path, x)) in
            SSet.mem v set
            with Not_found -> false) !paths in
      (concat_domain (r, x),v)
      with Not_found -> failwithf "resolve_name %s" v)
  | x ->  x
