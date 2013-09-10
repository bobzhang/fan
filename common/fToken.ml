open LibUtil
  
type domains =
    [ `Absolute of string list | `Sub of string list]

      
let pp_print_domains: Format.formatter -> domains -> unit =
  fun fmt  ->
    function
    | `Absolute (_a0:string list) ->
        Format.fprintf fmt "@[<1>(`Absolute@ %a)@]"
          (Format.pp_print_list Format.pp_print_string) _a0
    | `Sub _a0 ->
        Format.fprintf fmt "@[<1>(`Sub@ %a)@]"
          (Format.pp_print_list Format.pp_print_string) _a0

type name = (domains* string) 

let pp_print_name: Format.formatter -> name -> unit =
  fun fmt  _a0  ->
    (fun fmt  (_a0,_a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_domains _a0
         Format.pp_print_string _a1) fmt _a0

type quotation = [ `QUOTATION of (name* string* int* string)] 

let pp_print_quotation: Format.formatter -> quotation -> unit =
  fun fmt  (`QUOTATION (_a0,_a1,_a2,_a3))  ->
    Format.fprintf fmt "@[<1>(`QUOTATION@ %a@ %a@ %a@ %a)@]" pp_print_name
      _a0 Format.pp_print_string _a1 Format.pp_print_int _a2 Format.pp_print_string _a3

(** (name,contents)  *)
type dir_quotation = [ `DirQuotation of (int* string* string)] 
let pp_print_dir_quotation: Format.formatter -> dir_quotation -> unit =
  fun fmt  (`DirQuotation (_a0,_a1,_a2))  ->
    Format.fprintf fmt "@[<1>(`DirQuotation@ %a@ %a@ %a)@]" Format.pp_print_int _a0
      Format.pp_print_string _a1 Format.pp_print_string _a2
type t =
  [ `KEYWORD of string | `SYMBOL of string | `Lid of string | `Uid of string
  | `ESCAPED_IDENT of string | `INT of (int* string)
  | `INT32 of (int32* string) | `INT64 of (int64* string)
  | `NATIVEINT of (nativeint* string) | `Flo of (float* string)
  | `CHAR of (char* string) | `STR of (string* string) | `LABEL of string
  | `OPTLABEL of string | quotation | dir_quotation
  | `Ant of (string* string) | `COMMENT of string | `BLANKS of string
  | `NEWLINE | `LINE_DIRECTIVE of (int* string option) | `EOI] 
let pp_print_t: Format.formatter -> t -> unit =
  fun fmt  ->
    function
    | `KEYWORD _a0 ->
        Format.fprintf fmt "@[<1>(`KEYWORD@ %a)@]" Format.pp_print_string _a0
    | `SYMBOL _a0 ->
        Format.fprintf fmt "@[<1>(`SYMBOL@ %a)@]" Format.pp_print_string _a0
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" Format.pp_print_string _a0
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" Format.pp_print_string _a0
    | `ESCAPED_IDENT _a0 ->
        Format.fprintf fmt "@[<1>(`ESCAPED_IDENT@ %a)@]" Format.pp_print_string _a0
    | `INT (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`INT@ %a@ %a)@]" Format.pp_print_int _a0
          Format.pp_print_string _a1
    | `INT32 (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`INT32@ %a@ %a)@]" Format.pp_print_int32 _a0
          Format.pp_print_string _a1
    | `INT64 (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`INT64@ %a@ %a)@]" Format.pp_print_int64 _a0
          Format.pp_print_string _a1
    | `NATIVEINT (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`NATIVEINT@ %a@ %a)@]" Format.pp_print_nativeint
          _a0 Format.pp_print_string _a1
    | `Flo (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Flo@ %a@ %a)@]" Format.pp_print_float _a0
          Format.pp_print_string _a1
    | `CHAR (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`CHAR@ %a@ %a)@]" Format.pp_print_char _a0
          Format.pp_print_string _a1
    | `STR (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`STR@ %a@ %a)@]" Format.pp_print_string _a0
          Format.pp_print_string _a1
    | `LABEL _a0 ->
        Format.fprintf fmt "@[<1>(`LABEL@ %a)@]" Format.pp_print_string _a0
    | `OPTLABEL _a0 ->
        Format.fprintf fmt "@[<1>(`OPTLABEL@ %a)@]" Format.pp_print_string _a0
    | #quotation as _a0 -> (pp_print_quotation fmt _a0 :>unit)
    | #dir_quotation as _a0 -> (pp_print_dir_quotation fmt _a0 :>unit)
    | `Ant (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" Format.pp_print_string _a0
          Format.pp_print_string _a1
    | `COMMENT _a0 ->
        Format.fprintf fmt "@[<1>(`COMMENT@ %a)@]" Format.pp_print_string _a0
    | `BLANKS _a0 ->
        Format.fprintf fmt "@[<1>(`BLANKS@ %a)@]" Format.pp_print_string _a0
    | `NEWLINE -> Format.fprintf fmt "`NEWLINE"
    | `LINE_DIRECTIVE (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LINE_DIRECTIVE@ %a@ %a)@]" Format.pp_print_int
          _a0 (Format.pp_print_option Format.pp_print_string) _a1
    | `EOI -> Format.fprintf fmt "`EOI"

          
type error =  
  | Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string* string)
  | Illegal_constructor of string

        
let pp_print_error: Format.formatter -> error -> unit =
  fun fmt  ->
    function
    | Illegal_token _a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token@ %a)@]" Format.pp_print_string _a0
    | Keyword_as_label _a0 ->
        Format.fprintf fmt "@[<1>(Keyword_as_label@ %a)@]" Format.pp_print_string
          _a0
    | Illegal_token_pattern _a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token_pattern@ %a)@]"
          (fun fmt  (_a0,_a1)  ->
             Format.fprintf fmt "@[<1>(%a,@,%a)@]" Format.pp_print_string _a0
               Format.pp_print_string _a1) _a0
    | Illegal_constructor _a0 ->
        Format.fprintf fmt "@[<1>(Illegal_constructor@ %a)@]" Format.pp_print_string
          _a0
          

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
  raise (FLoc.Exc_located (loc, TokenError error))

let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern (p_con, p_prm)))

let check_keyword _ = true
  (* FIXME let lb = Lexing.from_string s in
     let next () = token default_context lb in
     try
     match next () with
     [ SYMBOL _ | UidENT _ | LidENT _ -> (next () = EOI)
     | _ -> false ]
     with [ XStream.Error _ -> false ];                        *)

let error_on_unknown_keywords = ref false

(* let rec ignore_layout (__strm : _ XStream.t) = *)
(*   match XStream.peek __strm with *)
(*   | Some ((`COMMENT _|`BLANKS _|`NEWLINE|`LINE_DIRECTIVE _),_) -> *)
(*       (XStream.junk __strm; ignore_layout __strm) *)
(*   | Some x -> *)
(*       (XStream.junk __strm; *)
(*        (let s = __strm in *)
(*         XStream.icons x (XStream.slazy (fun _  -> ignore_layout s)))) *)
(*   | _ -> XStream.sempty *)

        
let print ppf x = Format.pp_print_string ppf (to_string x)
    

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
    (match List.rev
        (List.filter String.not_empty (String.nsplit s "." ) ) with
    | x::xs -> (`Absolute (List.rev xs),x)
    | _ -> assert false )
      
  |'A' .. 'Z' ->
      (match List.rev
          (List.filter String.not_empty (String.nsplit s ".")) with
      | x::xs -> (`Sub (List.rev xs),x )
      | _ -> assert false) 
  | _ -> (`Sub [],s)



let names_tbl : (domains,SSet.t) Hashtbl.t =
  Hashtbl.create 30 
    
(**  when no qualified path is given , it uses [Sub []] *)
let resolve_name loc (n:name) : name =
  match n with
  |(`Sub _ as x) ,v ->
      (try
        let r =
          List.find
            (fun path  ->
              (try
                let set = Hashtbl.find names_tbl (concat_domain (path, x)) in
                fun ()  -> SSet.mem v set
              with | Not_found  -> (fun ()  -> false)) ()) paths.contents in
        fun ()  -> ((concat_domain (r, x)), v)
      with  Not_found  -> (fun ()  -> FLoc.errorf loc "resolve_name %s" v)) ()
  | x ->  x
