




      
(*
  For some tokens the data constructor holds two representations with the
  evaluated one and the source one. For example
  the INT data constructor holds an integer and a string, this string can
  contains more information that's needed for a good pretty-printing
  ("42", "4_2", "0000042", "0b0101010"...).

  The meaning of the tokens are:
  -      [KEYWORD s] is the keyword [s].
  -      [LidENT s] is the ident [s] starting with a lowercase letter.
  -      [UidENT s] is the ident [s] starting with an uppercase letter.
  -      [INT i s] (resp. [INT32 i s], [INT64 i s] and [NATIVEINT i s])
         the integer constant [i] whose string source is [s].
  -      [FLOAT f s] is the float constant [f] whose string source is [s].
  -      [STRING s s'] is the string constant [s] whose string source is [s'].
  -      [CHAR c s] is the character constant [c] whose string source is [s].
  -      [QUOTATION q] is a quotation [q], see {!AstQuotation.t} for more information.
  -      [ANTIQUOT n s] is an antiquotation [n] holding the string [s].
  -      [EOI] is the end of input.

  Warning: the second string associated with the constructor [STRING] is
  the string found in the source without any interpretation. In particular,
  the backslashes are not interpreted. For example, if the input is ["\n"]
  the string is *not* a string with one element containing the character
  "return", but a string of two elements: the backslash and the character
  ["n"]. To interpret a string use the first string of the [STRING]
  constructor (or if you need to compute it use the module
  {!TokenEval}. Same thing for the constructor [CHAR]. *)
{:fans|
keep on;
derive(Print);
|};
open StdLib;

{:ocaml|
(** The generic quotation type.
    To see how fields are used here is an example:
    {:q_name@q_loc|q_contents|}
    The last one, q_shift is equal to the length of "<:q_name@q_loc<". *)

(* domain is the namespace all begins with capital letters *)
  
type domains =
    [= `Absolute of list string | `Sub of list string]  ;
type name = (domains*string);

type quotation ={
    q_name : name;
    q_loc : string;
    q_shift : int;
    q_contents : string
  };

type t =
  [=  `KEYWORD of string
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
  | `QUOTATION of quotation
  | `Ant of (string * string )
  | `COMMENT of string
  | `BLANKS of string
  | `NEWLINE
  | `LINE_DIRECTIVE of (int * option string )
  | `EOI];
type error = 
  [ Illegal_token of string
  | Keyword_as_label of string
  | Illegal_token_pattern of (string * string)
  | Illegal_constructor of string];
|};

type 'a token  = [> t] as 'a;


type stream = XStream.t (t * FanLoc.t);

type 'a estream  = XStream.t (token 'a * FanLoc.t);

type 'a parse  = stream -> 'a;

type filter = stream -> stream;

  
exception TokenError of  error;

open LibUtil;
let string_of_error_msg = to_string_of_printer pp_print_error;

Printexc.register_printer (fun
  [TokenError e -> Some (string_of_error_msg e)
  | _ -> None]);
let token_to_string = to_string_of_printer pp_print_t;  

let to_string = fun
  [ #t as x -> token_to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token"]; (* FIXME*)
  
let err error loc =
  raise (FanLoc.Exc_located loc (TokenError error));
    

let error_no_respect_rules p_con p_prm =
  raise (TokenError (Illegal_token_pattern p_con p_prm));

let check_keyword _ = true;
  (* FIXME let lb = Lexing.from_string s in
     let next () = token default_context lb in
     try
     match next () with
     [ SYMBOL _ | UidENT _ | LidENT _ -> (next () = EOI)
     | _ -> false ]
     with [ XStream.Error _ -> false ];                        *)

let error_on_unknown_keywords = ref false;

let rec ignore_layout  = parser
  [ [< (`COMMENT _ | `BLANKS _ | `NEWLINE | `LINE_DIRECTIVE _ , _); 's >] ->
    ignore_layout s
  | [< x; 's >] -> [< x; '(ignore_layout s) >]
  | [< >] -> [< >] ];

  
let print ppf x = pp_print_string ppf (to_string x);
    
let match_keyword kwd =  fun
  [ `KEYWORD kwd' when kwd = kwd' -> true
  | _ -> false ];

(*
  {[
  x=STRING -> extract_string x
  ]}
 *)  
let extract_string : [> t] -> string = fun
  [ `KEYWORD s | `SYMBOL s | `Lid s | `Uid s | `INT (_, s) | `INT32 (_, s) |
  `INT64 (_, s) | `NATIVEINT (_ ,s) | `Flo (_, s) | `CHAR (_, s) | `STR (_, s) |
  `LABEL s | `OPTLABEL s | `COMMENT s | `BLANKS s | `ESCAPED_IDENT s-> s
  | tok ->
      invalid_arg ("Cannot extract a string from this token: "^ to_string tok) ];


(* [SYMBOL] should all be filtered into keywords *)  
let keyword_conversion tok is_kwd = match tok with
  [ `SYMBOL s | `Lid s | `Uid s when is_kwd s -> `KEYWORD s
  | `ESCAPED_IDENT s -> `Lid s (* ESCAPED_IDENT *)
  | _ -> tok ];

let check_keyword_as_label tok loc is_kwd =
  let s =  match tok with
  [ `LABEL s         -> s
  | `OPTLABEL s      -> s
  | _               -> "" ] in
  if s <> "" && is_kwd s then err (Keyword_as_label s) loc else ();
    
let check_unknown_keywords tok loc =
  match tok with
  [ `SYMBOL s -> err (Illegal_token s) loc
  | _        -> () ];
  
  



    
let string_of_domains  = fun
  [`Absolute xs -> "." ^ String.concat "." xs
  |`Sub ls -> String.concat "." ls ];
  


let string_of_name (x,y) =
  string_of_domains x ^ "." ^ y  ;
  
(* [only absolute] domains can be stored
 *)  
let paths :  ref (list domains) =
  ref [ `Absolute ["Fan";"Lang"];
        `Absolute ["Fan";"Lang";"Meta"];
        `Absolute ["Fan";"Lang";"Filter"];
      ];

let concat_domain = fun
  [(`Absolute xs,`Sub ys) -> `Absolute (xs@ys)
  | _ -> invalid_arg "concat_domain"];

let empty_name : name = (`Sub [],"");

let name_of_string s : name =
  match s.[0] with
  ['.' ->
    (match List.rev (List.filter String.not_empty (String.nsplit s "." ) )with
    [ [x::xs] -> (`Absolute (List.rev xs),x)
    | _ -> assert false ])
      
  |'A' .. 'Z' ->
      (match List.rev (List.filter String.not_empty (String.nsplit s ".")) with
       [ [x::xs] -> (`Sub (List.rev xs),x )
       | _ -> assert false]) 
  | _ -> (`Sub [],s)];  



let names_tbl : Hashtbl.t domains SSet.t =
  Hashtbl.create 30 ;
    
(*
  when no qualified path is given , it uses [Sub []]
 *)
let resolve_name (n:name) : name =
  match n with
  [((`Sub _ as x) ,v) ->
    let try r =
      List.find
      (fun path ->
        let try set = Hashtbl.find names_tbl (concat_domain (path, x)) in
        SSet.mem v set
        with Not_found -> false) !paths in
    (concat_domain (r, x),v)
    with [Not_found -> failwithf "resolve_name %s" v]
  | x ->  x];
