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

let string_of_name = to_string_of_printer pp_print_name
      
type quot = {
    name:name;
    loc:FLoc.t; (* the starting location of the quot *) 
    meta:string option;(* a piece of small meta data, like loc name*)
    shift:int;
    content:string;
    retract:int; 
  }

let quot_expand expander (x:quot) =
  let loc = Location_util.join (FLoc.move `start x.shift x.loc) in
  let content =
    String.sub x.content x.shift (String.length x.content - x.retract - x.shift) in 
  expander loc x.meta content

type quotation = [ `Quot of quot ] 

(* FIXME *)      
let pp_print_quotation: Format.formatter -> quotation -> unit =
  fun fmt  (`Quot {name;meta;shift;content;loc;retract} )  ->
    Format.fprintf fmt "@[<1>(`Quot {name=%a;@;loc=%a@;meta=%a;@;shift=%a@;retract=%a;@;content=%a})@]"
      pp_print_name name
      (Format.pp_print_option Format.pp_print_string) meta
      FLoc.pp_print_t loc
      Format.pp_print_int shift
      Format.pp_print_int retract
      Format.pp_print_string content

(** (name,contents)  *)
type dir_quotation = [ `DirQuotation of (int* string* string)] 
let pp_print_dir_quotation: Format.formatter -> dir_quotation -> unit =
  fun fmt  (`DirQuotation (_a0,_a1,_a2))  ->
    Format.fprintf fmt "@[<1>(`DirQuotation@ %a@ %a@ %a)@]" Format.pp_print_int _a0
      Format.pp_print_string _a1 Format.pp_print_string _a2

type space_token =
   [ `Comment of string
   | `Blank of string
   | `NEWLINE
   | `LINE_DIRECTIVE of (int * string option) ]
      
type t =
  [ `Key of string | `Sym of string | `Lid of string | `Uid of string
  | `Eident of string | `Int of string
  | `Int32 of string | `Int64 of  string
  | `Nativeint of  string | `Flo of  string
  | `Chr of  string | `Str of string | `Label of string
  | `Optlabel of string | quotation | dir_quotation
  | `Ant of (string* string)
  | space_token
  | `EOI]


      
let pp_print_t: Format.formatter -> t -> unit =
  fun fmt  ->
    function
    | `Key _a0 ->
        Format.fprintf fmt "@[<1>(`Key@ %a)@]" Format.pp_print_string _a0
    | `Sym _a0 ->
        Format.fprintf fmt "@[<1>(`Sym@ %a)@]" Format.pp_print_string _a0
    | `Lid _a0 -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" Format.pp_print_string _a0
    | `Uid _a0 -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" Format.pp_print_string _a0
    | `Eident _a0 ->
        Format.fprintf fmt "@[<1>(`Eident@ %a)@]" Format.pp_print_string _a0
    | `Int _a1 ->
        Format.fprintf fmt "@[<1>(`Int@ %a)@]"
          Format.pp_print_string _a1
    | `Int32 _a1 ->
        Format.fprintf fmt "@[<1>(`Int32@ %a)@]" (* Format.pp_print_int32 _a0 *)
          Format.pp_print_string _a1
    | `Int64 _a1 ->
        Format.fprintf fmt "@[<1>(`Int6@ %a)@]" Format.pp_print_string _a1
    | `Nativeint _a1 ->
        Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]" 
           Format.pp_print_string _a1
    | `Flo _a1 ->
        Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
          Format.pp_print_string _a1
    | `Chr (_a1) ->
        Format.fprintf fmt "@[<1>(`Chr@ %a)@]" 
          Format.pp_print_string _a1
    | `Str _a1 ->
        Format.fprintf fmt "@[<1>(`Str@ %a)@]" 
          Format.pp_print_string _a1
    | `Label _a0 ->
        Format.fprintf fmt "@[<1>(`Label@ %a)@]" Format.pp_print_string _a0
    | `Optlabel _a0 ->
        Format.fprintf fmt "@[<1>(`Optlabel@ %a)@]" Format.pp_print_string _a0
    | #quotation as _a0 -> (pp_print_quotation fmt _a0 :>unit)
    | #dir_quotation as _a0 -> (pp_print_dir_quotation fmt _a0 :>unit)
    | `Ant (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" Format.pp_print_string _a0
          Format.pp_print_string _a1
    | `Comment _a0 ->
        Format.fprintf fmt "@[<1>(`Comment@ %a)@]" Format.pp_print_string _a0
    | `Blank _a0 ->
        Format.fprintf fmt "@[<1>(`Blank@ %a)@]" Format.pp_print_string _a0
    | `NEWLINE -> Format.fprintf fmt "`NEWLINE"
    | `LINE_DIRECTIVE (_a0,_a1) ->
        Format.fprintf fmt "@[<1>(`LINE_DIRECTIVE@ %a@ %a)@]" Format.pp_print_int
          _a0 (Format.pp_print_option Format.pp_print_string) _a1
    | `EOI -> Format.fprintf fmt "`EOI"

          

type 'a token  = [> t] as 'a


type stream = (t * FLoc.t) Fstream.t 

type 'a estream  = ('a token  * FLoc.t) Fstream.t 

type 'a parse  = stream -> 'a

type filter = stream -> stream
  



let token_to_string = to_string_of_printer pp_print_t

let to_string = function
  | #t as x -> token_to_string x
  | _ -> invalid_arg "token_to_string not implemented for this token" (* FIXME*)
  
(* let check_keyword _ = true *)
  (* FIXME let lb = Lexing.from_string s in
     let next () = token default_context lb in
     try
     match next () with
     [ Sym _ | UidENT _ | LidENT _ -> (next () = EOI)
     | _ -> false ]
     with [ Fstream.Error _ -> false ];                        *)

        
let print ppf x = Format.pp_print_string ppf (to_string x)
    

let extract_string : [> t] -> string = function
  | `Key s | `Sym s | `Lid s | `Uid s | `Int  s | `Int32  s |
  `Int64  s | `Nativeint  s | `Flo  s | `Chr  s | `Str  s |
  `Label s | `Optlabel s | `Comment s | `Blank s | `Eident s-> s
  | tok ->
      invalid_argf "Cannot extract a string from this token: %s" (to_string tok)



let empty_name : name = (`Sub [],"")

let name_of_string s : name =
  match s.[0] with
  | '.' ->
    (match List.rev
        @@ List.filter Fstring.not_empty (Fstring.nsplit s "." )  with
    | x::xs -> (`Absolute (List.rev xs),x)
    | _ -> assert false )
      
  |'A' .. 'Z' ->
      (match List.rev
          (List.filter Fstring.not_empty (Fstring.nsplit s ".")) with
      | x::xs -> (`Sub (List.rev xs),x )
      | _ -> assert false)
  | _ -> (`Sub [],s)

(* local variables: *)
(* compile-command: "cd .. && pmake common/ftoken.cmo" *)
(* end: *)
