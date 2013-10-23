open Util
  
type domains =
    [ `Absolute of string list | `Sub of string list]

      
let pp_print_domains: Format.formatter -> domains -> unit =
  fun fmt  ->
    function
    | `Absolute (_a0:string list) ->
        Format.fprintf fmt "@[<1>(`Absolute@ %a)@]"
          (Formatf.pp_print_list Formatf.pp_print_string) _a0
    | `Sub _a0 ->
        Format.fprintf fmt "@[<1>(`Sub@ %a)@]"
          (Formatf.pp_print_list Formatf.pp_print_string) _a0

type name = (domains* string) 

let pp_print_name: Format.formatter -> name -> unit =
  fun fmt  _a0  ->
    (fun fmt  (_a0,_a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]" pp_print_domains _a0
         Format.pp_print_string _a1) fmt _a0

let string_of_name = Formatf.to_string pp_print_name
      
type quot = {
    name:name;
    loc:Locf.t; (* the starting location of the quot *) 
    meta:string option;(* a piece of small meta data, like loc name*)
    shift:int;
    content:string;
    retract:int; 
  }

type txt = {
    loc : Locf.t;
    txt : string;
  }
      
type ant  = {
    meta : string option;
    shift : int ;
    retract : int;
    loc : Locf.t;
    content : string;
  }
      
let pp_print_quot : Format.formatter -> quot -> unit =
  fun fmt {name;meta;shift;content;loc;retract}  ->
    Format.fprintf fmt "@[<1>{name=%a;@;loc=%a@;meta=%a;@;shift=%a@;retract=%a;@;content=%a}@]"
      pp_print_name name
      (Formatf.pp_print_option Formatf.pp_print_string) meta
      Locf.pp_print_t loc
      Format.pp_print_int shift
      Format.pp_print_int retract
      Format.pp_print_string content
  

type 'a expand_fun = Locf.t -> string option -> string -> 'a
let quot_expand (expander:'a expand_fun) (x:quot) =
  let loc =
    Location_util.join
      { x.loc with
        loc_start =
        { x.loc.loc_start with
          pos_cnum = x.loc.loc_start.pos_cnum + x.shift}} in
  let content =
    String.sub x.content x.shift (String.length x.content - x.retract - x.shift) in 
  expander loc x.meta content

type quotation = [ `Quot of quot ] 


let pp_print_quotation: Format.formatter -> quotation -> unit =
  fun fmt  (`Quot x)  ->
    Format.fprintf fmt "@[<1>(`Quot %a)@]"
      pp_print_quot x 

(** (name,contents)  *)
type dir_quotation =
    [`DirQuotation of quot]


let pp_print_dir_quotation: Format.formatter -> dir_quotation -> unit =
  fun fmt  (`DirQuotation x)  ->
    Format.fprintf fmt "@[<1>(`DirQuotation %a)@]"
      pp_print_quot x 


type loc = Locf.t

type line = {
    loc : loc;
    txt : string ;
    line : int;
    name : string option;
  }
      
type space_token = [
    `Comment of txt
  | `Blank of txt
  | `Newline of txt
  | `LINE_DIRECTIVE of line (* (loc * int * string option) *)
]


      
type t =
  [ `Key       of txt
  | `Sym       of txt
  | `Lid       of txt
  | `Uid       of txt
  | `Eident    of txt (* (+)*)

  | `Int       of txt
  | `Int32     of txt
  | `Int64     of txt
  | `Nativeint of txt
  | `Flo       of txt
  | `Chr       of txt
  | `Label     of txt
  | `Optlabel  of txt
  | `Str       of txt
  | space_token
   (* . *)
  | quotation
  | dir_quotation
  | `Ant       of (string * string )        
  | `EOI       of txt]
      
(* let eq (x:t) (y:t) = *)
(*   match (x,y) with *)
(*   | `Key (_,a), `Key(_,b) *)
(*   | `Sym (_,a), `Sym(_,b) *)
(*   | `Lid (_,a), `Lid(_,b) *)
(*   | `Uid (_,a), `Uid(_,b) *)
(*   | `Eident(_,a), `Eident(_,b) *)
(*   | `Int(_,a), `Int(_,b) *)
(*   | `Int32(_,a),`Int(_,b) *)
(*   | `Int64(_,a),`Int64(_,b) *)
(*   | `Nativeint(_,a),`Nativeint(_,b) *)
(*   | `Flo(_,a),`Flo(_,b) *)
(*   | `Chr(_,a),`Chr(_,b) *)
(*   | `Label(_,a),`Label(_,b) *)
(*   | `Optlabel(_,a),`Optlabel(_,b) *)
(*   | `Str(_,a),`Str(_,b) *)
(*   | `Comment(_,a),`Comment(_,b) *)
(*   | `Blank(_,a),`Blank(_,b) *)
(*       -> a = b  *)

(*   | `Newline(_,a,b),`Newline(_,a,b) *)

(*   | `Quot  *)
(*   | `DirQuotation *)
(*   | `Ant (loc,) *)

(*   | `LINE_DIRECTIVE(_,a),`LINE_DIRECTIVE(_,b) *)
(*   | `EOI,`EOI -> true        *)
(*   |  _,_ -> false  *)


      
let pp_print_t (fmt:Format.formatter)  (x:t) : unit =
  match x with 
  | `Key x ->
      Format.fprintf fmt "@[<1>(`Key@ %a)@]" Format.pp_print_string x.txt
  | `Sym x ->
      Format.fprintf fmt "@[<1>(`Sym@ %a)@]" Format.pp_print_string x.txt
  | `Lid x -> Format.fprintf fmt "@[<1>(`Lid@ %a)@]" Format.pp_print_string x.txt
  | `Uid x -> Format.fprintf fmt "@[<1>(`Uid@ %a)@]" Format.pp_print_string x.txt
  | `Eident x ->
      Format.fprintf fmt "@[<1>(`Eident@ %a)@]" Format.pp_print_string x.txt
  | `Int x ->
      Format.fprintf fmt "@[<1>(`Int@ %a)@]"
        Format.pp_print_string x.txt
  | `Int32 x ->
      Format.fprintf fmt "@[<1>(`Int32@ %a)@]" (* Format.pp_print_int32 _a0 *)
        Format.pp_print_string x.txt
  | `Int64 x ->
      Format.fprintf fmt "@[<1>(`Int6@ %a)@]" Format.pp_print_string x.txt
  | `Nativeint x ->
      Format.fprintf fmt "@[<1>(`Nativeint@ %a)@]" 
        Format.pp_print_string x.txt
  | `Flo x ->
      Format.fprintf fmt "@[<1>(`Flo@ %a)@]"
        Format.pp_print_string x.txt
  | `Chr x ->
      Format.fprintf fmt "@[<1>(`Chr@ %a)@]" 
        Format.pp_print_string x.txt
  | `Str x ->
      Format.fprintf fmt "@[<1>(`Str@ %a)@]" 
        Format.pp_print_string x.txt
  | `Label x ->
      Format.fprintf fmt "@[<1>(`Label@ %a)@]" Format.pp_print_string x.txt
  | `Optlabel x ->
      Format.fprintf fmt "@[<1>(`Optlabel@ %a)@]" Format.pp_print_string x.txt
  | #quotation as _a0 -> (pp_print_quotation fmt _a0 :>unit)
  | #dir_quotation as _a0 -> (pp_print_dir_quotation fmt _a0 :>unit)
  | `Ant (_a0,_a1) ->
      Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" Format.pp_print_string _a0
        Format.pp_print_string _a1
  | `Comment x ->
      Format.fprintf fmt "@[<1>(`Comment@ %a)@]" Format.pp_print_string x.txt
  | `Blank x ->
      Format.fprintf fmt "@[<1>(`Blank@ %a)@]" Format.pp_print_string x.txt
  | `Newline _ -> Format.fprintf fmt "`Newline"
  | `LINE_DIRECTIVE x ->
      Format.fprintf fmt
        "@[<1>(`LINE_DIRECTIVE@ %a@ %a)@]" Format.pp_print_int
        x.line (Formatf.pp_print_option Format.pp_print_string) x.name
  | `EOI _ -> Format.fprintf fmt "`EOI"

          

type 'a token  = [> t] as 'a


type stream = (t * Locf.t) Streamf.t 

type 'a estream  = ('a token  * Locf.t) Streamf.t 

type 'a parse  = stream -> 'a

type filter = stream -> stream
  



let token_to_string = Formatf.to_string pp_print_t

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
     with [ Streamf.Error _ -> false ];                        *)

        
let print ppf x = Format.pp_print_string ppf (to_string x)
    

let extract_string : [> t] -> string = function
  | `Key x | `Sym x | `Lid x | `Uid x | `Int  x | `Int32  x
  | `Int64  x | `Nativeint  x | `Flo  x | `Chr  x | `Str x
  | `Label x | `Optlabel x | `Comment x | `Blank x
  | `Eident x -> x.txt 
  | `LINE_DIRECTIVE x -> x.txt
  | tok ->
      invalid_argf "Cannot extract a string from this token: %s" (to_string tok)



let empty_name : name = (`Sub [],"")

let name_of_string s : name =
  match s.[0] with
  | '.' ->
    (match List.rev
        @@ List.filter Stringf.not_empty (Stringf.nsplit s "." )  with
    | x::xs -> (`Absolute (List.rev xs),x)
    | _ -> assert false )
      
  |'A' .. 'Z' ->
      (match List.rev
          (List.filter Stringf.not_empty (Stringf.nsplit s ".")) with
      | x::xs -> (`Sub (List.rev xs),x )
      | _ -> assert false)
  | _ -> (`Sub [],s)

(* local variables: *)
(* compile-command: "cd .. && pmake common/tokenf.cmo" *)
(* end: *)
