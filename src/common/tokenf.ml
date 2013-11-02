type loc = Locf.t

  
type domains =
    [ `Absolute of string list | `Sub of string list]

type name = (domains* string)
      
let pp_print_domains : domains Formatf.t = 
  fun fmt  ->
    function
    | `Absolute (_a0:string list) ->
        Format.fprintf fmt ".%s" (String.concat "." _a0)
    | `Sub _a0 ->
        Format.fprintf fmt "%s" (String.concat "." _a0)

let pp_print_name : Format.formatter -> name -> unit =
  fun fmt  _a0  ->
    (fun fmt  (_a0,_a1)  ->
       Format.fprintf fmt "@[<1>(%a,@,%a)@]"
        pp_print_domains _a0
        Format.pp_print_string _a1) fmt _a0


let string_of_name = Formatf.to_string pp_print_name
            



type quot = {
    name:name;
    loc: loc; (* the starting location of the quot *) 
    meta:string option;(* a piece of small meta data, like loc name*)
    shift:int;
    (* content:string; *)
    txt : string;
    retract:int; 
  }

type txt = {
    loc : loc ;
    txt : string;
  }
(*
type symbol ={
    loc : loc;
    txt : string;
    precedence : int;
  }
*)
type ant = {
    loc : loc ;
    cxt : string option;
    kind : string;
    txt : string; (* whole *)
    shift : int;
    retract : int;
  }

let ant_expand p (x:ant) =
  let content =
    String.sub x.txt x.shift (String.length x.txt - x.retract - x.shift ) in
  let loc =
    Location_util.join
      { x.loc with
        loc_start =
        { x.loc.loc_start with
          pos_cnum = x.loc.loc_start.pos_cnum + x.shift}} in
  p loc content

let mk_ant ?c  (x:ant)  =
  match c with
  | None -> `Ant(x.loc,x)
  | Some _  -> `Ant(x.loc, {x with cxt = c })

(* BOOTSTRAPING *)
let pp_print_ant fmt  (x:ant) =
  Format.fprintf fmt "cxt:%S;content:%S"
  (match x.cxt with None ->""|Some s -> s)  x.txt 
      
let pp_print_quot : Format.formatter -> quot -> unit =
  fun fmt {name;meta;shift;txt;loc;retract}  ->
    Format.fprintf fmt
      "@[<1>{name=%a;@;loc=%a@;meta=%a;@;shift=%a@;retract=%a;@;txt=%a}@]"
      pp_print_name name
      (Formatf.pp_print_option Formatf.pp_print_string) meta
      Locf.pp_print_t loc
      Format.pp_print_int shift
      Format.pp_print_int retract
      Format.pp_print_string txt

type 'a expand_fun = Locf.t -> string option -> string -> 'a
      
type quotation = [ `Quot of quot ] 
(** (name,contents)  *)
type dir_quotation =
    [`DirQuotation of quot]
type line = {
    loc : loc;
    txt : string ;
    line : int;
    name : string option;}
      
type space_token =
    [`Comment of txt
    | `Blank of txt
    | `Newline of txt
    | `LINE_DIRECTIVE of line]

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
  | `EOI       of txt
  | `Pre       of txt 
  | quotation
  | dir_quotation
  | `Ant       of ant ]

let quot_expand (expander:'a expand_fun) (x:quot) =
  let loc =
    Location_util.join
      { x.loc with
        loc_start =
        { x.loc.loc_start with
          pos_cnum = x.loc.loc_start.pos_cnum + x.shift}} in
  let content =
    String.sub x.txt x.shift (String.length x.txt - x.retract - x.shift) in 
  expander loc x.meta content
      
let pp_print_quotation: Format.formatter -> quotation -> unit =
  fun fmt  (`Quot x)  ->
    Format.fprintf fmt "@[<1>(`Quot %a)@]"
      pp_print_quot x 


let pp_print_dir_quotation: Format.formatter -> dir_quotation -> unit =
  fun fmt  (`DirQuotation x)  ->
    Format.fprintf fmt "@[<1>(`DirQuotation %a)@]"
      pp_print_quot x 

(* idea: match x with %exp{ (( `Pre | `Key | `Sym | `Lid | `Uid) as s) x } ->*)      
let pp_print_t (fmt:Format.formatter)  (x:t) : unit =
  match x with
  | `Pre x -> 
      Format.fprintf fmt "@[<1>(`Pre@ %a)@]" Format.pp_print_string x.txt
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
      Format.fprintf fmt "@[<1>(`Int32@ %a)@]"
        Format.pp_print_string x.txt
  | `Int64 x ->
      Format.fprintf fmt "@[<1>(`Int6@ %a)@]"
        Format.pp_print_string x.txt
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
  | `Ant x  ->
      Format.fprintf fmt "@[<1>(`Ant@ %a@ %a)@]" Format.pp_print_string x.kind
        Format.pp_print_string x.txt 
  | `EOI _ -> Format.fprintf fmt "`EOI"

type stream = t Streamf.t 

type 'a parse  = stream -> 'a

type filter = stream -> stream
  
type filter_error =
  | Illegal_token of string
  | Keyword_as_label of string

exception TokenError of  filter_error

let filter_err error loc = raise @@ Locf.Exc_located (loc, TokenError error)

let pp_print_error: filter_error Formatf.t  =
  fun fmt  ->
    function
    | Illegal_token _a0 ->
        Format.fprintf fmt "@[<1>(Illegal_token@ %a)@]" Format.pp_print_string _a0
    | Keyword_as_label _a0 ->
        Format.fprintf fmt "@[<1>(Keyword_as_label@ %a)@]" Format.pp_print_string
          _a0
let string_of_error_msg = Formatf.to_string pp_print_error;;

(* [Sym] should always be filtered into keywords *)  
let keyword_conversion (tok:t) kwds =
  match tok with
  | `Sym u  | `Lid u
  | `Uid u when Setf.String.mem u.txt  kwds -> `Key u
  | `Eident u -> `Lid u
  | _ -> tok 

let check_keyword_as_label (tok:t)  kwds =
  match tok with
  |`Label u | `Optlabel u when Setf.String.mem u.txt kwds
    -> filter_err (Keyword_as_label u.txt) u.loc 
  | _               -> ()  

type filter_plugin = {
    mutable kwds : Setf.String.t;
    mutable filter : filter option;
  }        
let check_unknown_keywords (tok:t) loc =
  match tok with
  | `Sym s -> filter_err (Illegal_token s.txt) loc
  | _        -> () 

let filter x =
  let f (t:t) =
    let t = keyword_conversion t x.kwds in begin
      check_keyword_as_label t x.kwds;
      t 
    end in
  match x.filter with
  | None -> Streamf.map f 
  | Some filter -> fun strm -> filter (Streamf.map f strm)



    

(* BOOTSTRAPPING --  *)
let to_string = Formatf.to_string pp_print_t
        
let print ppf x = Format.pp_print_string ppf (to_string x)


let strip (x:t) : Obj.t  =
  Obj.field (Obj.repr x) 1 
 

let get_string (x:t) :  string =
  match x with
  | `Pre x 
  | `Key x
  | `Sym x
  | `Lid x
  | `Uid x
  | `Int  x
  | `Int32  x
  | `Int64  x
  | `Nativeint  x
  | `Flo  x
  | `Chr  x
  | `Str x
  | `Label x
  | `Optlabel x
  | `EOI x  
  | `Eident x -> x.txt
  | `Quot x -> x.txt
  | `DirQuotation x -> x.txt        
  | `Ant x -> x.txt

let get_loc (x:t) =
  match x with
  | `Key x
  | `Sym x
  | `Lid x
  | `Uid x
  | `Int  x
  | `Int32  x
  | `Int64  x
  | `Nativeint  x
  | `Flo  x
  | `Chr  x
  | `Pre x 
  | `Str x
  | `Label x
  | `Optlabel x
  | `EOI x 
  | `Eident x -> x.loc
  | `Ant x -> x.loc
  | `Quot x -> x.loc
  | `DirQuotation x -> x.loc
    



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
        
let () =
  Printexc.register_printer @@ function
  |TokenError e -> Some (string_of_error_msg e)
  | _ -> None

(* local variables: *)
(* compile-command: "cd .. && pmake common/tokenf.cmo" *)
(* end: *)
