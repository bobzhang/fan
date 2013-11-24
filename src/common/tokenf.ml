type loc = Locf.t

  
type domains =
    [ `Absolute of string list | `Sub of string list]

type name = {
    domains : domains;
    name : string
  }
      
let pp_print_domains : domains Formatf.t = 
  fun fmt  ->
    function
    | `Absolute (_a0:string list) ->
        Format.fprintf fmt ".%s" (String.concat "." _a0)
    | `Sub _a0 ->
        Format.fprintf fmt "%s" (String.concat "." _a0)

let pp_print_name fmt (x:name) =
  Format.fprintf fmt "%a.%a" pp_print_domains x.domains Format.pp_print_string x.name
      


let string_of_name = Formatf.to_string pp_print_name
            



type quot = {
    loc : loc; (* the starting location of the quot *)
    txt : string;
    name : name;
    meta : string option;(* a piece of small meta data, like loc name*)
    shift : int;
    (* content:string; *)
    retract:int; 
  }

type txt = {
    loc : loc ;
    txt : string;
  }
type op = {
    loc : loc;
    txt : string;
    level : int;
  }
      
type ant = {
    loc : loc ;
    txt : string; (* whole *)
    cxt : string option;
    kind : string;
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
  [ `Key          of txt
  | `Lid          of txt
  | `Uid          of txt
  | `Int          of txt
  | `Int32        of txt
  | `Int64        of txt
  | `Nativeint    of txt
  | `Flo          of txt
  | `Chr          of txt
  | `Label        of txt
  | `Optlabel     of txt
  | `Str          of txt
  | `EOI          of txt
  | `Pre          of txt
  | `Inf          of op
  | `Quot         of quot
  | `DirQuotation of quot
  | `Ant          of ant ]
type tag =
  [`Key          
  | `Lid          
  | `Uid          
  | `Int          
  | `Int32        
  | `Int64        
  | `Nativeint    
  | `Flo          
  | `Chr          
  | `Label        
  | `Optlabel     
  | `Str          
  | `EOI          
  | `Pre          
  | `Inf          
  | `Quot         
  | `DirQuotation 
  | `Ant]

type word =
  | Any
  | A of string
  | Kind of string
  | Level of int

and descr =  {
    tag : tag;
    word : word;
    tag_name : string;
  }

type pattern = {
    descr : descr
  }

      
let string_of_descr (x:descr)=
  match x.word with
  | Any -> x.tag_name 
  | Kind s -> x.tag_name ^ " " ^ s
  | A  s -> Printf.sprintf "%S" s
  | Level d -> x.tag_name ^ Printf.sprintf "Level %d" d
        

let string_of_pattern (x:pattern) = string_of_descr x.descr
    
let eq_pattern  (x :pattern)  (y : pattern) : bool =
  x.descr = y.descr
      
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
  | `Inf x ->
      Format.fprintf fmt "@[<1>(`Inf@ %a)@]" Format.pp_print_string x.txt
  | `Pre x -> 
      Format.fprintf fmt "@[<1>(`Pre@ %a)@]" Format.pp_print_string x.txt
  | `Key x ->
      Format.fprintf fmt "@[<1>(`Key@ %a)@]" Format.pp_print_string x.txt
  | `Lid x ->
      Format.fprintf fmt "@[<1>(`Lid@ %a)@]" Format.pp_print_string x.txt
  | `Uid x ->
      Format.fprintf fmt "@[<1>(`Uid@ %a)@]" Format.pp_print_string x.txt
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
type 'a lex = Locf.t -> char Streamf.t -> 'a
type filter = stream -> stream
  
(* BOOTSTRAPPING --  *)
let to_string = Formatf.to_string pp_print_t
        
let print ppf x = Format.pp_print_string ppf (to_string x)
          
let strip (x:t) : Obj.t  =
  Obj.field (Obj.repr x) 1 
 
let get_tag (x:t ) : tag =
  (Obj.magic (Obj.field (Obj.repr x) 0 ) : tag)

let destruct (x:t) =
  ((Obj.magic (Obj.field (Obj.repr x) 0 ) : tag), Obj.field (Obj.repr x) 1 )
  
let get_string (x:t) :  string =
  match x with
  | `Pre x 
  | `Key x
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
  | `EOI x  -> x.txt
  | `Quot x -> x.txt
  | `DirQuotation x -> x.txt
  | `Ant x -> x.txt
  | `Inf x -> x.txt 

let get_loc (x:t) =
  match x with
  | `Key x
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
  | `EOI x  -> x.loc
  | `Ant x -> x.loc
  | `Quot x -> x.loc
  | `DirQuotation x -> x.loc
  | `Inf x -> x.loc
    



let empty_name : name =
  { domains = `Sub []; name = ""}

let name_of_string s : name =
  match s.[0] with
  | '.' ->
    (match List.rev
        @@ List.filter Stringf.not_empty (Stringf.nsplit s "." )  with
    | x::xs -> {domains = `Absolute (List.rev xs); name =  x}
    | _ -> assert false )
      
  |'A' .. 'Z' ->
      (match List.rev
          (List.filter Stringf.not_empty (Stringf.nsplit s ".")) with
      | x::xs ->
          {domains = `Sub (List.rev xs) ; name = x }
      | _ -> assert false)
  | _ -> {domains = `Sub []; name = s}

(** partial evaluation --
    pattern will be gottern first, while t is retrived later
    pervasive tests needed
 *)      
let match_token (x:pattern)  =
  let tag = x.descr.tag in
  let word = x.descr.word in
  fun (token_tag, obj)  ->
    (tag = token_tag  &&
     (match word with
     | Any -> true
     | Kind s -> (Obj.magic (Obj.field obj 3) : string) = s
     | A s -> (Obj.magic (Obj.field obj 1) : string) = s
     | Level i -> (Obj.magic (Obj.field obj 2) : int) = i))


(* local variables: *)
(* compile-command: "cd .. && pmake common/tokenf.cmo" *)
(* end: *)
