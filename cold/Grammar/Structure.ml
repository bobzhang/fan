open LibUtil
open FanToken
type assoc = [ `NA | `RA | `LA] 
type position =
  [ `First | `Last | `Before of string | `After of string | `Level of string] 
module Action =
  struct
    type t = Obj.t 
    let mk: 'a -> t = Obj.repr
    let get: t -> 'a = Obj.obj
    let getf: t -> 'a -> 'b = Obj.obj
    let getf2: t -> 'a -> 'b -> 'c = Obj.obj
  end
type 'a cont_parse = FanLoc.t -> Action.t -> 'a parse 
type description = [ `Normal | `Antiquot] 
type descr = (description* string) 
type token_pattern = ((FanToken.t -> bool)* descr) 
type terminal = [ `Skeyword of string | `Stoken of token_pattern] 
type gram = 
  {
  gfilter: FanTokenFilter.t;
  gkeywords: (string,int ref) Hashtbl.t;
  glexer: FanLoc.t -> char XStream.t -> stream} 
type internal_entry = 
  {
  egram: gram;
  ename: string;
  mutable estart: int -> Action.t parse;
  mutable econtinue: int -> Action.t cont_parse;
  mutable edesc: desc;
  mutable freezed: bool} 
and desc =  
  | Dlevels of level list
  | Dparser of (stream -> Action.t) 
and level =  {
  assoc: assoc;
  lname: string;
  lsuffix: tree;
  lprefix: tree} 
and symbol =
  [ `Smeta of (string list* symbol list* Action.t)
  | `Snterm of internal_entry | `Snterml of (internal_entry* string)
  | `Slist0 of symbol | `Slist0sep of (symbol* symbol) | `Slist1 of symbol
  | `Slist1sep of (symbol* symbol) | `Sopt of symbol | `Stry of symbol
  | `Speek of symbol | `Sself | `Snext | `Stree of tree | terminal] 
and tree =  
  | Node of node
  | LocAct of Action.t* Action.t list
  | DeadEnd 
and node =  {
  node: symbol;
  son: tree;
  brother: tree} 
type production = (symbol list* Action.t) 
type olevel = (string* assoc* production list) 
type extend_statment = (position option* olevel list) 
type delete_statment = symbol list 
type ('a,'b,'c) fold =
  internal_entry -> symbol list -> ('a XStream.t -> 'b) -> 'a XStream.t -> 'c 
type ('a,'b,'c) foldsep =
  internal_entry ->
    symbol list ->
      ('a XStream.t -> 'b) -> ('a XStream.t -> unit) -> 'a XStream.t -> 'c
  
let get_filter g = g.gfilter
let gram_of_entry { egram;_} = egram
let using { gkeywords = table; gfilter = filter;_} kwd =
  let r =
    try Hashtbl.find table kwd
    with | Not_found  -> let r = ref 0 in (Hashtbl.add table kwd r; r) in
  FanTokenFilter.keyword_added filter kwd (r.contents = 0); incr r
let mk_action = Action.mk
let string_of_token = FanToken.extract_string
let removing { gkeywords = table; gfilter = filter;_} kwd =
  let r = Hashtbl.find table kwd in
  let () = decr r in
  if r.contents = 0
  then (FanTokenFilter.keyword_removed filter kwd; Hashtbl.remove table kwd)
  else ()
let rec flatten_tree =
  function
  | DeadEnd  -> []
  | LocAct (_,_) -> [[]]
  | Node { node = n; brother = b; son = s } ->
      (List.map (fun l  -> n :: l) (flatten_tree s)) @ (flatten_tree b)
type brothers =  
  | Bro of symbol* brothers list 
type space_formatter = (unit,Format.formatter,unit) format 
let get_brothers x =
  let rec aux acc =
    function
    | DeadEnd |LocAct _ -> List.rev acc
    | Node { node = n; brother = b; son = s } ->
        aux ((Bro (n, (aux [] s))) :: acc) b in
  aux [] x
let get_children x =
  let rec aux acc =
    function
    | [] -> List.rev acc
    | (Bro (n,x))::[] -> aux (n :: acc) x
    | _ -> raise Exit in
  aux [] x
let get_first =
  let rec aux acc =
    function
    | Node { node; brother;_} -> aux (node :: acc) brother
    | LocAct (_,_)|DeadEnd  -> acc in
  aux []
let get_first_from levels set =
  List.iter
    (fun level  -> (level.lprefix |> get_first) |> (Hashset.add_list set))
    levels