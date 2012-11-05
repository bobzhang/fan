open FanSig
open LibUtil
type assoc = [ `NA | `RA | `LA] 
type position =
  [ `First | `Last | `Before of string | `After of string | `Level of string] 
module Action = struct
  type t = Obj.t  let mk: 'a -> t = Obj.repr let get: t -> 'a = Obj.obj
  let getf: t -> 'a -> 'b = Obj.obj
  let getf2: t -> 'a -> 'b -> 'c = Obj.obj
  end
type gram = 
  {
  gfilter: filter;
  gkeywords: (string,int ref) Hashtbl.t;
  glexer: FanLoc.t -> char Stream.t -> (token* FanLoc.t) Stream.t;
  warning_verbose: bool ref;
  error_verbose: bool ref} 
type token_info =  {
  prev_loc: FanLoc.t;
  cur_loc: FanLoc.t;
  prev_loc_only: bool} 
let ghost_token_info =
  { prev_loc = FanLoc.ghost; cur_loc = FanLoc.ghost; prev_loc_only = false }
type token_stream = (token* token_info) Stream.t 
type efun = token_stream -> Action.t 
type description = [ `Normal | `Antiquot] 
type descr = (description* string) 
type token_pattern = ((token -> bool)* descr) 
type internal_entry = 
  {
  egram: gram;
  ename: string;
  mutable estart: int -> efun;
  mutable econtinue: int -> FanLoc.t -> Action.t -> efun;
  mutable edesc: desc} 
and desc =  
  | Dlevels of level list
  | Dparser of (token_stream -> Action.t) 
and level =  {
  assoc: assoc;
  lname: string option;
  lsuffix: tree;
  lprefix: tree} 
and symbol =
  [ `Smeta of (string list* symbol list* Action.t)
  | `Snterm of internal_entry | `Snterml of (internal_entry* string)
  | `Slist0 of symbol | `Slist0sep of (symbol* symbol) | `Slist1 of symbol
  | `Slist1sep of (symbol* symbol) | `Sopt of symbol | `Stry of symbol
  | `Sself | `Snext | `Stoken of token_pattern | `Skeyword of string
  | `Stree of tree] 
and tree =  
  | Node of node
  | LocAct of Action.t* Action.t list
  | DeadEnd 
and node =  {
  node: symbol;
  son: tree;
  brother: tree} 
type production_rule = (symbol list* Action.t) 
type single_extend_statment =
  (string option* assoc option* production_rule list) 
type extend_statment = (position option* single_extend_statment list) 
type delete_statment = symbol list 
type ('a,'b,'c) fold =
  internal_entry -> symbol list -> ('a Stream.t -> 'b) -> 'a Stream.t -> 'c 
type ('a,'b,'c) foldsep =
  internal_entry ->
    symbol list ->
      ('a Stream.t -> 'b) -> ('a Stream.t -> unit) -> 'a Stream.t -> 'c
  
let get_filter g = g.gfilter
let token_location r = r.cur_loc
let using { gkeywords = table; gfilter = filter;_} kwd =
  let r =
    try Hashtbl.find table kwd
    with | Not_found  -> let r = ref 0 in (Hashtbl.add table kwd r; r) in
  FanToken.Filter.keyword_added filter kwd (r.contents = 0); incr r
let mk_action = Action.mk
let string_of_token = FanToken.extract_string
let removing { gkeywords = table; gfilter = filter;_} kwd =
  let r = Hashtbl.find table kwd in
  let () = decr r in
  if r.contents = 0
  then (FanToken.Filter.keyword_removed filter kwd; Hashtbl.remove table kwd)
  else ()
let rec flatten_tree =
  function
  | DeadEnd  -> []
  | LocAct (_,_) -> [[]]
  | Node { node = n; brother = b; son = s } ->
      (List.map (fun l  -> n :: l) (flatten_tree s)) @ (flatten_tree b)