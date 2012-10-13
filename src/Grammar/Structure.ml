open FanSig;

type assoc =
    [= `NA|`RA|`LA];
type position =
    [= `First | `Last | `Before of string | `After of string | `Level of string];


module Action  = struct
  type  t     = Obj.t   ;
  let mk :'a -> t   = Obj.repr;
  let get: t -> 'a  = Obj.obj ;
  let getf: t-> 'a -> 'b  = Obj.obj ;
  let getf2: t -> 'a -> 'b -> 'c = Obj.obj ;
end;


type gram = {
    gfilter         : filter;
    gkeywords       : Hashtbl.t string (ref int);
    glexer          : FanLoc.t -> Stream.t char -> Stream.t (token * FanLoc.t);
    warning_verbose : ref bool;
    error_verbose   : ref bool };

type token_info = {
    prev_loc : FanLoc.t;
    cur_loc : FanLoc.t ;
    prev_loc_only : bool };
  
let ghost_token_info = {
  prev_loc=FanLoc.ghost;
  cur_loc = FanLoc.ghost;
  prev_loc_only = False;};
  (* with neighbor token info stored*)  

type token_stream = Stream.t (token * token_info);

type efun = token_stream -> Action.t;
  
type description =
    [= `Normal
    | `Antiquot];

type descr = (description * string) ;  
type token_pattern = ((token -> bool) * descr);

type internal_entry =
    { egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> FanLoc.t -> Action.t -> efun;
      edesc     : mutable desc }
and desc =
    [ Dlevels of list level
    | Dparser of token_stream -> Action.t ]
and level = {
    assoc   : assoc         ;
    lname   : option string ;
    lsuffix : tree          ;
    lprefix : tree          }
and symbol =
    [= `Smeta of (string * list symbol * Action.t)
    | `Snterm of internal_entry
    | `Snterml of (internal_entry * string)
    | `Slist0 of symbol
    | `Slist0sep of (symbol * symbol)
    | `Slist1 of symbol
    | `Slist1sep of (symbol * symbol)
    | `Sopt of symbol
    | `Stry of symbol
    | `Sself
    | `Snext
    | `Stoken of token_pattern
    | `Skeyword of string
    | `Stree of tree ]
and tree = (* internal struccture *)
    [ Node of node
    | LocAct of Action.t and list Action.t
    | DeadEnd ]
and node = {
    node    : symbol ;
    son     : tree   ;
    brother : tree   };

type production_rule = (list symbol * Action.t);
type single_extend_statment =
  (option string * option assoc * list production_rule);
type extend_statment =
  (option position * list single_extend_statment);
type delete_statment = list symbol;

type fold 'a 'b 'c =
    internal_entry -> list symbol ->
      (Stream.t 'a -> 'b) -> Stream.t 'a -> 'c;

type foldsep 'a 'b 'c =
    internal_entry -> list symbol ->
      (Stream.t 'a -> 'b) -> (Stream.t 'a -> unit) -> Stream.t 'a -> 'c;

let get_filter g = g.gfilter;
let token_location r = r.cur_loc;

  
let using { gkeywords = table; gfilter = filter; _ } kwd =
  let r = try Hashtbl.find table kwd with
    [ Not_found ->
      let r = ref 0 in do { Hashtbl.add table kwd r; r } ]
  in begin
    FanToken.Filter.keyword_added filter kwd (r.contents = 0);
    incr r
  end;
let mk_action=Action.mk;
let string_of_token=FanToken.extract_string  ;
let removing { gkeywords = table; gfilter = filter; _ } kwd =
  let r = Hashtbl.find table kwd in
  let () = decr r in
    if !r = 0 then begin
      FanToken.Filter.keyword_removed filter kwd;
      Hashtbl.remove table kwd
    end else ();


