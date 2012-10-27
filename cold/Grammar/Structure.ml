open FanSig
type assoc = [ `NA | `RA | `LA] 
type position =
  [ `First | `Last | `Before of  string  | `After of  string 
  | `Level of  string ] 
module Action = struct
  type t =  Obj.t   let mk = (Obj.repr :('a ->  t )  )
  let get = (Obj.obj :( t  -> 'a)  )
  let getf = (Obj.obj :( t  -> ('a -> 'b) )  )
  let getf2 = (Obj.obj :( t  -> ('a -> ('b -> 'c) ) )  )
  end
type gram = 
  {
  gfilter:  filter ;
  gkeywords: ( string , int  ref ) Hashtbl.t ;
  glexer:
    ( FanLoc.t  -> ( char  Stream.t  -> ( token * FanLoc.t ) Stream.t ) ) ;
  warning_verbose:  bool  ref ;
  error_verbose:  bool  ref } 
type token_info = 
  {
  prev_loc:  FanLoc.t ;
  cur_loc:  FanLoc.t ;
  prev_loc_only:  bool } 
let ghost_token_info =
  {prev_loc = FanLoc.ghost;cur_loc = FanLoc.ghost;prev_loc_only = false }
type token_stream = ( token * token_info ) Stream.t  
type efun = ( token_stream  ->  Action.t )  
type description = [ `Normal | `Antiquot] 
type descr = ( description * string ) 
type token_pattern = (( token  ->  bool ) * descr ) 
type internal_entry = 
  {
  egram:  gram ;
  ename:  string ;
  mutable estart: ( int  ->  efun ) ;
  mutable econtinue: ( int  -> ( FanLoc.t  -> ( Action.t  ->  efun ) ) ) ;
  mutable edesc:  desc } 
and desc =  
  | Dlevels of  level  list 
  | Dparser of ( token_stream  ->  Action.t )  
and level = 
  {
  assoc:  assoc ;
  lname:  string  option ;
  lsuffix:  tree ;
  lprefix:  tree } 
and symbol =
  [ `Smeta of ( string * symbol  list * Action.t )
  | `Snterm of  internal_entry  | `Snterml of ( internal_entry * string )
  | `Slist0 of  symbol  | `Slist0sep of ( symbol * symbol )
  | `Slist1 of  symbol  | `Slist1sep of ( symbol * symbol )
  | `Sopt of  symbol  | `Stry of  symbol  | `Sself | `Snext
  | `Stoken of  token_pattern  | `Skeyword of  string  | `Stree of  tree ] 
and tree =  
  | Node of  node 
  | LocAct of  Action.t * Action.t  list 
  | DeadEnd 
and node =  {
  node:  symbol ;
  son:  tree ;
  brother:  tree } 
type production_rule = ( symbol  list * Action.t ) 
type single_extend_statment =
  ( string  option * assoc  option * production_rule  list ) 
type extend_statment = ( position  option * single_extend_statment  list ) 
type delete_statment =  symbol  list  
type ('a,'b,'c) fold =
  ( internal_entry  ->
    ( symbol  list  -> (('a Stream.t  -> 'b)  -> ('a Stream.t  -> 'c) ) ) ) 
  
type ('a,'b,'c) foldsep =
  ( internal_entry  ->
    ( symbol  list  ->
      (('a Stream.t  -> 'b)  ->
        (('a Stream.t  ->  unit )  -> ('a Stream.t  -> 'c) ) ) ) ) 
  
let get_filter (g) = g.gfilter
let token_location (r) = r.cur_loc
let using ({gkeywords = table;gfilter = filter;_}) (kwd) =
  let r = begin try (Hashtbl.find table kwd)
    with
    | Not_found  ->   let r = (ref 0) in begin
                        (Hashtbl.add table kwd r);
                        r
                        end
  end in
  begin
    (FanToken.Filter.keyword_added filter kwd ( (( r.contents ) = 0) ));
    (incr r)
    end
let mk_action = Action.mk
let string_of_token = FanToken.extract_string
let removing ({gkeywords = table;gfilter = filter;_}) (kwd) =
  let r = (Hashtbl.find table kwd) in
  let ()  = (decr r) in
  if (( r.contents ) = 0) then begin
    begin
    (FanToken.Filter.keyword_removed filter kwd);
    (Hashtbl.remove table kwd)
    end
  end else begin
    ()
  end