(* open FanSig; *)

(* module type S = sig *)

(*   module Token        : FanSig.Token;  *)
(*   module Lexer        : Sig.Lexer  with  module Token = Token; *)
(*   module Action       : FanSig.Grammar.Action; *)

(*   type gram = *)
(*     { gfilter         : Token.Filter.t; *)
(*       gkeywords       : Hashtbl.t string (ref int); *)
(*       glexer          : FanLoc.t -> Stream.t char -> Stream.t (Token.t * FanLoc.t); *)
(*       warning_verbose : ref bool; *)
(*       error_verbose   : ref bool }; *)

(*   type token_info = { *)
(*       prev_loc : FanLoc.t; *)
(*       cur_loc : FanLoc.t; *)
(*       prev_loc_only : bool *)
(*     }; *)

(*   type token_stream = Stream.t (Token.t * token_info); *)

(*   type efun = token_stream -> Action.t; *)

(*   (\* maybe could be improved *)
(*      {[ *)
(*      The second part is a description *)
(*      ]} *)
(*    *\) *)
(*   type token_pattern = ((Token.t -> bool) * string);  *)

(*   type internal_entry = *)
(*     { egram     : gram; *)
(*       ename     : string; *)
(*       estart    : mutable int -> efun; *)
(*       econtinue : mutable int -> FanLoc.t -> Action.t -> efun; *)
(*       edesc     : mutable desc } *)
(*   and desc = *)
(*     [ Dlevels of list level *)
(*     | Dparser of token_stream -> Action.t ] *)
(*   and level = *)
(*     { assoc   : FanSig.assoc         ; *)
(*       lname   : option string ; *)
(*       lsuffix : tree          ; *)
(*       lprefix : tree          } *)
(*   and symbol = *)
(*     [ Smeta of string and list symbol and Action.t *)
(*     | Snterm of internal_entry *)
(*     | Snterml of internal_entry and string *)
(*     | Slist0 of symbol *)
(*     | Slist0sep of symbol and symbol *)
(*     | Slist1 of symbol *)
(*     | Slist1sep of symbol and symbol *)
(*     | Sopt of symbol *)
(*     | Stry of symbol *)
(*     | Sself *)
(*     | Snext *)
(*     | Stoken of token_pattern *)
(*     | Skeyword of string *)
(*     | Stree of tree ] *)
(*   and tree = *)
(*     [ Node of node *)
(*     | LocAct of Action.t and list Action.t *)
(*     | DeadEnd ] *)
(*   and node = *)
(*     { node    : symbol ; *)
(*       son     : tree   ; *)
(*       brother : tree   }; *)

(*   type production_rule = (list symbol * Action.t); *)
(*   type single_extend_statment = *)
(*     (option string * option FanSig.assoc * list production_rule); *)
(*   type extend_statment = *)
(*     (option FanSig.position * list single_extend_statment); *)
(*   type delete_statment = list symbol; *)
(*   type token = Token.t; *)
(*   type fold 'a 'b 'c = *)
(*     internal_entry -> list symbol -> *)
(*       (Stream.t 'a -> 'b) -> Stream.t 'a -> 'c; *)

(*   type foldsep 'a 'b 'c = *)
(*     internal_entry -> list symbol -> *)
(*       (Stream.t 'a -> 'b) -> (Stream.t 'a -> unit) -> Stream.t 'a -> 'c; *)

(*   (\* Accessors *\) *)
(*   val get_filter : gram -> Token.Filter.t; *)

(*   (\* Useful functions *\) *)
(*   val using: gram -> string -> unit; *)
(*   val removing: gram -> string -> unit; *)
(*   val mk_action: 'a -> Action.t; *)
(*   val string_of_token:Token.t -> string; *)
(* end; *)

type assoc =
    [= `NA|`RA|`LA];
type position =
    [= `First | `Last | `Before of string | `After of string | `Level of string];

(* module Make (Lexer  : Sig.Lexer) = struct *)
(*   module Token = Lexer.Token; *)
  (* type token=Token.t; *)
  module Action  (*: FanSig.Grammar.Action *) = struct
    type  t     = Obj.t   ;
    let mk :'a -> t   = Obj.repr;
    let get: t -> 'a  = Obj.obj ;
    let getf: t-> 'a -> 'b  = Obj.obj ;
    let getf2: t -> 'a -> 'b -> 'c = Obj.obj ;
  end;
  (* module Lexer = Lexer; *)

  type gram =
    { gfilter         : FanToken.Filter.t;
      gkeywords       : Hashtbl.t string (ref int);
      glexer          : FanLoc.t -> Stream.t char -> Stream.t (FanToken.t * FanLoc.t);
      warning_verbose : ref bool;
      error_verbose   : ref bool };

  type token_info = {
      prev_loc : FanLoc.t;
      cur_loc : FanLoc.t ;
      prev_loc_only : bool
    };
  let ghost_token_info = {
    prev_loc=FanLoc.ghost;
    cur_loc = FanLoc.ghost;
    prev_loc_only = False;
  };  
  type token_stream = Stream.t (FanToken.t * token_info);

  type efun = token_stream -> Action.t;

  type token_pattern = ((FanToken.t -> bool) * string);

  type internal_entry =
    { egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> FanLoc.t -> Action.t -> efun;
      edesc     : mutable desc }
  and desc =
    [ Dlevels of list level
    | Dparser of token_stream -> Action.t ]
  and level =
    { assoc   : assoc         ;
      lname   : option string ;
      lsuffix : tree          ;
      lprefix : tree          }
  and symbol =
    [ Smeta of string and list symbol and Action.t
    | Snterm of internal_entry
    | Snterml of internal_entry and string
    | Slist0 of symbol
    | Slist0sep of symbol and symbol
    | Slist1 of symbol
    | Slist1sep of symbol and symbol
    | Sopt of symbol
    | Stry of symbol
    | Sself
    | Snext
    | Stoken of token_pattern
    | Skeyword of string
    | Stree of tree ]
  and tree =
    [ Node of node
    | LocAct of Action.t and list Action.t
    | DeadEnd ]
  and node =
    { node    : symbol ;
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
    in do { FanToken.Filter.keyword_added filter kwd (r.contents = 0);
            incr r };
  let mk_action=Action.mk;
  let string_of_token=FanToken.extract_string  ;
  let removing { gkeywords = table; gfilter = filter; _ } kwd =
    let r = Hashtbl.find table kwd in
    let () = decr r in
    if !r = 0 then do {
      FanToken.Filter.keyword_removed filter kwd;
      Hashtbl.remove table kwd
    } else ();
(* end; *)

