(****************************************************************************)
(*                                                                          *)
(*                                   OCaml                                  *)
(*                                                                          *)
(*                            INRIA Rocquencourt                            *)
(*                                                                          *)
(*  Copyright  2006   Institut National de Recherche  en  Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed under   *)
(*  the terms of the GNU Library General Public License, with the special   *)
(*  exception on linking described in LICENSE at the top of the OCaml       *)
(*  source tree.                                                            *)
(*                                                                          *)
(****************************************************************************)

(* Authors:
 * - Daniel de Rauglaudre: initial version
 * - Nicolas Pouillard: refactoring
 *)


module type S = sig
  type assoc =
  [ NonA
  | RightA
  | LeftA ];

  type position =
  [ First
  | Last
  | Before of string
  | After of string
  | Level of string ];
  
  module Loc          : FanSig.Loc;
  module Token        : FanSig.Token with module Loc = Loc;
  module Lexer        : FanSig.Lexer
                        with module Loc   = Loc
                         and module Token = Token;
  module Action       : sig
    type t;
    value mk    : 'a ->  t;
    value get   :  t -> 'a;
    value getf  :  t -> ('a -> 'b);
    value getf2 :  t -> ('a -> 'b -> 'c);
  end;

  type gram =
    { gfilter         : Token.Filter.t;
      gkeywords       : Hashtbl.t string (ref int);
      glexer          : Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t);
      warning_verbose : ref bool;
      error_verbose   : ref bool };

  type token_info = { prev_loc : Loc.t
                    ; cur_loc : Loc.t
                    ; prev_loc_only : bool
                    };

  type token_stream = Stream.t (Token.t * token_info);

  type efun = token_stream -> Action.t;

  type token_pattern = ((Token.t -> bool) * string);

  type internal_entry =
    { egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> Loc.t -> Action.t -> efun;
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

  (* Accessors *)
  value get_filter : gram -> Token.Filter.t;

  (* Useful functions *)
  value using : gram -> string -> unit;
  value removing : gram -> string -> unit;
end;

  
module Make (Lexer  : FanSig.Lexer) = struct

  type assoc =
    [ NonA
    | RightA
    | LeftA ];

  type position =
    [ First
    | Last
    | Before of string
    | After of string
    | Level of string ];

  module Loc = Lexer.Loc;
  module Token = Lexer.Token;
  module Action  = struct
    type  t     = Obj.t   ;
    value mk    = Obj.repr;
    value get   = Obj.obj ;
    value getf  = Obj.obj ;
    value getf2 = Obj.obj ;
  end;
  module Lexer = Lexer;

  type gram = {
      gfilter         : Token.Filter.t;
      gkeywords       : Hashtbl.t string (ref int);
      glexer          : Loc.t -> Stream.t char -> Stream.t (Token.t * Loc.t);
      warning_verbose : ref bool;
      error_verbose   : ref bool };

  type token_info = {
      prev_loc : Loc.t;
      cur_loc : Loc.t ;
      prev_loc_only : bool;
    };

  type token_stream =
      Stream.t (Token.t * token_info);

  type efun =
      token_stream -> Action.t;

  type token_pattern =
      ((Token.t -> bool) * string);

  type internal_entry = {
      egram     : gram;
      ename     : string;
      estart    : mutable int -> efun;
      econtinue : mutable int -> Loc.t -> Action.t -> efun;
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

  value get_filter g = g.gfilter;
  value token_location r = r.cur_loc;

  type not_filtered 'a = 'a;
  value using { gkeywords = table; gfilter = filter } kwd =
    let r = try Hashtbl.find table kwd with
            [ Not_found ->
                let r = ref 0 in do { Hashtbl.add table kwd r; r } ]
    in do { Token.Filter.keyword_added filter kwd (r.val = 0);
            incr r };

  value removing { gkeywords = table; gfilter = filter } kwd =
    let r = Hashtbl.find table kwd in
    let () = decr r in
    if r.val = 0 then do {
      Token.Filter.keyword_removed filter kwd;
      Hashtbl.remove table kwd
    } else ();
end;


