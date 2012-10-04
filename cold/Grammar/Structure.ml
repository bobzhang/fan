open FanSig.Grammar

module type S =
                      sig
                       module Token : FanSig.Token

                       module Lexer :
                        (FanSig.Lexer with module Token = Token)

                       module Action : FanSig.Grammar.Action

                       type gram = {
                                     gfilter:Token.Filter.t;
                                     gkeywords:(string, int ref) Hashtbl.t;
                                     glexer:(FanLoc.t ->
                                             (char Stream.t ->
                                              (Token.t * FanLoc.t) Stream.t));
                                     warning_verbose:bool ref;
                                     error_verbose:bool ref}

                       type token_info = {
                                           prev_loc:FanLoc.t;
                                           cur_loc:FanLoc.t;
                                           prev_loc_only:bool}

 type token_stream = (Token.t * token_info) Stream.t

 type efun = (token_stream -> Action.t)

 type token_pattern = ((Token.t -> bool) * string)

 type internal_entry = {
                         egram:gram;
                         ename:string;
                         mutable estart:(int -> efun);
                         mutable econtinue:(int ->
                                            (FanLoc.t -> (Action.t -> efun)));
                         mutable edesc:desc}
and desc = Dlevels of level list | Dparser of (token_stream -> Action.t)
and level = {assoc:assoc; lname:string option; lsuffix:tree; lprefix:tree}
and symbol =
    Smeta of string * symbol list * Action.t
  | Snterm of internal_entry
  | Snterml of internal_entry * string
  | Slist0 of symbol
  | Slist0sep of symbol * symbol
  | Slist1 of symbol
  | Slist1sep of symbol * symbol
  | Sopt of symbol
  | Stry of symbol
  | Sself
  | Snext
  | Stoken of token_pattern
  | Skeyword of string
  | Stree of tree
and tree = Node of node | LocAct of Action.t * Action.t list | DeadEnd
and node = {node:symbol; son:tree; brother:tree}

 type production_rule = (symbol list * Action.t)

 type single_extend_statment =
  (string option * assoc option * production_rule list)

 type extend_statment =
  (position option * single_extend_statment list)

 type delete_statment = symbol list

 type ('a, 'b, 'c) fold =
  (internal_entry ->
   (symbol list -> (('a Stream.t -> 'b) -> ('a Stream.t -> 'c))))

 type ('a, 'b, 'c) foldsep =
  (internal_entry ->
   (symbol list ->
    (('a Stream.t -> 'b) ->
     (('a Stream.t -> unit) -> ('a Stream.t -> 'c)))))

 val get_filter : (gram -> Token.Filter.t)

 val using : (gram -> (string -> unit))

 val removing : (gram -> (string -> unit))

end

module Make =
      functor (Lexer : FanSig.Lexer) ->
       struct
        module Token = Lexer.Token

        module Action : FanSig.Grammar.Action =
         struct
          type t = Obj.t

          let mk = Obj.repr

          let get = Obj.obj

          let getf = Obj.obj

          let getf2 = Obj.obj

         end

        module Lexer = Lexer

        type gram = {
                      gfilter:Token.Filter.t;
                      gkeywords:(string, int ref) Hashtbl.t;
                      glexer:(FanLoc.t ->
                              (char Stream.t ->
                               (Token.t * FanLoc.t) Stream.t));
                      warning_verbose:bool ref;
                      error_verbose:bool ref}

        type token_info = {
                            prev_loc:FanLoc.t;
                            cur_loc:FanLoc.t;
                            prev_loc_only:bool}

       type token_stream = (Token.t * token_info) Stream.t

       type efun = (token_stream -> Action.t)

       type token_pattern = ((Token.t -> bool) * string)

       type internal_entry = {
                               egram:gram;
                               ename:string;
                               mutable estart:(int -> efun);
                               mutable econtinue:(int ->
                                                  (FanLoc.t ->
                                                   (Action.t -> efun)));
                               mutable edesc:desc}
      and desc =
          Dlevels of level list
        | Dparser of (token_stream -> Action.t)
      and level = {
                    assoc:assoc;
                    lname:string option;
                    lsuffix:tree;
                    lprefix:tree}
and symbol =
    Smeta of string * symbol list * Action.t
  | Snterm of internal_entry
  | Snterml of internal_entry * string
  | Slist0 of symbol
  | Slist0sep of symbol * symbol
  | Slist1 of symbol
  | Slist1sep of symbol * symbol
  | Sopt of symbol
  | Stry of symbol
  | Sself
  | Snext
  | Stoken of token_pattern
  | Skeyword of string
  | Stree of tree
and tree =
    Node of node | LocAct of Action.t * Action.t list | DeadEnd
and node = {node:symbol; son:tree; brother:tree}

 type production_rule = (symbol list * Action.t)

 type single_extend_statment =
  (string option * assoc option * production_rule list)

 type extend_statment =
  (position option * single_extend_statment list)

 type delete_statment = symbol list

 type ('a, 'b, 'c) fold =
  (internal_entry ->
   (symbol list -> (('a Stream.t -> 'b) -> ('a Stream.t -> 'c))))

 type ('a, 'b, 'c) foldsep =
  (internal_entry ->
   (symbol list ->
    (('a Stream.t -> 'b) ->
     (('a Stream.t -> unit) -> ('a Stream.t -> 'c)))))

 let get_filter = fun g -> g.gfilter

 let token_location = fun r -> r.cur_loc

 let using =
  fun {gkeywords = table;
   gfilter = filter; _ } ->
   fun kwd ->
    let r =
     (try (Hashtbl.find table kwd) with
      Not_found ->
       let r = (ref 0) in ( (Hashtbl.add table kwd r) ); r) in
    (
    (Token.Filter.keyword_added filter kwd ( (( r.contents ) = 0) ))
    );
    (incr r)

 let removing =
  fun {gkeywords = table;
   gfilter = filter; _ } ->
   fun kwd ->
    let r = (Hashtbl.find table kwd) in
    let () = (decr r) in
    if (( r.contents ) = 0)
    then
     begin
     (
     (Token.Filter.keyword_removed filter kwd)
     );
     (Hashtbl.remove table kwd)
    end else ()

end
