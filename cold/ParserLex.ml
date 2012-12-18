module Ast = Camlp4Ast
open PreCast.Syntax
let regexp = Gram.mk "regexp"
let chr = Gram.mk "chr"
let ch_class = Gram.mk "ch_class"
let apply () =
  Gram.extend (expr : 'expr Gram.t )
    ((Some (`Level "top")),
      [(None, None,
         [([`Skeyword "lexer";
           `Skeyword "[";
           `Slist0sep
             ((Gram.srules expr
                 [([`Snterm (Gram.obj (regexp : 'regexp Gram.t ));
                   `Skeyword "->";
                   `Snterm (Gram.obj (expr : 'expr Gram.t ))],
                    (Gram.mk_action
                       (fun (a : 'expr)  _  (r : 'regexp)  (_loc : FanLoc.t) 
                          -> ((r, a) : 'e__1 ))))]), (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (l : 'e__1 list)  _  _  (_loc : FanLoc.t)  ->
                  (LexGen.gen_definition _loc l : 'expr ))))])]);
  Gram.extend (str_item : 'str_item Gram.t )
    ((Some (`Level "top")),
      [(None, None,
         [([`Skeyword "let";
           `Stoken
             (((function | `LID "regexp" -> true | _ -> false)),
               (`Normal, "`LID \"regexp\""));
           `Stoken
             (((function | `LID _ -> true | _ -> false)),
               (`Normal, "`LID _"));
           `Skeyword "=";
           `Snterm (Gram.obj (regexp : 'regexp Gram.t ))],
            (Gram.mk_action
               (fun (r : 'regexp)  _  (__fan_2 : [> FanToken.t]) 
                  (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match (__fan_2, __fan_1) with
                  | (`LID x,`LID "regexp") ->
                      ((if Hashtbl.mem Ulex.named_regexps x
                        then
                          Printf.eprintf
                            "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                            x
                        else ();
                        Hashtbl.add Ulex.named_regexps x r;
                        Ast.StNil _loc) : 'str_item )
                  | _ -> assert false)))])]);
  Gram.extend (regexp : 'regexp Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "|"; `Sself],
            (Gram.mk_action
               (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (Ulex.alt r1 r2 : 'regexp ))))]);
      (None, None,
        [([`Sself; `Sself],
           (Gram.mk_action
              (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                 (Ulex.seq r1 r2 : 'regexp ))))]);
      (None, None,
        [([`Sself; `Skeyword "*"],
           (Gram.mk_action
              (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                 (Ulex.rep r1 : 'regexp ))));
        ([`Sself; `Skeyword "+"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                (Ulex.plus r1 : 'regexp ))));
        ([`Sself; `Skeyword "?"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                (Ulex.alt Ulex.eps r1 : 'regexp ))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  -> (r1 : 'regexp ))));
        ([`Skeyword "_"],
          (Gram.mk_action
             (fun _  (_loc : FanLoc.t)  -> (Ulex.chars Cset.any : 'regexp ))));
        ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
          (Gram.mk_action
             (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                (Ulex.chars (Cset.singleton c) : 'regexp ))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `STR (s,_) -> (LexGen.regexp_for_string s : 'regexp )
                | _ -> assert false)));
        ([`Skeyword "[";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          (Gram.mk_action
             (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                (Ulex.chars cc : 'regexp ))));
        ([`Skeyword "[^";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          (Gram.mk_action
             (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                (Ulex.chars (Cset.difference Cset.any cc) : 'regexp ))));
        ([`Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `LID x ->
                    ((try Hashtbl.find Ulex.named_regexps x
                      with
                      | Not_found  ->
                          failwith
                            ("pa_ulex (error): reference to unbound regexp name `"
                               ^ (x ^ "'"))) : 'regexp )
                | _ -> assert false)))])]);
  Gram.extend (chr : 'chr Gram.t )
    (None,
      [(None, None,
         [([`Stoken
              (((function | `CHAR (_,_) -> true | _ -> false)),
                (`Normal, "`CHAR (_,_)"))],
            (Gram.mk_action
               (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                  match __fan_0 with
                  | `CHAR (c,_) -> (Char.code c : 'chr )
                  | _ -> assert false)));
         ([`Stoken
             (((function | `INT (_,_) -> true | _ -> false)),
               (`Normal, "`INT (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `INT (_,i) -> (LexGen.char_int i : 'chr )
                 | _ -> assert false)))])]);
  Gram.extend (ch_class : 'ch_class Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (chr : 'chr Gram.t ));
           `Skeyword "-";
           `Snterm (Gram.obj (chr : 'chr Gram.t ))],
            (Gram.mk_action
               (fun (c2 : 'chr)  _  (c1 : 'chr)  (_loc : FanLoc.t)  ->
                  (Cset.interval c1 c2 : 'ch_class ))));
         ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
           (Gram.mk_action
              (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                 (Cset.singleton c : 'ch_class ))));
         ([`Sself; `Sself],
           (Gram.mk_action
              (fun (cc2 : 'ch_class)  (cc1 : 'ch_class)  (_loc : FanLoc.t) 
                 -> (Cset.union cc1 cc2 : 'ch_class ))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (s,_) ->
                     (let c = ref Cset.empty in
                      (for i = 0 to (String.length s) - 1 do
                         c :=
                           (Cset.union c.contents
                              (Cset.singleton (Char.code (s.[i]))))
                       done;
                       c.contents) : 'ch_class )
                 | _ -> assert false)))])])
let _ = AstParsers.register_parser ("lexer", apply)
let change_ids suffix =
  object 
    inherit  Camlp4Ast.map
    method! ident =
      function
      | Ast.IdLid (loc,s) when
          ((String.length s) > 6) && ((String.sub s 0 6) = "__ulex") ->
          Ast.IdLid (loc, (s ^ suffix))
      | i -> i
  end
let () =
  let first = ref true in
  let _loc = FanLoc.ghost in
  AstFilters.register_str_item_filter
    ("ulex",
      (fun s  ->
         assert (first.contents);
         first := false;
         (let parts = List.map LexGen.partition (Ulex.partitions ()) in
          let tables = List.map LexGen.table (LexGen.get_tables ()) in
          let suffix =
            "__" ^
              (Digest.to_hex
                 (Digest.string (Marshal.to_string (parts, tables) []))) in
          (change_ids suffix)#str_item
            (Ast.StSem
               (_loc, (Ast.stSem_of_list tables),
                 (Ast.StSem (_loc, (Ast.stSem_of_list parts), s)))))))