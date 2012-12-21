module Ast = Camlp4Ast
open LibUtil
open PreCast.Syntax
let regexp = Gram.mk "regexp"
let chr = Gram.mk "chr"
let ch_class = Gram.mk "ch_class"
let regexps = Gram.mk "regexps"
let lex = Gram.mk "lex"
let declare_regexp = Gram.mk "declare_regexp"
let _ =
  Gram.extend (lex : 'lex Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "[";
           `Slist0sep
             ((Gram.srules lex
                 [([`Snterm (Gram.obj (regexp : 'regexp Gram.t ));
                   `Skeyword "->";
                   `Snterm (Gram.obj (expr : 'expr Gram.t ))],
                    (Gram.mk_action
                       (fun (a : 'expr)  _  (r : 'regexp)  (_loc : FanLoc.t) 
                          -> ((r, a) : 'e__1 ))))]), (`Skeyword "|"));
           `Skeyword "]"],
            (Gram.mk_action
               (fun _  (l : 'e__1 list)  _  (_loc : FanLoc.t)  ->
                  (FanLexTools.gen_definition _loc l : 'lex ))))])]);
  Gram.extend (declare_regexp : 'declare_regexp Gram.t )
    (None,
      [(None, None,
         [([`Slist1sep
              ((Gram.srules declare_regexp
                  [([`Stoken
                       (((function | `LID _ -> true | _ -> false)),
                         (`Normal, "`LID _"));
                    `Skeyword ":";
                    `Snterm (Gram.obj (regexp : 'regexp Gram.t ))],
                     (Gram.mk_action
                        (fun (r : 'regexp)  _  (__fan_0 : [> FanToken.t]) 
                           (_loc : FanLoc.t)  ->
                           match __fan_0 with
                           | `LID x -> ((x, r) : 'e__2 )
                           | _ -> assert false)))]), (`Skeyword ";"))],
            (Gram.mk_action
               (fun (xrs : 'e__2 list)  (_loc : FanLoc.t)  ->
                  (List.iter
                     (fun (x,r)  ->
                        if Hashtbl.mem FanLexTools.named_regexps x
                        then
                          Printf.eprintf
                            "pa_ulex (warning): multiple definition of named regexp '%s'\n"
                            x
                        else ();
                        Hashtbl.add FanLexTools.named_regexps x r) xrs;
                   Ast.StNil _loc : 'declare_regexp ))))])]);
  Gram.extend (regexps : 'regexps Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "{";
           `Slist1sep
             ((`Snterm (Gram.obj (regexp : 'regexp Gram.t ))),
               (`Skeyword ";"));
           `Skeyword "}"],
            (Gram.mk_action
               (fun _  (xs : 'regexp list)  _  (_loc : FanLoc.t)  ->
                  (Array.of_list xs : 'regexps ))))])]);
  Gram.extend (regexp : 'regexp Gram.t )
    (None,
      [(None, None,
         [([`Sself; `Skeyword "|"; `Sself],
            (Gram.mk_action
               (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                  (FanLexTools.alt r1 r2 : 'regexp ))))]);
      (None, None,
        [([`Sself; `Sself],
           (Gram.mk_action
              (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                 (FanLexTools.seq r1 r2 : 'regexp ))))]);
      (None, None,
        [([`Sself; `Skeyword "*"],
           (Gram.mk_action
              (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                 (FanLexTools.rep r1 : 'regexp ))));
        ([`Sself; `Skeyword "+"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                (FanLexTools.plus r1 : 'regexp ))));
        ([`Sself; `Skeyword "?"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  (_loc : FanLoc.t)  ->
                (FanLexTools.alt FanLexTools.eps r1 : 'regexp ))));
        ([`Skeyword "("; `Sself; `Skeyword ")"],
          (Gram.mk_action
             (fun _  (r1 : 'regexp)  _  (_loc : FanLoc.t)  -> (r1 : 'regexp ))));
        ([`Skeyword "_"],
          (Gram.mk_action
             (fun _  (_loc : FanLoc.t)  ->
                (FanLexTools.chars LexSet.any : 'regexp ))));
        ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
          (Gram.mk_action
             (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                (FanLexTools.chars (LexSet.singleton c) : 'regexp ))));
        ([`Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `STR (s,_) -> (FanLexTools.of_string s : 'regexp )
                | _ -> assert false)));
        ([`Skeyword "[";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          (Gram.mk_action
             (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                (FanLexTools.chars cc : 'regexp ))));
        ([`Skeyword "[^";
         `Snterm (Gram.obj (ch_class : 'ch_class Gram.t ));
         `Skeyword "]"],
          (Gram.mk_action
             (fun _  (cc : 'ch_class)  _  (_loc : FanLoc.t)  ->
                (FanLexTools.chars (LexSet.difference LexSet.any cc) : 
                'regexp ))));
        ([`Stoken
            (((function | `LID _ -> true | _ -> false)), (`Normal, "`LID _"))],
          (Gram.mk_action
             (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                match __fan_0 with
                | `LID x ->
                    ((try Hashtbl.find FanLexTools.named_regexps x
                      with
                      | Not_found  ->
                          failwithf
                            "referenced to unbound named  regexp  `%s'" x) : 
                    'regexp )
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
                 | `INT (i,s) ->
                     (if (i >= 0) && (i <= LexSet.max_code)
                      then i
                      else failwithf "Invalid Unicode code point:%s" s : 
                     'chr )
                 | _ -> assert false)))])]);
  Gram.extend (ch_class : 'ch_class Gram.t )
    (None,
      [(None, None,
         [([`Snterm (Gram.obj (chr : 'chr Gram.t ));
           `Skeyword "-";
           `Snterm (Gram.obj (chr : 'chr Gram.t ))],
            (Gram.mk_action
               (fun (c2 : 'chr)  _  (c1 : 'chr)  (_loc : FanLoc.t)  ->
                  (LexSet.interval c1 c2 : 'ch_class ))));
         ([`Snterm (Gram.obj (chr : 'chr Gram.t ))],
           (Gram.mk_action
              (fun (c : 'chr)  (_loc : FanLoc.t)  ->
                 (LexSet.singleton c : 'ch_class ))));
         ([`Sself; `Sself],
           (Gram.mk_action
              (fun (cc2 : 'ch_class)  (cc1 : 'ch_class)  (_loc : FanLoc.t) 
                 -> (LexSet.union cc1 cc2 : 'ch_class ))));
         ([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                 match __fan_0 with
                 | `STR (s,_) ->
                     (let c = ref LexSet.empty in
                      (for i = 0 to (String.length s) - 1 do
                         c :=
                           (LexSet.union c.contents
                              (LexSet.singleton (Char.code (s.[i]))))
                       done;
                       c.contents) : 'ch_class )
                 | _ -> assert false)))])])
let _ = AstQuotation.of_expr ~name:"lex" ~entry:lex
let _ = AstQuotation.of_str_item ~name:"lex.regexp" ~entry:declare_regexp