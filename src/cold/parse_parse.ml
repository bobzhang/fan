let gm = Compile_gram.gm
let module_name = Compile_gram.module_name
let mk_prule = Compile_gram.mk_prule
let make = Compile_gram.make
let is_irrefut_pat = Fan_ops.is_irrefut_pat
let sem_of_list = Ast_gen.sem_of_list
let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
let tuple_com = Ast_gen.tuple_com
open FAst
open Util
let mk_name (i : FAst.vid) =
  (let rec aux x =
     match (x : FAst.vid ) with
     | `Lid (_,x)|`Uid (_,x) -> x
     | `Dot (_,`Uid (_,x),xs) -> x ^ ("__" ^ (aux xs))
     | _ -> failwith "internal error in the Grammar extension" in
   { id = i; tvar = (aux i); loc = (loc_of i) } : Gram_def.name )
let g =
  Gramf.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["(";
              ")";
              ",";
              "as";
              "|";
              "_";
              ":";
              ".";
              ";";
              "{";
              "}";
              "let";
              "[";
              "]";
              "SEP";
              "LEVEL";
              "S";
              "EOI";
              "Lid";
              "Uid";
              "Ant";
              "Quot";
              "DirQuotation";
              "Str";
              "Label";
              "Optlabel";
              "Chr";
              "Int";
              "Int32";
              "Int64";
              "Int64";
              "Nativeint";
              "Flo";
              "TRY";
              "PEEK";
              "L0";
              "L1";
              "First";
              "Last";
              "Before";
              "After";
              "Level";
              "LA";
              "RA";
              "NA";
              "+";
              "*";
              "?";
              "=";
              "@";
              "Inline"] ()
let inline_rules: (string,Gram_def.rule list) Hashtbl.t = Hashtbl.create 50
let query_inline (x : string) = Hashtblf.find_opt inline_rules x
let extend_header = Gramf.mk_dynamic g "extend_header"
let qualuid: vid Gramf.t = Gramf.mk_dynamic g "qualuid"
let qualid: vid Gramf.t = Gramf.mk_dynamic g "qualid"
let t_qualid: vid Gramf.t = Gramf.mk_dynamic g "t_qualid"
let entry_name:
  ([ `name of Tokenf.name option | `non]* Gram_def.name) Gramf.t =
  Gramf.mk_dynamic g "entry_name"
let position = Gramf.mk_dynamic g "position"
let assoc = Gramf.mk_dynamic g "assoc"
let name = Gramf.mk_dynamic g "name"
let string = Gramf.mk_dynamic g "string"
let rules = Gramf.mk_dynamic g "rules"
let symbol: Gram_def.psymbol list Gramf.t = Gramf.mk_dynamic g "symbol"
let rule = Gramf.mk_dynamic g "rule"
let meta_rule = Gramf.mk_dynamic g "meta_rule"
let rule_list = Gramf.mk_dynamic g "rule_list"
let psymbol = Gramf.mk_dynamic g "psymbol"
let level = Gramf.mk_dynamic g "level"
let level_list = Gramf.mk_dynamic g "level_list"
let entry: Gram_def.entry option Gramf.t = Gramf.mk_dynamic g "entry"
let extend_body = Gramf.mk_dynamic g "extend_body"
let unsafe_extend_body = Gramf.mk_dynamic g "unsafe_extend_body"
let simple: Gram_def.psymbol list Gramf.t = Gramf.mk_dynamic g "simple"
let single_symbol: Gram_def.symbol Gramf.t =
  Gramf.mk_dynamic g "single_symbol"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let or_words: 'or_words Gramf.t = grammar_entry_create "or_words"
  and str: 'str Gramf.t = grammar_entry_create "str"
  and or_strs: 'or_strs Gramf.t = grammar_entry_create "or_strs"
  and str0: 'str0 Gramf.t = grammar_entry_create "str0"
  and level_str: 'level_str Gramf.t = grammar_entry_create "level_str"
  and sep_symbol: 'sep_symbol Gramf.t = grammar_entry_create "sep_symbol" in
  Gramf.extend_single (single_symbol : 'single_symbol Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, \"EOI\")), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Empty\"))))) in\nlet des_str = Gram_pat.to_string (`Vrn (_loc, v)) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Key ({ txt = v;_} : Tokenf.txt) ->
                        (let i = hash_variant v in
                         let pred: FAst.exp =
                           `Fun
                             (_loc,
                               (`Bar
                                  (_loc,
                                    (`Case
                                       (_loc,
                                         (`App
                                            (_loc, (`Vrn (_loc, "EOI")),
                                              (`Any _loc))),
                                         (`Lid (_loc, "true")))),
                                    (`Case
                                       (_loc, (`Any _loc),
                                         (`Lid (_loc, "false"))))))) in
                         let des: FAst.exp =
                           `Par
                             (_loc,
                               (`Com
                                  (_loc, (`Int (_loc, (string_of_int i))),
                                    (`Vrn (_loc, "Empty"))))) in
                         let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in
                         {
                           text = (`Token (_loc, pred, des, des_str));
                           styp = (`Tok _loc);
                           pattern = None;
                           outer_pattern = None
                         } : 'single_symbol )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com\n         (_loc, (`Int (_loc, (string_of_int i))),\n           (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Constraint
                                                (_loc,
                                                  (`Record
                                                     (_loc,
                                                       (`Sem
                                                          (_loc,
                                                            (`RecBind
                                                               (_loc,
                                                                 (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                 (`Str
                                                                    (_loc, x)))),
                                                            (`Any _loc))))),
                                                  (`Dot
                                                     (_loc,
                                                       (`Uid (_loc, "Tokenf")),
                                                       (`Lid (_loc, "txt")))))))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`App
                                      (_loc, (`Vrn (_loc, "A")),
                                        (`Str (_loc, x))))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Str (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com\n         (_loc, (`Int (_loc, (string_of_int i))),\n           (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Constraint
                                                (_loc,
                                                  (`Record
                                                     (_loc,
                                                       (`Sem
                                                          (_loc,
                                                            (`RecBind
                                                               (_loc,
                                                                 (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                 (`Str
                                                                    (_loc, x)))),
                                                            (`Any _loc))))),
                                                  (`Dot
                                                     (_loc,
                                                       (`Uid (_loc, "Tokenf")),
                                                       (`Lid (_loc, "txt")))))))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`App
                                      (_loc, (`Vrn (_loc, "A")),
                                        (`Str (_loc, x))))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Str (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com\n         (_loc, (`Int (_loc, (string_of_int i))),\n           (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Constraint
                                                (_loc,
                                                  (`Record
                                                     (_loc,
                                                       (`Sem
                                                          (_loc,
                                                            (`RecBind
                                                               (_loc,
                                                                 (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                 (`Str
                                                                    (_loc, x)))),
                                                            (`Any _loc))))),
                                                  (`Dot
                                                     (_loc,
                                                       (`Uid (_loc, "Tokenf")),
                                                       (`Lid (_loc, "txt")))))))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`App
                                      (_loc, (`Vrn (_loc, "A")),
                                        (`Str (_loc, x))))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Str (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid"],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern = None;
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid"],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern = None;
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str"],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern = None;
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int32";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int64";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Nativeint";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Flo";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Chr";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Label";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Optlabel";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"loc\")),\n                                (`Lid (xloc, loc)))),\n                           (`Sem\n                              (xloc,\n                                (`RecBind\n                                   (xloc, (`Lid (xloc, \"txt\")),\n                                     (`Lid (xloc, x)))), (`Any xloc))))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "loc")),
                                                        (`Lid (xloc, loc)))),
                                                   (`Sem
                                                      (xloc,
                                                        (`RecBind
                                                           (xloc,
                                                             (`Lid
                                                                (xloc, "txt")),
                                                             (`Lid (xloc, x)))),
                                                        (`Any xloc))))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"loc\")),\n                                (`Lid (xloc, loc)))),\n                           (`Sem\n                              (xloc,\n                                (`RecBind\n                                   (xloc, (`Lid (xloc, \"txt\")),\n                                     (`Lid (xloc, x)))), (`Any xloc))))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "loc")),
                                                        (`Lid (xloc, loc)))),
                                                   (`Sem
                                                      (xloc,
                                                        (`RecBind
                                                           (xloc,
                                                             (`Lid
                                                                (xloc, "txt")),
                                                             (`Lid (xloc, x)))),
                                                        (`Any xloc))))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, v)),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"loc\")),\n                                (`Lid (xloc, loc)))),\n                           (`Sem\n                              (xloc,\n                                (`RecBind\n                                   (xloc, (`Lid (xloc, \"txt\")),\n                                     (`Lid (xloc, x)))), (`Any xloc))))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, v)),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "loc")),
                                                        (`Lid (xloc, loc)))),
                                                   (`Sem
                                                      (xloc,
                                                        (`RecBind
                                                           (xloc,
                                                             (`Lid
                                                                (xloc, "txt")),
                                                             (`Lid (xloc, x)))),
                                                        (`Any xloc))))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Quot";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ txt = x;_} : Tokenf.txt),`Key
                                                         ({ txt = v;_} :
                                                           Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "DirQuotation";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let i = hash_variant v in\nlet pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Tok _loc);\n  pattern =\n    (Some (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ txt = x;_} : Tokenf.txt),`Key
                                                         ({ txt = v;_} :
                                                           Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let pred: FAst.exp =
                          `Fun
                            (_loc,
                              (`Bar
                                 (_loc,
                                   (`Case
                                      (_loc,
                                        (`App
                                           (_loc, (`Vrn (_loc, v)),
                                             (`Any _loc))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Par
                            (_loc,
                              (`Com
                                 (_loc, (`Int (_loc, (string_of_int i))),
                                   (`Vrn (_loc, "Any"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s")],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Tok _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = s;_} : Tokenf.txt) ->
                       ({
                          text = (`Keyword (_loc, s));
                          styp = (`Tok _loc);
                          pattern = None;
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s");
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid i")],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Tok _loc);\n  pattern =\n    (Some\n       (`App\n          (xloc, (`Vrn (xloc, \"Key\")),\n            (`Constraint\n               (xloc,\n                 (`Record\n                    (xloc,\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, i)))),\n                           (`Any xloc))))),\n                 (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = i;_} : Tokenf.txt),`Str
                                                                    ({
                                                                    txt = s;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       ({
                          text = (`Keyword (_loc, s));
                          styp = (`Tok _loc);
                          pattern =
                            (Some
                               (`App
                                  (xloc, (`Vrn (xloc, "Key")),
                                    (`Constraint
                                       (xloc,
                                         (`Record
                                            (xloc,
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "loc")),
                                                        (`Lid (xloc, i)))),
                                                   (`Any xloc))))),
                                         (`Dot
                                            (xloc, (`Uid (xloc, "Tokenf")),
                                              (`Lid (xloc, "txt"))))))) : 
                               FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("{\n  text = (`Nterm (_loc, n, lev));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let lev = None in
                   ({
                      text = (`Nterm (_loc, n, lev));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      pattern = None;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Nterm (Gramf.obj (level_str : 'level_str Gramf.t ))],
           ("{\n  text = (`Nterm (_loc, n, lev));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(lev : 'level_str)  ~__fan_0:(n : 'name) 
                   (_loc : Locf.t)  ->
                   let lev = Some lev in
                   ({
                      text = (`Nterm (_loc, n, lev));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      pattern = None;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "S"],
           ("{\n  text = (`Self _loc);\n  styp = (`Self _loc);\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ({
                      text = (`Self _loc);
                      styp = (`Self _loc);
                      pattern = None;
                      outer_pattern = None
                    } : 'single_symbol )))))]) : Gramf.olevel ));
  Gramf.extend_single (or_words : 'or_words Gramf.t )
    (None,
      ((None, None,
         [([`List1sep
              ((`Nterm (Gramf.obj (str : 'str Gramf.t ))), (`Keyword "|"))],
            ("(v, None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(v : 'str list)  (_loc : Locf.t)  ->
                    ((v, None) : 'or_words )))));
         ([`List1sep
             ((`Nterm (Gramf.obj (str : 'str Gramf.t ))), (`Keyword "|"));
          `Keyword "as";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid s")],
           ("(v, (Some (xloc, s)))\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(v : 'str list)  (_loc : Locf.t)  ->
                   match __fan_2 with
                   | `Lid ({ loc = xloc; txt = s;_} : Tokenf.txt) ->
                       ((v, (Some (xloc, s))) : 'or_words )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_2))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (str : 'str Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "`Str s")],
            ("(s, _loc)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = s;_} : Tokenf.txt) -> ((s, _loc) : 'str )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (simple : 'simple Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, \"EOI\")), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com\n            (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Empty\"))))) in\n   let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern = None;\n     outer_pattern = None\n   })\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Key ({ txt = v;_} : Tokenf.txt) ->
                        (((fun (symbol : Gram_def.symbol)  ->
                             [({ kind = Gram_def.KNormal; symbol } : 
                             Gram_def.psymbol )]))
                           (let i = hash_variant v in
                            let pred: FAst.exp =
                              `Fun
                                (_loc,
                                  (`Bar
                                     (_loc,
                                       (`Case
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, "EOI")),
                                                 (`Any _loc))),
                                            (`Lid (_loc, "true")))),
                                       (`Case
                                          (_loc, (`Any _loc),
                                            (`Lid (_loc, "false"))))))) in
                            let des: FAst.exp =
                              `Par
                                (_loc,
                                  (`Com
                                     (_loc, (`Int (_loc, (string_of_int i))),
                                       (`Vrn (_loc, "Empty"))))) in
                            let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in
                            {
                              text = (`Token (_loc, pred, des, des_str));
                              styp = (`Tok _loc);
                              pattern = None;
                              outer_pattern = None
                            }) : 'simple )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com\n            (_loc, (`Int (_loc, (string_of_int i))),\n              (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Str (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Constraint
                                                   (_loc,
                                                     (`Record
                                                        (_loc,
                                                          (`Sem
                                                             (_loc,
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (
                                                                    `Str
                                                                    (_loc, x)))),
                                                               (`Any _loc))))),
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc, "Tokenf")),
                                                          (`Lid (_loc, "txt")))))))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`App
                                         (_loc, (`Vrn (_loc, "A")),
                                           (`Str (_loc, x))))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Str (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com\n            (_loc, (`Int (_loc, (string_of_int i))),\n              (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Str (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Constraint
                                                   (_loc,
                                                     (`Record
                                                        (_loc,
                                                          (`Sem
                                                             (_loc,
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (
                                                                    `Str
                                                                    (_loc, x)))),
                                                               (`Any _loc))))),
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc, "Tokenf")),
                                                          (`Lid (_loc, "txt")))))))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`App
                                         (_loc, (`Vrn (_loc, "A")),
                                           (`Str (_loc, x))))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Str (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Token
            (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
              "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com\n            (_loc, (`Int (_loc, (string_of_int i))),\n              (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Str (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Str ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Constraint
                                                   (_loc,
                                                     (`Record
                                                        (_loc,
                                                          (`Sem
                                                             (_loc,
                                                               (`RecBind
                                                                  (_loc,
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (
                                                                    `Str
                                                                    (_loc, x)))),
                                                               (`Any _loc))))),
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc, "Tokenf")),
                                                          (`Lid (_loc, "txt")))))))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`App
                                         (_loc, (`Vrn (_loc, "A")),
                                           (`Str (_loc, x))))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Str (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern = None;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern = None;
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern = None;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern = None;
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern = None;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = v;_} : Tokenf.txt) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern = None;
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int32";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Int64";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Nativeint";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Flo";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Chr";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Label";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Optlabel";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "txt")),
                                                           (`Lid (xloc, x)))),
                                                      (`Any xloc))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"loc\")),\n                                   (`Lid (xloc, loc)))),\n                              (`Sem\n                                 (xloc,\n                                   (`RecBind\n                                      (xloc, (`Lid (xloc, \"txt\")),\n                                        (`Lid (xloc, x)))), (`Any xloc))))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "loc")),
                                                           (`Lid (xloc, loc)))),
                                                      (`Sem
                                                         (xloc,
                                                           (`RecBind
                                                              (xloc,
                                                                (`Lid
                                                                   (xloc,
                                                                    "txt")),
                                                                (`Lid
                                                                   (xloc, x)))),
                                                           (`Any xloc))))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"loc\")),\n                                   (`Lid (xloc, loc)))),\n                              (`Sem\n                                 (xloc,\n                                   (`RecBind\n                                      (xloc, (`Lid (xloc, \"txt\")),\n                                        (`Lid (xloc, x)))), (`Any xloc))))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "loc")),
                                                           (`Lid (xloc, loc)))),
                                                      (`Sem
                                                         (xloc,
                                                           (`RecBind
                                                              (xloc,
                                                                (`Lid
                                                                   (xloc,
                                                                    "txt")),
                                                                (`Lid
                                                                   (xloc, x)))),
                                                           (`Any xloc))))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some\n          (`App\n             (xloc, (`Vrn (xloc, v)),\n               (`Constraint\n                  (xloc,\n                    (`Record\n                       (xloc,\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"loc\")),\n                                   (`Lid (xloc, loc)))),\n                              (`Sem\n                                 (xloc,\n                                   (`RecBind\n                                      (xloc, (`Lid (xloc, \"txt\")),\n                                        (`Lid (xloc, x)))), (`Any xloc))))))),\n                    (`Dot\n                       (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.t) 
                   ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = x;_} : Tokenf.txt),`Lid
                                                                    ({
                                                                    txt = loc;_}
                                                                    :
                                                                    Tokenf.txt),
                      `Key ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (xloc, (`Vrn (xloc, v)),
                                       (`Constraint
                                          (xloc,
                                            (`Record
                                               (xloc,
                                                 (`Sem
                                                    (xloc,
                                                      (`RecBind
                                                         (xloc,
                                                           (`Lid
                                                              (xloc, "loc")),
                                                           (`Lid (xloc, loc)))),
                                                      (`Sem
                                                         (xloc,
                                                           (`RecBind
                                                              (xloc,
                                                                (`Lid
                                                                   (xloc,
                                                                    "txt")),
                                                                (`Lid
                                                                   (xloc, x)))),
                                                           (`Any xloc))))))),
                                            (`Dot
                                               (xloc,
                                                 (`Uid (xloc, "Tokenf")),
                                                 (`Lid (xloc, "txt"))))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s %s"
                            (Tokenf.to_string __fan_3)
                            (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "Quot";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ txt = x;_} : Tokenf.txt),`Key
                                                         ({ txt = v;_} :
                                                           Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (_loc, (`Vrn (_loc, v)),
                                       (`Lid (_loc, x))) : FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "DirQuotation";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let i = hash_variant v in\n   let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Par\n       (_loc,\n         (`Com (_loc, (`Int (_loc, (string_of_int i))), (`Vrn (_loc, \"Any\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Tok _loc);\n     pattern =\n       (Some (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))) : FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (`Lid ({ txt = x;_} : Tokenf.txt),`Key
                                                         ({ txt = v;_} :
                                                           Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let i = hash_variant v in
                           let pred: FAst.exp =
                             `Fun
                               (_loc,
                                 (`Bar
                                    (_loc,
                                      (`Case
                                         (_loc,
                                           (`App
                                              (_loc, (`Vrn (_loc, v)),
                                                (`Any _loc))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Par
                               (_loc,
                                 (`Com
                                    (_loc, (`Int (_loc, (string_of_int i))),
                                      (`Vrn (_loc, "Any"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp = (`Tok _loc);
                             pattern =
                               (Some
                                  (`App
                                     (_loc, (`Vrn (_loc, v)),
                                       (`Lid (_loc, x))) : FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_1)
                            (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Tok _loc);\n    pattern = None;\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str ({ txt = s;_} : Tokenf.txt) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = KNormal; symbol } : Gram_def.psymbol )]))
                          {
                            text = (`Keyword (_loc, s));
                            styp = (`Tok _loc);
                            pattern = None;
                            outer_pattern = None
                          } : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s");
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid i")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Tok _loc);\n    pattern =\n      (Some\n         (`App\n            (xloc, (`Vrn (xloc, \"Key\")),\n              (`Constraint\n                 (xloc,\n                   (`Record\n                      (xloc,\n                        (`Sem\n                           (xloc,\n                             (`RecBind\n                                (xloc, (`Lid (xloc, \"loc\")),\n                                  (`Lid (xloc, i)))), (`Any xloc))))),\n                   (`Dot\n                      (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))))) : \n         FAst.pat ));\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = i;_} : Tokenf.txt),`Str
                                                                    ({
                                                                    txt = s;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = KNormal; symbol } : Gram_def.psymbol )]))
                          {
                            text = (`Keyword (_loc, s));
                            styp = (`Tok _loc);
                            pattern =
                              (Some
                                 (`App
                                    (xloc, (`Vrn (xloc, "Key")),
                                      (`Constraint
                                         (xloc,
                                           (`Record
                                              (xloc,
                                                (`Sem
                                                   (xloc,
                                                     (`RecBind
                                                        (xloc,
                                                          (`Lid (xloc, "loc")),
                                                          (`Lid (xloc, i)))),
                                                     (`Any xloc))))),
                                           (`Dot
                                              (xloc, (`Uid (xloc, "Tokenf")),
                                                (`Lid (xloc, "txt"))))))) : 
                                 FAst.pat ));
                            outer_pattern = None
                          } : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Nterm (_loc, n, lev));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    pattern = None;\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let lev = None in
                   ((fun (symbol : Gram_def.symbol)  ->
                       [({ kind = KNormal; symbol } : Gram_def.psymbol )])
                      {
                        text = (`Nterm (_loc, n, lev));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        pattern = None;
                        outer_pattern = None
                      } : 'simple )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Nterm (Gramf.obj (level_str : 'level_str Gramf.t ))],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Nterm (_loc, n, lev));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    pattern = None;\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(lev : 'level_str)  ~__fan_0:(n : 'name) 
                   (_loc : Locf.t)  ->
                   let lev = Some lev in
                   ((fun (symbol : Gram_def.symbol)  ->
                       [({ kind = KNormal; symbol } : Gram_def.psymbol )])
                      {
                        text = (`Nterm (_loc, n, lev));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        pattern = None;
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "S"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Self _loc);\n    styp = (`Self _loc);\n    pattern = None;\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ((fun (symbol : Gram_def.symbol)  ->
                       [({ kind = KNormal; symbol } : Gram_def.psymbol )])
                      {
                        text = (`Self _loc);
                        styp = (`Self _loc);
                        pattern = None;
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Ant";
          `Keyword "(";
          `Nterm (Gramf.obj (or_words : 'or_words Gramf.t ));
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid s");
          `Keyword ")"],
           ("let i = hash_variant v in\nlet p = `Lid (xloc, s) in\nmatch ps with\n| (vs,y) ->\n    vs |>\n      (List.map\n         (fun (x,xloc)  ->\n            let z = `Str (xloc, x) in\n            let pred: FAst.exp =\n              `Fun\n                (_loc,\n                  (`Bar\n                     (_loc,\n                       (`Case\n                          (_loc,\n                            (`App\n                               (_loc, (`Vrn (_loc, v)),\n                                 (`Constraint\n                                    (_loc,\n                                      (`Record\n                                         (_loc,\n                                           (`Sem\n                                              (_loc,\n                                                (`RecBind\n                                                   (_loc,\n                                                     (`Lid (_loc, \"kind\")),\n                                                     z)), (`Any _loc))))),\n                                      (`Dot\n                                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                                           (`Lid (_loc, \"ant\")))))))),\n                            (`Lid (_loc, \"true\")))),\n                       (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n            let des: FAst.exp =\n              `Par\n                (_loc,\n                  (`Com\n                     (_loc, (`Int (_loc, (string_of_int i))),\n                       (`App (_loc, (`Vrn (_loc, \"A\")), z))))) in\n            let des_str =\n              Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), p)) in\n            let pp =\n              match y with\n              | None  -> (z : FAst.pat )\n              | Some (xloc,u) ->\n                  (`Alias (xloc, z, (`Lid (xloc, u))) : FAst.pat ) in\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Token (_loc, pred, des, des_str));\n                   styp = (`Tok _loc);\n                   pattern =\n                     (Some\n                        (`App\n                           (_loc, (`Vrn (_loc, v)),\n                             (`Constraint\n                                (_loc,\n                                  (`Alias\n                                     (_loc,\n                                       (`Record\n                                          (_loc,\n                                            (`Sem\n                                               (_loc,\n                                                 (`RecBind\n                                                    (_loc,\n                                                      (`Lid (_loc, \"kind\")),\n                                                      pp)), (`Any _loc))))),\n                                       p)),\n                                  (`Dot\n                                     (_loc, (`Uid (_loc, \"Tokenf\")),\n                                       (`Lid (_loc, \"ant\"))))))) : FAst.pat ));\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.t)  ~__fan_3:_ 
                   ~__fan_2:(ps : 'or_words)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_4, __fan_0) with
                   | (`Lid ({ loc = xloc; txt = s;_} : Tokenf.txt),`Key
                                                                    ({
                                                                    txt = v;_}
                                                                    :
                                                                    Tokenf.txt))
                       ->
                       (let i = hash_variant v in
                        let p = `Lid (xloc, s) in
                        (match ps with
                         | (vs,y) ->
                             vs |>
                               (List.map
                                  (fun (x,xloc)  ->
                                     let z = `Str (xloc, x) in
                                     let pred: FAst.exp =
                                       `Fun
                                         (_loc,
                                           (`Bar
                                              (_loc,
                                                (`Case
                                                   (_loc,
                                                     (`App
                                                        (_loc,
                                                          (`Vrn (_loc, v)),
                                                          (`Constraint
                                                             (_loc,
                                                               (`Record
                                                                  (_loc,
                                                                    (
                                                                    `Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "kind")),
                                                                    z)),
                                                                    (`Any
                                                                    _loc))))),
                                                               (`Dot
                                                                  (_loc,
                                                                    (
                                                                    `Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (
                                                                    `Lid
                                                                    (_loc,
                                                                    "ant")))))))),
                                                     (`Lid (_loc, "true")))),
                                                (`Case
                                                   (_loc, (`Any _loc),
                                                     (`Lid (_loc, "false"))))))) in
                                     let des: FAst.exp =
                                       `Par
                                         (_loc,
                                           (`Com
                                              (_loc,
                                                (`Int
                                                   (_loc, (string_of_int i))),
                                                (`App
                                                   (_loc, (`Vrn (_loc, "A")),
                                                     z))))) in
                                     let des_str =
                                       Gram_pat.to_string
                                         (`App (_loc, (`Vrn (_loc, v)), p)) in
                                     let pp =
                                       match y with
                                       | None  -> (z : FAst.pat )
                                       | Some (xloc,u) ->
                                           (`Alias
                                              (xloc, z, (`Lid (xloc, u))) : 
                                           FAst.pat ) in
                                     ({
                                        kind = KNormal;
                                        symbol =
                                          {
                                            text =
                                              (`Token
                                                 (_loc, pred, des, des_str));
                                            styp = (`Tok _loc);
                                            pattern =
                                              (Some
                                                 (`App
                                                    (_loc, (`Vrn (_loc, v)),
                                                      (`Constraint
                                                         (_loc,
                                                           (`Alias
                                                              (_loc,
                                                                (`Record
                                                                   (_loc,
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "kind")),
                                                                    pp)),
                                                                    (`Any
                                                                    _loc))))),
                                                                p)),
                                                           (`Dot
                                                              (_loc,
                                                                (`Uid
                                                                   (_loc,
                                                                    "Tokenf")),
                                                                (`Lid
                                                                   (_loc,
                                                                    "ant"))))))) : 
                                                 FAst.pat ));
                                            outer_pattern = None
                                          }
                                      } : Gram_def.psymbol )))) : 'simple )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_4)
                            (Tokenf.to_string __fan_0))))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
          `Keyword ")"],
           ("match v with\n| (vs,None ) ->\n    vs |>\n      (List.map\n         (fun x  ->\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Keyword (_loc, x));\n                   styp = (`Tok _loc);\n                   pattern = None;\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n| (vs,Some b) ->\n    vs |>\n      (List.map\n         (fun x  ->\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Keyword (_loc, x));\n                   styp = (`Tok _loc);\n                   pattern =\n                     (Some\n                        (`App\n                           (_loc, (`Vrn (_loc, \"Key\")),\n                             (`Constraint\n                                (_loc,\n                                  (`Record\n                                     (_loc,\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"txt\")),\n                                                 (`Lid (_loc, b)))),\n                                            (`Any _loc))))),\n                                  (`Dot\n                                     (_loc, (`Uid (_loc, \"Tokenf\")),\n                                       (`Lid (_loc, \"txt\"))))))) : FAst.pat ));\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(v : 'or_strs)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (match v with
                    | (vs,None ) ->
                        vs |>
                          (List.map
                             (fun x  ->
                                ({
                                   kind = KNormal;
                                   symbol =
                                     {
                                       text = (`Keyword (_loc, x));
                                       styp = (`Tok _loc);
                                       pattern = None;
                                       outer_pattern = None
                                     }
                                 } : Gram_def.psymbol )))
                    | (vs,Some b) ->
                        vs |>
                          (List.map
                             (fun x  ->
                                ({
                                   kind = KNormal;
                                   symbol =
                                     {
                                       text = (`Keyword (_loc, x));
                                       styp = (`Tok _loc);
                                       pattern =
                                         (Some
                                            (`App
                                               (_loc, (`Vrn (_loc, "Key")),
                                                 (`Constraint
                                                    (_loc,
                                                      (`Record
                                                         (_loc,
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")),
                                                                    (`Lid
                                                                    (_loc, b)))),
                                                                (`Any _loc))))),
                                                      (`Dot
                                                         (_loc,
                                                           (`Uid
                                                              (_loc,
                                                                "Tokenf")),
                                                           (`Lid
                                                              (_loc, "txt"))))))) : 
                                            FAst.pat ));
                                       outer_pattern = None
                                     }
                                 } : Gram_def.psymbol ))) : 'simple )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (or_strs : 'or_strs Gramf.t )
    (None,
      ((None, None,
         [([`List1sep
              ((`Nterm (Gramf.obj (str0 : 'str0 Gramf.t ))), (`Keyword "|"))],
            ("(xs, None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(xs : 'str0 list)  (_loc : Locf.t)  ->
                    ((xs, None) : 'or_strs )))));
         ([`List1sep
             ((`Nterm (Gramf.obj (str0 : 'str0 Gramf.t ))), (`Keyword "|"));
          `Keyword "as";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid s")],
           ("(xs, (Some s))\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(xs : 'str0 list)  (_loc : Locf.t)  ->
                   match __fan_2 with
                   | `Lid ({ txt = s;_} : Tokenf.txt) ->
                       ((xs, (Some s)) : 'or_strs )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_2))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (str0 : 'str0 Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "`Str s")],
            ("s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = s;_} : Tokenf.txt) -> (s : 'str0 )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (level_str : 'level_str Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "Level";
           `Token
             (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
               "`Str s")],
            ("s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | `Str ({ txt = s;_} : Tokenf.txt) -> (s : 'level_str )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (sep_symbol : 'sep_symbol Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "SEP";
           `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
            ("t\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'single_symbol)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (t : 'sep_symbol )))))]) : Gramf.olevel ));
  Gramf.extend_single (symbol : 'symbol Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "L0";
           `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
            ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None }\n }]\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'single_symbol) 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    let sep = None in
                    match __fan_0 with
                    | `Key ({ txt = l;_} : Tokenf.txt) ->
                        (let styp =
                           `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                         let text =
                           `List
                             (_loc, (if l = "L0" then false else true), s,
                               sep) in
                         [{
                            kind = KNormal;
                            symbol =
                              {
                                text;
                                styp;
                                pattern = None;
                                outer_pattern = None
                              }
                          }] : 'symbol )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   let sep = None in
                   match __fan_0 with
                   | `Key ({ txt = l;_} : Tokenf.txt) ->
                       (let styp =
                          `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                        let text =
                          `List
                            (_loc, (if l = "L0" then false else true), s,
                              sep) in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp;
                               pattern = None;
                               outer_pattern = None
                             }
                         }] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "L0";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   let sep = Some sep in
                   match __fan_0 with
                   | `Key ({ txt = l;_} : Tokenf.txt) ->
                       (let styp =
                          `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                        let text =
                          `List
                            (_loc, (if l = "L0" then false else true), s,
                              sep) in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp;
                               pattern = None;
                               outer_pattern = None
                             }
                         }] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   let sep = Some sep in
                   match __fan_0 with
                   | `Key ({ txt = l;_} : Tokenf.txt) ->
                       (let styp =
                          `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                        let text =
                          `List
                            (_loc, (if l = "L0" then false else true), s,
                              sep) in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp;
                               pattern = None;
                               outer_pattern = None
                             }
                         }] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("[{ kind = KNone; symbol = { s with outer_pattern = None } };\n{ kind = KSome; symbol = { s with outer_pattern = None } }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   ([{ kind = KNone; symbol = { s with outer_pattern = None }
                     };
                    { kind = KSome; symbol = { s with outer_pattern = None }
                    }] : 'symbol )))));
         ([`Keyword "TRY";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   symbol = { text; styp = (s.styp); pattern = None; outer_pattern = None }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = p;_} : Tokenf.txt) ->
                       (let v = (_loc, (s.text)) in
                        let text = if p = "TRY" then `Try v else `Peek v in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp = (s.styp);
                               pattern = None;
                               outer_pattern = None
                             }
                         }] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "PEEK";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   symbol = { text; styp = (s.styp); pattern = None; outer_pattern = None }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = p;_} : Tokenf.txt) ->
                       (let v = (_loc, (s.text)) in
                        let text = if p = "TRY" then `Try v else `Peek v in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp = (s.styp);
                               pattern = None;
                               outer_pattern = None
                             }
                         }] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Nterm (Gramf.obj (simple : 'simple Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'simple)  (_loc : Locf.t)  ->
                   (p : 'symbol )))))]) : Gramf.olevel ));
  Gramf.extend_single (psymbol : 'psymbol Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (symbol : 'symbol Gramf.t ))],
            ("ss\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(ss : 'symbol)  (_loc : Locf.t)  ->
                    (ss : 'psymbol )))));
         ([`Nterm (Gramf.obj (symbol : 'symbol Gramf.t ));
          `Keyword "as";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid i")],
           ("List.map\n  (fun (s : Gram_def.psymbol)  ->\n     { s with symbol = { (s.symbol) with outer_pattern = (Some (xloc, i)) } })\n  ss\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(ss : 'symbol)  (_loc : Locf.t)  ->
                   match __fan_2 with
                   | `Lid ({ loc = xloc; txt = i;_} : Tokenf.txt) ->
                       (List.map
                          (fun (s : Gram_def.psymbol)  ->
                             {
                               s with
                               symbol =
                                 {
                                   (s.symbol) with
                                   outer_pattern = (Some (xloc, i))
                                 }
                             }) ss : 'psymbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_2))))))]) : 
      Gramf.olevel ))
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let str: 'str Gramf.t = grammar_entry_create "str"
  and left_rule: 'left_rule Gramf.t = grammar_entry_create "left_rule"
  and opt_action: 'opt_action Gramf.t = grammar_entry_create "opt_action" in
  Gramf.extend_single (str : 'str Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "`Str y")],
            ("y\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = y;_} : Tokenf.txt) -> (y : 'str )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (extend_header : 'extend_header Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "(";
           `Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
           `Keyword ")"],
            ("let old = gm () in let () = module_name := t in ((Some i), old)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 't_qualid)  ~__fan_2:_ 
                    ~__fan_1:(i : 'qualid)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (let old = gm () in
                     let () = module_name := t in ((Some i), old) : 'extend_header )))));
         ([`Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ))],
           ("let old = gm () in let () = module_name := t in (None, old)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'qualuid)  (_loc : Locf.t)  ->
                   (let old = gm () in
                    let () = module_name := t in (None, old) : 'extend_header )))));
         ([],
           ("(None, (gm ()))\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> ((None, (gm ())) : 'extend_header )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (extend_body : 'extend_body Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (extend_header : 'extend_header Gramf.t ));
           `List1 (`Nterm (Gramf.obj (entry : 'entry Gramf.t )))],
            ("let (gram,old) = rest in\nlet items = Listf.filter_map (fun x  -> x) el in\nlet res = make _loc { items; gram; safe = true } in\nlet () = module_name := old in res\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'entry list) 
                    ~__fan_0:(rest : 'extend_header)  (_loc : Locf.t)  ->
                    (let (gram,old) = rest in
                     let items = Listf.filter_map (fun x  -> x) el in
                     let res = make _loc { items; gram; safe = true } in
                     let () = module_name := old in res : 'extend_body )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (unsafe_extend_body : 'unsafe_extend_body Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (extend_header : 'extend_header Gramf.t ));
           `List1 (`Nterm (Gramf.obj (entry : 'entry Gramf.t )))],
            ("let (gram,old) = rest in\nlet items = Listf.filter_map (fun x  -> x) el in\nlet res = make _loc { items; gram; safe = false } in\nlet () = module_name := old in res\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'entry list) 
                    ~__fan_0:(rest : 'extend_header)  (_loc : Locf.t)  ->
                    (let (gram,old) = rest in
                     let items = Listf.filter_map (fun x  -> x) el in
                     let res = make _loc { items; gram; safe = false } in
                     let () = module_name := old in res : 'unsafe_extend_body )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "`Uid x");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid x")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid ({ txt = x;_} : Tokenf.txt) ->
                       (`Uid (_loc, x) : 'qualuid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (qualid : 'qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "`Uid x");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
               "`Lid i")],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid ({ txt = i;_} : Tokenf.txt) ->
                       (`Lid (_loc, i) : 'qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (t_qualid : 't_qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
                "`Uid x");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 't_qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Uid ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)), (4250480, `Any),
               "`Uid x");
          `Keyword ".";
          `Token
            (((function
               | `Lid ({ txt = "t";_} : Tokenf.txt) -> true
               | _ -> false)), (3802919, (`A "t")), "`Lid \"t\"")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.t)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (`Lid ({ txt = "t";_} : Tokenf.txt),`Uid
                                                           ({ txt = x;_} :
                                                             Tokenf.txt))
                       -> (`Uid (_loc, x) : 't_qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s %s" (Tokenf.to_string __fan_2)
                            (Tokenf.to_string __fan_0))))))]) : Gramf.olevel ));
  Gramf.extend_single (name : 'name Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (qualid : 'qualid Gramf.t ))],
            ("mk_name il\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(il : 'qualid)  (_loc : Locf.t)  ->
                    (mk_name il : 'name )))))]) : Gramf.olevel ));
  Gramf.extend_single (entry_name : 'entry_name Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (qualid : 'qualid Gramf.t ))],
            ("let x =\n  match name with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), x) with\n       | None  -> Locf.failf _loc \"DDSL `%s' not resolved\" x\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(il : 'qualid)  (_loc : Locf.t)  ->
                    let name = None in
                    (let x =
                       match name with
                       | Some x ->
                           let old = !Ast_quotation.default in
                           (match Ast_quotation.resolve_name ((`Sub []), x)
                            with
                            | None  ->
                                Locf.failf _loc "DDSL `%s' not resolved" x
                            | Some x ->
                                (Ast_quotation.default := (Some x); `name old))
                       | None  -> `non in
                     (x, (mk_name il)) : 'entry_name )))));
         ([`Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
          `Nterm (Gramf.obj (str : 'str Gramf.t ))],
           ("let x =\n  match name with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), x) with\n       | None  -> Locf.failf _loc \"DDSL `%s' not resolved\" x\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(name : 'str)  ~__fan_0:(il : 'qualid) 
                   (_loc : Locf.t)  ->
                   let name = Some name in
                   (let x =
                      match name with
                      | Some x ->
                          let old = !Ast_quotation.default in
                          (match Ast_quotation.resolve_name ((`Sub []), x)
                           with
                           | None  ->
                               Locf.failf _loc "DDSL `%s' not resolved" x
                           | Some x ->
                               (Ast_quotation.default := (Some x); `name old))
                      | None  -> `non in
                    (x, (mk_name il)) : 'entry_name )))))]) : Gramf.olevel ));
  Gramf.extend_single (entry : 'entry Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
            ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = false; pos; levels })\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(levels : 'level_list)  ~__fan_1:_ 
                    ~__fan_0:(rest : 'entry_name)  (_loc : Locf.t)  ->
                    let pos = None in
                    (let (n,p) = rest in
                     (match n with
                      | `name old -> Ast_quotation.default := old
                      | _ -> ());
                     (match (pos, levels) with
                      | (Some
                         (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),
                         `Group _) ->
                          failwithf
                            "For Group levels the position can not be applied to Level"
                      | _ -> Some { name = p; local = false; pos; levels }) : 
                      'entry )))));
         ([`Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (position : 'position Gramf.t ));
          `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = false; pos; levels })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(levels : 'level_list) 
                   ~__fan_2:(pos : 'position)  ~__fan_1:_ 
                   ~__fan_0:(rest : 'entry_name)  (_loc : Locf.t)  ->
                   let pos = Some pos in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    (match (pos, levels) with
                     | (Some
                        (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),`Group _)
                         ->
                         failwithf
                           "For Group levels the position can not be applied to Level"
                     | _ -> Some { name = p; local = false; pos; levels }) : 
                     'entry )))));
         ([`Keyword "let";
          `Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = true; pos; levels })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(levels : 'level_list)  ~__fan_2:_ 
                   ~__fan_1:(rest : 'entry_name)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   let pos = None in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    (match (pos, levels) with
                     | (Some
                        (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),`Group _)
                         ->
                         failwithf
                           "For Group levels the position can not be applied to Level"
                     | _ -> Some { name = p; local = true; pos; levels }) : 
                     'entry )))));
         ([`Keyword "let";
          `Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (position : 'position Gramf.t ));
          `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = true; pos; levels })\n",
             (Gramf.mk_action
                (fun ~__fan_4:(levels : 'level_list) 
                   ~__fan_3:(pos : 'position)  ~__fan_2:_ 
                   ~__fan_1:(rest : 'entry_name)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   let pos = Some pos in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    (match (pos, levels) with
                     | (Some
                        (`App (_loc,`Vrn (_,"Level"),_) : FAst.exp),`Group _)
                         ->
                         failwithf
                           "For Group levels the position can not be applied to Level"
                     | _ -> Some { name = p; local = true; pos; levels }) : 
                     'entry )))));
         ([`Keyword "Inline";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x");
          `Keyword ":";
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("Hashtbl.add inline_rules x rules; None\n",
             (Gramf.mk_action
                (fun ~__fan_3:(rules : 'rule_list)  ~__fan_2:_ 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   match __fan_1 with
                   | `Lid ({ txt = x;_} : Tokenf.txt) ->
                       ((Hashtbl.add inline_rules x rules; None) : 'entry )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (position : 'position Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "First"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Key ({ txt = x;_} : Tokenf.txt) ->
                        ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Last"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Before"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "After"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "Level"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (level_list : 'level_list Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "{";
           `List1 (`Nterm (Gramf.obj (level : 'level Gramf.t )));
           `Keyword "}"],
            ("`Group ll\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(ll : 'level list)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Group ll : 'level_list )))));
         ([`Nterm (Gramf.obj (level : 'level Gramf.t ))],
           ("`Single l\n",
             (Gramf.mk_action
                (fun ~__fan_0:(l : 'level)  (_loc : Locf.t)  ->
                   (`Single l : 'level_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (level : 'level Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
            ("{ label; assoc; rules }\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(rules : 'rule_list)  (_loc : Locf.t)  ->
                    let label = None and assoc = None in
                    ({ label; assoc; rules } : 'level )))));
         ([`Nterm (Gramf.obj (str : 'str Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list)  ~__fan_0:(label : 'str) 
                   (_loc : Locf.t)  ->
                   let label = Some label and assoc = None in
                   ({ label; assoc; rules } : 'level )))));
         ([`Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list)  ~__fan_0:(assoc : 'assoc)
                    (_loc : Locf.t)  ->
                   let label = None and assoc = Some assoc in
                   ({ label; assoc; rules } : 'level )))));
         ([`Nterm (Gramf.obj (str : 'str Gramf.t ));
          `Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(rules : 'rule_list)  ~__fan_1:(assoc : 'assoc)
                    ~__fan_0:(label : 'str)  (_loc : Locf.t)  ->
                   let label = Some label and assoc = Some assoc in
                   ({ label; assoc; rules } : 'level )))))]) : Gramf.olevel ));
  Gramf.extend_single (assoc : 'assoc Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "LA"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Key ({ txt = x;_} : Tokenf.txt) ->
                        ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "RA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Keyword "NA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (rule_list : 'rule_list Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "["; `Keyword "]"],
            ("[]\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ([] : 'rule_list )))));
         ([`Keyword "[";
          `List1sep
            ((`Nterm (Gramf.obj (rule : 'rule Gramf.t ))), (`Keyword "|"));
          `Keyword "]"],
           ("Listf.concat ruless\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(ruless : 'rule list)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (Listf.concat ruless : 'rule_list )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (rule : 'rule Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ))],
            ("let prods = Listf.cross prod in\nList.map (fun (prod : Gram_def.psymbol list)  -> mk_prule ~prod ~action)\n  prods\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(prod : 'left_rule)  (_loc : Locf.t)  ->
                    let action = None in
                    (let prods = Listf.cross prod in
                     List.map
                       (fun (prod : Gram_def.psymbol list)  ->
                          mk_prule ~prod ~action) prods : 'rule )))));
         ([`Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ));
          `Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))],
           ("let prods = Listf.cross prod in\nList.map (fun (prod : Gram_def.psymbol list)  -> mk_prule ~prod ~action)\n  prods\n",
             (Gramf.mk_action
                (fun ~__fan_1:(action : 'opt_action) 
                   ~__fan_0:(prod : 'left_rule)  (_loc : Locf.t)  ->
                   let action = Some action in
                   (let prods = Listf.cross prod in
                    List.map
                      (fun (prod : Gram_def.psymbol list)  ->
                         mk_prule ~prod ~action) prods : 'rule )))));
         ([`Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x")],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let action = None in
                   match __fan_1 with
                   | `Lid ({ loc = xloc; txt = x;_} : Tokenf.txt) ->
                       (let rules =
                          match query_inline x with
                          | Some x -> x
                          | None  ->
                              Locf.failf xloc "inline rules %s not found" x in
                        (match action with
                         | None  -> rules
                         | Some a ->
                             List.map
                               (fun (x : Gram_def.rule)  ->
                                  match x.action with
                                  | None  -> { x with action = (Some a) }
                                  | Some b ->
                                      {
                                        x with
                                        action =
                                          (Some
                                             (`App (_loc, a, b) : FAst.exp ))
                                      }) rules) : 'rule )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))));
         ([`Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)), (3802919, `Any),
              "`Lid x");
          `Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_2:(action : 'opt_action) 
                   ~__fan_1:(__fan_1 : Tokenf.t)  ~__fan_0:_  (_loc : Locf.t)
                    ->
                   let action = Some action in
                   match __fan_1 with
                   | `Lid ({ loc = xloc; txt = x;_} : Tokenf.txt) ->
                       (let rules =
                          match query_inline x with
                          | Some x -> x
                          | None  ->
                              Locf.failf xloc "inline rules %s not found" x in
                        (match action with
                         | None  -> rules
                         | Some a ->
                             List.map
                               (fun (x : Gram_def.rule)  ->
                                  match x.action with
                                  | None  -> { x with action = (Some a) }
                                  | Some b ->
                                      {
                                        x with
                                        action =
                                          (Some
                                             (`App (_loc, a, b) : FAst.exp ))
                                      }) rules) : 'rule )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_1))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (left_rule : 'left_rule Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (psymbol : 'psymbol Gramf.t ))],
            ("[x]\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(x : 'psymbol)  (_loc : Locf.t)  ->
                    ([x] : 'left_rule )))));
         ([`Nterm (Gramf.obj (psymbol : 'psymbol Gramf.t ));
          `Keyword ";";
          `Self],
           ("x :: xs\n",
             (Gramf.mk_action
                (fun ~__fan_2:(xs : 'left_rule)  ~__fan_1:_ 
                   ~__fan_0:(x : 'psymbol)  (_loc : Locf.t)  -> (x ::
                   xs : 'left_rule )))));
         ([],
           ("[]\n",
             (Gramf.mk_action (fun (_loc : Locf.t)  -> ([] : 'left_rule )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (opt_action : 'opt_action Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Quot _ -> true | _ -> false)),
                (904098089, `Any), "`Quot _")],
            ("if x.name = Tokenf.empty_name\nthen let expander loc _ s = Parsef.exp loc s in Tokenf.quot_expand expander x\nelse Ast_quotation.expand x Dyn_tag.exp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Quot x ->
                        (if x.name = Tokenf.empty_name
                         then
                           let expander loc _ s = Parsef.exp loc s in
                           Tokenf.quot_expand expander x
                         else Ast_quotation.expand x Dyn_tag.exp : 'opt_action )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (string : 'string Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)), (4153489, `Any),
                "`Str s")],
            ("(`Str (_loc, s) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | `Str ({ txt = s;_} : Tokenf.txt) ->
                        ((`Str (_loc, s) : FAst.exp ) : 'string )
                    | _ ->
                        failwith
                          (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)), (3257031, (`A "")), "`Ant s")],
           ("Tokenf.ant_expand Parsef.exp s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Ant (({ kind = "";_} as s) : Tokenf.ant) ->
                       (Tokenf.ant_expand Parsef.exp s : 'string )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s" (Tokenf.to_string __fan_0))))))]) : 
      Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "extend")
    ~entry:extend_body ();
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "unsafe_extend")
    ~entry:unsafe_extend_body ()
