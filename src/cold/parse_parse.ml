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
              "Pre";
              "Inf";
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
              "Inline";
              "Local"] ()
let inline_rules: (string,Gram_def.rule list) Hashtbl.t = Hashtbl.create 50
let query_inline (x : string) = Hashtblf.find_opt inline_rules x
let extend_header = Gramf.mk_dynamic g "extend_header"
let left_rule = Gramf.mk_dynamic g "left_rule"
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
  let or_strs: 'or_strs Gramf.t = grammar_entry_create "or_strs"
  and level_str: 'level_str Gramf.t = grammar_entry_create "level_str"
  and sep_symbol: 'sep_symbol Gramf.t = grammar_entry_create "sep_symbol" in
  Gramf.extend_single (single_symbol : 'single_symbol Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, \"EOI\")), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind\n                   (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Empty\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = Gram_pat.to_string (`Vrn (_loc, v)) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern = None;\n  bounds = [];\n  outer_pattern = None\n}\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = v;_} : Tokenf.txt) ->
                        (let pred: FAst.exp =
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
                           `Constraint
                             (_loc,
                               (`Record
                                  (_loc,
                                    (`Sem
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "tag")),
                                              (`Vrn (_loc, v)))),
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "word")),
                                              (`Uid (_loc, "Empty")))))))),
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "descr"))))) in
                         let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in
                         {
                           text = (`Token (_loc, pred, des, des_str));
                           styp =
                             (`Dot
                                (_loc, (`Uid (_loc, "Tokenf")),
                                  (`Lid (_loc, "txt"))));
                           pattern = None;
                           bounds = [];
                           outer_pattern = None
                         } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text =\n    (`Token\n       (_loc, pred,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"word\")),\n                             (`App\n                                (_loc, (`Uid (_loc, \"A\")), (`Str (_loc, x)))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n         FAst.exp ), des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                      (`Any xloc))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text =
                            (`Token
                               (_loc, pred,
                                 (`Constraint
                                    (_loc,
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "tag")),
                                                     (`Vrn (_loc, v)))),
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "word")),
                                                     (`App
                                                        (_loc,
                                                          (`Uid (_loc, "A")),
                                                          (`Str (_loc, x)))))))))),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Tokenf")),
                                           (`Lid (_loc, "descr"))))) : 
                                 FAst.exp ), des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "txt")),
                                                   (`Str (xloc, x)))),
                                              (`Any xloc))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text =\n    (`Token\n       (_loc, pred,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"word\")),\n                             (`App\n                                (_loc, (`Uid (_loc, \"A\")), (`Str (_loc, x)))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n         FAst.exp ), des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                      (`Any xloc))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text =
                            (`Token
                               (_loc, pred,
                                 (`Constraint
                                    (_loc,
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "tag")),
                                                     (`Vrn (_loc, v)))),
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "word")),
                                                     (`App
                                                        (_loc,
                                                          (`Uid (_loc, "A")),
                                                          (`Str (_loc, x)))))))))),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Tokenf")),
                                           (`Lid (_loc, "descr"))))) : 
                                 FAst.exp ), des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "txt")),
                                                   (`Str (xloc, x)))),
                                              (`Any xloc))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Str";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"txt\")),\n                                         (`Str (_loc, x)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"txt\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n{\n  text =\n    (`Token\n       (_loc, pred,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"word\")),\n                             (`App\n                                (_loc, (`Uid (_loc, \"A\")), (`Str (_loc, x)))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n         FAst.exp ), des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                      (`Any xloc))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                        {
                          text =
                            (`Token
                               (_loc, pred,
                                 (`Constraint
                                    (_loc,
                                      (`Record
                                         (_loc,
                                           (`Sem
                                              (_loc,
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "tag")),
                                                     (`Vrn (_loc, v)))),
                                                (`RecBind
                                                   (_loc,
                                                     (`Lid (_loc, "word")),
                                                     (`App
                                                        (_loc,
                                                          (`Uid (_loc, "A")),
                                                          (`Str (_loc, x)))))))))),
                                      (`Dot
                                         (_loc, (`Uid (_loc, "Tokenf")),
                                           (`Lid (_loc, "descr"))))) : 
                                 FAst.exp ), des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "txt")),
                                                   (`Str (xloc, x)))),
                                              (`Any xloc))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Lid"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Uid"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int32"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int64"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Nativeint"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Flo"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Chr"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Label"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Optlabel"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Str"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Pre"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int32";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Int64";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Nativeint";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Flo";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Chr";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Label";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Optlabel";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Str";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Pre";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = v in\nlet (pattern,bounds) =\n  match (x, xloc) with\n  | (Some x,Some xloc) ->\n      ((Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat )), [(xloc, x)])\n  | _ -> (None, []) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern;\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = v in
                        let (pattern,bounds) =
                          match (x, xloc) with
                          | (Some x,Some xloc) ->
                              ((Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat )), [(xloc, x)])
                          | _ -> (None, []) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern;
                          bounds;
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [(xloc, x); (lloc, loc)];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [(xloc, x); (lloc, loc)];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "loc")),
                                                   (`Lid (xloc, loc)))),
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [(xloc, x); (lloc, loc)];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [(xloc, x); (lloc, loc)];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "loc")),
                                                   (`Lid (xloc, loc)))),
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [(xloc, x); (lloc, loc)];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [(xloc, x); (lloc, loc)];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "loc")),
                                                   (`Lid (xloc, loc)))),
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [(xloc, x); (lloc, loc)];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                      (`Sem\n                         (xloc,\n                           (`RecBind\n                              (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                           (`Any xloc))))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          bounds = [(xloc, x); (lloc, loc)];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "loc")),
                                                   (`Lid (xloc, loc)))),
                                              (`Sem
                                                 (xloc,
                                                   (`RecBind
                                                      (xloc,
                                                        (`Lid (xloc, "txt")),
                                                        (`Lid (xloc, x)))),
                                                   (`Any xloc))))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Quot";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [(loc, x)];\n  pattern =\n    (Some\n       (`Constraint\n          (_loc, (`Lid (_loc, x)),\n            (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                         Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "quot"))));
                          bounds = [(loc, x)];
                          pattern =
                            (Some
                               (`Constraint
                                  (_loc, (`Lid (_loc, x)),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "quot"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "DirQuotation";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [(loc, x)];\n  pattern =\n    (Some\n       (`Constraint\n          (_loc, (`Lid (_loc, x)),\n            (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                         Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`Uid (_loc, "Any")))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str =
                          Gram_pat.to_string
                            (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "quot"))));
                          bounds = [(loc, x)];
                          pattern =
                            (Some
                               (`Constraint
                                  (_loc, (`Lid (_loc, x)),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "quot"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Inf";
          `Keyword "(";
          `Token
            (((function | `Int _ -> true | _ -> false)),
              ({ tag = `Int; word = Any } : Tokenf.descr ), "Int");
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x");
          `Keyword ")"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"level\")),\n                                         (`Int (_loc, level)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"op\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind\n                   (_loc, (`Lid (_loc, \"word\")),\n                     (`App\n                        (_loc, (`Uid (_loc, \"Level\")), (`Int (_loc, level)))))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = \"Precedence\" ^ level in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds = [(xloc, x)];\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                      (`Any xloc))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"op\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_4, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({
                                                                 txt = level;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (let pred: FAst.exp =
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
                                                                    "level")),
                                                                 (`Int
                                                                    (_loc,
                                                                    level)))),
                                                            (`Any _loc))))),
                                                  (`Dot
                                                     (_loc,
                                                       (`Uid (_loc, "Tokenf")),
                                                       (`Lid (_loc, "op")))))))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`App
                                                (_loc,
                                                  (`Uid (_loc, "Level")),
                                                  (`Int (_loc, level)))))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = "Precedence" ^ level in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "op"))));
                          bounds = [(xloc, x)];
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "txt")),
                                                   (`Lid (xloc, x)))),
                                              (`Any xloc))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "op"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Keyword "Inf";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid l");
          `Keyword "(";
          `Token
            (((function | `Int _ -> true | _ -> false)),
              ({ tag = `Int; word = Any } : Tokenf.descr ), "Int");
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x");
          `Keyword ")"],
           ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, v)),\n                     (`Constraint\n                        (_loc,\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"level\")),\n                                         (`Int (_loc, level)))), (`Any _loc))))),\n                          (`Dot\n                             (_loc, (`Uid (_loc, \"Tokenf\")),\n                               (`Lid (_loc, \"op\")))))))),\n                (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Constraint\n    (_loc,\n      (`Record\n         (_loc,\n           (`Sem\n              (_loc,\n                (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                (`RecBind\n                   (_loc, (`Lid (_loc, \"word\")),\n                     (`App\n                        (_loc, (`Uid (_loc, \"Level\")), (`Int (_loc, level)))))))))),\n      (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\nlet des_str = \"Precedence\" ^ level in\nlet p: FAst.pat = `Lid (xloc, x) in\nlet lp: FAst.pat = `Lid (lloc, l) in\n{\n  text = (`Token (_loc, pred, des, des_str));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds = [(xloc, x)];\n  pattern =\n    (Some\n       (`Constraint\n          (_loc,\n            (`Record\n               (_loc,\n                 (`Sem\n                    (_loc, (`RecBind (_loc, (`Lid (_loc, \"loc\")), lp)),\n                      (`Sem\n                         (_loc, (`RecBind (_loc, (`Lid (_loc, \"txt\")), p)),\n                           (`Any _loc))))))),\n            (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))))) : \n       FAst.pat ));\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.txt)  ~__fan_5:_ 
                   ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_6, __fan_4, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({
                                                                 txt = level;_}
                                                                : Tokenf.txt),
                      ({ loc = lloc; txt = l;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (let pred: FAst.exp =
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
                                                                    "level")),
                                                                 (`Int
                                                                    (_loc,
                                                                    level)))),
                                                            (`Any _loc))))),
                                                  (`Dot
                                                     (_loc,
                                                       (`Uid (_loc, "Tokenf")),
                                                       (`Lid (_loc, "op")))))))),
                                        (`Lid (_loc, "true")))),
                                   (`Case
                                      (_loc, (`Any _loc),
                                        (`Lid (_loc, "false"))))))) in
                        let des: FAst.exp =
                          `Constraint
                            (_loc,
                              (`Record
                                 (_loc,
                                   (`Sem
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "tag")),
                                             (`Vrn (_loc, v)))),
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "word")),
                                             (`App
                                                (_loc,
                                                  (`Uid (_loc, "Level")),
                                                  (`Int (_loc, level)))))))))),
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "descr"))))) in
                        let des_str = "Precedence" ^ level in
                        let p: FAst.pat = `Lid (xloc, x) in
                        let lp: FAst.pat = `Lid (lloc, l) in
                        {
                          text = (`Token (_loc, pred, des, des_str));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "op"))));
                          bounds = [(xloc, x)];
                          pattern =
                            (Some
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc, (`Lid (_loc, "loc")),
                                                   lp)),
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "txt")),
                                                        p)), (`Any _loc))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "op"))))) : FAst.pat ));
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern = None;\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = s;_} : Tokenf.txt) ->
                       ({
                          text = (`Keyword (_loc, s));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern = None;
                          bounds = [];
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str");
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid i")],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  pattern =\n    (Some\n       (`Constraint\n          (xloc,\n            (`Record\n               (xloc,\n                 (`Sem\n                    (xloc,\n                      (`RecBind\n                         (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, i)))),\n                      (`Any xloc))))),\n            (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n       FAst.pat ));\n  bounds = [(xloc, i)];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (({ loc = xloc; txt = i;_} : Tokenf.txt),({ txt = s;_} :
                                                                Tokenf.txt))
                       ->
                       ({
                          text = (`Keyword (_loc, s));
                          styp =
                            (`Dot
                               (_loc, (`Uid (_loc, "Tokenf")),
                                 (`Lid (_loc, "txt"))));
                          pattern =
                            (Some
                               (`Constraint
                                  (xloc,
                                    (`Record
                                       (xloc,
                                         (`Sem
                                            (xloc,
                                              (`RecBind
                                                 (xloc, (`Lid (xloc, "loc")),
                                                   (`Lid (xloc, i)))),
                                              (`Any xloc))))),
                                    (`Dot
                                       (xloc, (`Uid (xloc, "Tokenf")),
                                         (`Lid (xloc, "txt"))))) : FAst.pat ));
                          bounds = [(xloc, i)];
                          outer_pattern = None
                        } : 'single_symbol )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("{\n  text = (`Nterm (_loc, n, lev));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let lev = None in
                   ({
                      text = (`Nterm (_loc, n, lev));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      pattern = None;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Nterm (Gramf.obj (level_str : 'level_str Gramf.t ))],
           ("{\n  text = (`Nterm (_loc, n, lev));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  pattern = None;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(lev : 'level_str)  ~__fan_0:(n : 'name) 
                   (_loc : Locf.t)  ->
                   let lev = Some lev in
                   ({
                      text = (`Nterm (_loc, n, lev));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      pattern = None;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "S"],
           ("{\n  text = (`Self _loc);\n  styp = (`Self _loc);\n  pattern = None;\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ({
                      text = (`Self _loc);
                      styp = (`Self _loc);
                      pattern = None;
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))))]) : Gramf.olevel ));
  Gramf.extend_single (or_strs : 'or_strs Gramf.t )
    (None,
      ((None, None,
         [([`List1sep
              ((`Token
                  (((function | `Str _ -> true | _ -> false)),
                    ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")),
                (`Keyword "|"))],
            ("(xs, None, None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                    ((xs, None, None) : 'or_strs )))));
         ([`List1sep
             ((`Token
                 (((function | `Str _ -> true | _ -> false)),
                   ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")),
               (`Keyword "|"));
          `Keyword "as";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid s")],
           ("(xs, None, (Some (xloc, s)))\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                   match __fan_2 with
                   | ({ loc = xloc; txt = s;_} : Tokenf.txt) ->
                       ((xs, None, (Some (xloc, s))) : 'or_strs )))));
         ([`List1sep
             ((`Token
                 (((function | `Str _ -> true | _ -> false)),
                   ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")),
               (`Keyword "|"));
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid l");
          `Keyword "as";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid s")],
           ("(xs, (Some (lloc, l)), (Some (xloc, s)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                   match (__fan_4, __fan_2) with
                   | (({ loc = xloc; txt = s;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = l;_}
                                                                : Tokenf.txt))
                       ->
                       ((xs, (Some (lloc, l)), (Some (xloc, s))) : 'or_strs )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (simple : 'simple Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, \"EOI\")), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Empty\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern = None;\n     bounds = [];\n     outer_pattern = None\n   })\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = v;_} : Tokenf.txt) ->
                        (((fun (symbol : Gram_def.symbol)  ->
                             [({ kind = Gram_def.KNormal; symbol } : 
                             Gram_def.psymbol )]))
                           (let pred: FAst.exp =
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
                              `Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "tag")),
                                                 (`Vrn (_loc, v)))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "word")),
                                                 (`Uid (_loc, "Empty")))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "descr"))))) in
                            let des_str = Gram_pat.to_string (`Vrn (_loc, v)) in
                            {
                              text = (`Token (_loc, pred, des, des_str));
                              styp =
                                (`Dot
                                   (_loc, (`Uid (_loc, "Tokenf")),
                                     (`Lid (_loc, "txt"))));
                              pattern = None;
                              bounds = [];
                              outer_pattern = None
                            }) : 'simple )))));
         ([`Keyword "Lid";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text =\n       (`Token\n          (_loc, pred,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"word\")),\n                                (`App\n                                   (_loc, (`Uid (_loc, \"A\")),\n                                     (`Str (_loc, x)))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n            FAst.exp ), des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text =
                               (`Token
                                  (_loc, pred,
                                    (`Constraint
                                       (_loc,
                                         (`Record
                                            (_loc,
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "tag")),
                                                        (`Vrn (_loc, v)))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "word")),
                                                        (`App
                                                           (_loc,
                                                             (`Uid
                                                                (_loc, "A")),
                                                             (`Str (_loc, x)))))))))),
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Tokenf")),
                                              (`Lid (_loc, "descr"))))) : 
                                    FAst.exp ), des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Uid";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text =\n       (`Token\n          (_loc, pred,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"word\")),\n                                (`App\n                                   (_loc, (`Uid (_loc, \"A\")),\n                                     (`Str (_loc, x)))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n            FAst.exp ), des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text =
                               (`Token
                                  (_loc, pred,
                                    (`Constraint
                                       (_loc,
                                         (`Record
                                            (_loc,
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "tag")),
                                                        (`Vrn (_loc, v)))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "word")),
                                                        (`App
                                                           (_loc,
                                                             (`Uid
                                                                (_loc, "A")),
                                                             (`Str (_loc, x)))))))))),
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Tokenf")),
                                              (`Lid (_loc, "descr"))))) : 
                                    FAst.exp ), des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Str";
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "`Str x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"txt\")),\n                                            (`Str (_loc, x)))), (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"txt\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\n   {\n     text =\n       (`Token\n          (_loc, pred,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"word\")),\n                                (`App\n                                   (_loc, (`Uid (_loc, \"A\")),\n                                     (`Str (_loc, x)))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) : \n            FAst.exp ), des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Str (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                           {
                             text =
                               (`Token
                                  (_loc, pred,
                                    (`Constraint
                                       (_loc,
                                         (`Record
                                            (_loc,
                                              (`Sem
                                                 (_loc,
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "tag")),
                                                        (`Vrn (_loc, v)))),
                                                   (`RecBind
                                                      (_loc,
                                                        (`Lid (_loc, "word")),
                                                        (`App
                                                           (_loc,
                                                             (`Uid
                                                                (_loc, "A")),
                                                             (`Str (_loc, x)))))))))),
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Tokenf")),
                                              (`Lid (_loc, "descr"))))) : 
                                    FAst.exp ), des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Lid"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Uid"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int32"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int64"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Nativeint"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Flo"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Chr"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Label"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Optlabel"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Str"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Pre"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = v;_} : Tokenf.txt) ->
                       let xloc = None and x = None in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Lid";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Uid";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int32";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Int64";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Nativeint";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Flo";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Chr";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Label";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Optlabel";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Str";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Pre";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = v in\n   let (pattern,bounds) =\n     match (x, xloc) with\n     | (Some x,Some xloc) ->\n         ((Some\n             (`Constraint\n                (xloc,\n                  (`Record\n                     (xloc,\n                       (`Sem\n                          (xloc,\n                            (`RecBind\n                               (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                            (`Any xloc))))),\n                  (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n             FAst.pat )), [(xloc, x)])\n     | _ -> (None, []) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     pattern;\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       let xloc = Some xloc and x = Some x in
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = v in
                           let (pattern,bounds) =
                             match (x, xloc) with
                             | (Some x,Some xloc) ->
                                 ((Some
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
                                               (`Lid (xloc, "txt"))))) : 
                                     FAst.pat )), [(xloc, x)])
                             | _ -> (None, []) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             pattern;
                             bounds;
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [(xloc, x); (lloc, loc)];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [(xloc, x); (lloc, loc)];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [(xloc, x); (lloc, loc)];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [(xloc, x); (lloc, loc)];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [(xloc, x); (lloc, loc)];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [(xloc, x); (lloc, loc)];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid loc");
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds = [(xloc, x); (lloc, loc)];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, loc)))),\n                         (`Sem\n                            (xloc,\n                              (`RecBind\n                                 (xloc, (`Lid (xloc, \"txt\")),\n                                   (`Lid (xloc, x)))), (`Any xloc))))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_3, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({ loc = lloc;
                                                                 txt = loc;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App
                                  (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "txt"))));
                             bounds = [(xloc, x); (lloc, loc)];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "txt"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Quot";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n     bounds = [(loc, x)];\n     pattern =\n       (Some\n          (`Constraint\n             (_loc, (`Lid (_loc, x)),\n               (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                         Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "quot"))));
                             bounds = [(loc, x)];
                             pattern =
                               (Some
                                  (`Constraint
                                     (_loc, (`Lid (_loc, x)),
                                       (`Dot
                                          (_loc, (`Uid (_loc, "Tokenf")),
                                            (`Lid (_loc, "quot"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "DirQuotation";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")), (`Uid (_loc, \"Any\")))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str =\n     Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n     bounds = [(loc, x)];\n     pattern =\n       (Some\n          (`Constraint\n             (_loc, (`Lid (_loc, x)),\n               (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_1, __fan_0) with
                   | (({ loc; txt = x;_} : Tokenf.txt),({ txt = v;_} :
                                                         Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`Uid (_loc, "Any")))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str =
                             Gram_pat.to_string
                               (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "quot"))));
                             bounds = [(loc, x)];
                             pattern =
                               (Some
                                  (`Constraint
                                     (_loc, (`Lid (_loc, x)),
                                       (`Dot
                                          (_loc, (`Uid (_loc, "Tokenf")),
                                            (`Lid (_loc, "quot"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Inf";
          `Keyword "(";
          `Token
            (((function | `Int _ -> true | _ -> false)),
              ({ tag = `Int; word = Any } : Tokenf.descr ), "Int");
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x");
          `Keyword ")"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"level\")),\n                                            (`Int (_loc, level)))),\n                                       (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"op\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")),\n                        (`App\n                           (_loc, (`Uid (_loc, \"Level\")),\n                             (`Int (_loc, level)))))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = \"Precedence\" ^ level in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n     bounds = [(xloc, x)];\n     pattern =\n       (Some\n          (`Constraint\n             (xloc,\n               (`Record\n                  (xloc,\n                    (`Sem\n                       (xloc,\n                         (`RecBind\n                            (xloc, (`Lid (xloc, \"txt\")), (`Lid (xloc, x)))),\n                         (`Any xloc))))),\n               (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"op\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_4, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({
                                                                 txt = level;_}
                                                                : Tokenf.txt),
                      ({ txt = v;_} : Tokenf.txt)) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                                                                    "level")),
                                                                    (
                                                                    `Int
                                                                    (_loc,
                                                                    level)))),
                                                               (`Any _loc))))),
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc, "Tokenf")),
                                                          (`Lid (_loc, "op")))))))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`App
                                                   (_loc,
                                                     (`Uid (_loc, "Level")),
                                                     (`Int (_loc, level)))))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = "Precedence" ^ level in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "op"))));
                             bounds = [(xloc, x)];
                             pattern =
                               (Some
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
                                            (`Lid (xloc, "op"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Keyword "Inf";
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid l");
          `Keyword "(";
          `Token
            (((function | `Int _ -> true | _ -> false)),
              ({ tag = `Int; word = Any } : Tokenf.descr ), "Int");
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x");
          `Keyword ")"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )])\n  (let pred: FAst.exp =\n     `Fun\n       (_loc,\n         (`Bar\n            (_loc,\n              (`Case\n                 (_loc,\n                   (`App\n                      (_loc, (`Vrn (_loc, v)),\n                        (`Constraint\n                           (_loc,\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"level\")),\n                                            (`Int (_loc, level)))),\n                                       (`Any _loc))))),\n                             (`Dot\n                                (_loc, (`Uid (_loc, \"Tokenf\")),\n                                  (`Lid (_loc, \"op\")))))))),\n                   (`Lid (_loc, \"true\")))),\n              (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n   let des: FAst.exp =\n     `Constraint\n       (_loc,\n         (`Record\n            (_loc,\n              (`Sem\n                 (_loc,\n                   (`RecBind (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"word\")),\n                        (`App\n                           (_loc, (`Uid (_loc, \"Level\")),\n                             (`Int (_loc, level)))))))))),\n         (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n   let des_str = \"Precedence\" ^ level in\n   let p: FAst.pat = `Lid (xloc, x) in\n   let lp: FAst.pat = `Lid (lloc, l) in\n   {\n     text = (`Token (_loc, pred, des, des_str));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n     bounds = [(xloc, x)];\n     pattern =\n       (Some\n          (`Constraint\n             (_loc,\n               (`Record\n                  (_loc,\n                    (`Sem\n                       (_loc, (`RecBind (_loc, (`Lid (_loc, \"loc\")), lp)),\n                         (`Sem\n                            (_loc,\n                              (`RecBind (_loc, (`Lid (_loc, \"txt\")), p)),\n                              (`Any _loc))))))),\n               (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))))) : \n          FAst.pat ));\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.txt)  ~__fan_5:_ 
                   ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_6, __fan_4, __fan_2, __fan_0) with
                   | (({ loc = xloc; txt = x;_} : Tokenf.txt),({
                                                                 txt = level;_}
                                                                : Tokenf.txt),
                      ({ loc = lloc; txt = l;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = Gram_def.KNormal; symbol } : Gram_def.psymbol )]))
                          (let pred: FAst.exp =
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
                                                                    "level")),
                                                                    (
                                                                    `Int
                                                                    (_loc,
                                                                    level)))),
                                                               (`Any _loc))))),
                                                     (`Dot
                                                        (_loc,
                                                          (`Uid
                                                             (_loc, "Tokenf")),
                                                          (`Lid (_loc, "op")))))))),
                                           (`Lid (_loc, "true")))),
                                      (`Case
                                         (_loc, (`Any _loc),
                                           (`Lid (_loc, "false"))))))) in
                           let des: FAst.exp =
                             `Constraint
                               (_loc,
                                 (`Record
                                    (_loc,
                                      (`Sem
                                         (_loc,
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "tag")),
                                                (`Vrn (_loc, v)))),
                                           (`RecBind
                                              (_loc, (`Lid (_loc, "word")),
                                                (`App
                                                   (_loc,
                                                     (`Uid (_loc, "Level")),
                                                     (`Int (_loc, level)))))))))),
                                 (`Dot
                                    (_loc, (`Uid (_loc, "Tokenf")),
                                      (`Lid (_loc, "descr"))))) in
                           let des_str = "Precedence" ^ level in
                           let p: FAst.pat = `Lid (xloc, x) in
                           let lp: FAst.pat = `Lid (lloc, l) in
                           {
                             text = (`Token (_loc, pred, des, des_str));
                             styp =
                               (`Dot
                                  (_loc, (`Uid (_loc, "Tokenf")),
                                    (`Lid (_loc, "op"))));
                             bounds = [(xloc, x)];
                             pattern =
                               (Some
                                  (`Constraint
                                     (_loc,
                                       (`Record
                                          (_loc,
                                            (`Sem
                                               (_loc,
                                                 (`RecBind
                                                    (_loc,
                                                      (`Lid (_loc, "loc")),
                                                      lp)),
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "txt")),
                                                           p)), (`Any _loc))))))),
                                       (`Dot
                                          (_loc, (`Uid (_loc, "Tokenf")),
                                            (`Lid (_loc, "op"))))) : 
                                  FAst.pat ));
                             outer_pattern = None
                           }) : 'simple )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    pattern = None;\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = s;_} : Tokenf.txt) ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = KNormal; symbol } : Gram_def.psymbol )]))
                          {
                            text = (`Keyword (_loc, s));
                            styp =
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "txt"))));
                            pattern = None;
                            bounds = [];
                            outer_pattern = None
                          } : 'simple )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str");
          `Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid i")],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    pattern =\n      (Some\n         (`Constraint\n            (xloc,\n              (`Record\n                 (xloc,\n                   (`Sem\n                      (xloc,\n                        (`RecBind\n                           (xloc, (`Lid (xloc, \"loc\")), (`Lid (xloc, i)))),\n                        (`Any xloc))))),\n              (`Dot (xloc, (`Uid (xloc, \"Tokenf\")), (`Lid (xloc, \"txt\"))))) : \n         FAst.pat ));\n    bounds = [(xloc, i)];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (({ loc = xloc; txt = i;_} : Tokenf.txt),({ txt = s;_} :
                                                                Tokenf.txt))
                       ->
                       (((fun (symbol : Gram_def.symbol)  ->
                            [({ kind = KNormal; symbol } : Gram_def.psymbol )]))
                          {
                            text = (`Keyword (_loc, s));
                            styp =
                              (`Dot
                                 (_loc, (`Uid (_loc, "Tokenf")),
                                   (`Lid (_loc, "txt"))));
                            pattern =
                              (Some
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
                                           (`Lid (xloc, "txt"))))) : 
                                 FAst.pat ));
                            bounds = [(xloc, i)];
                            outer_pattern = None
                          } : 'simple )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Nterm (_loc, n, lev));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    pattern = None;\n    outer_pattern = None\n  }\n",
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
                        bounds = [];
                        pattern = None;
                        outer_pattern = None
                      } : 'simple )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Nterm (Gramf.obj (level_str : 'level_str Gramf.t ))],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Nterm (_loc, n, lev));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    pattern = None;\n    outer_pattern = None\n  }\n",
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
                        bounds = [];
                        pattern = None;
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "S"],
           ("(fun (symbol : Gram_def.symbol)  ->\n   [({ kind = KNormal; symbol } : Gram_def.psymbol )])\n  {\n    text = (`Self _loc);\n    styp = (`Self _loc);\n    pattern = None;\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ((fun (symbol : Gram_def.symbol)  ->
                       [({ kind = KNormal; symbol } : Gram_def.psymbol )])
                      {
                        text = (`Self _loc);
                        styp = (`Self _loc);
                        pattern = None;
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Ant";
          `Keyword "(";
          `Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
          `Keyword ",";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid s");
          `Keyword ")"],
           ("let p = `Lid (xloc, s) in\nmatch ps with\n| (vs,loc,y) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let (x,xloc) = ((x.txt), (x.loc)) in\n            let z = `Str (xloc, x) in\n            let pred: FAst.exp =\n              `Fun\n                (_loc,\n                  (`Bar\n                     (_loc,\n                       (`Case\n                          (_loc,\n                            (`App\n                               (_loc, (`Vrn (_loc, v)),\n                                 (`Constraint\n                                    (_loc,\n                                      (`Record\n                                         (_loc,\n                                           (`Sem\n                                              (_loc,\n                                                (`RecBind\n                                                   (_loc,\n                                                     (`Lid (_loc, \"kind\")),\n                                                     z)), (`Any _loc))))),\n                                      (`Dot\n                                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                                           (`Lid (_loc, \"ant\")))))))),\n                            (`Lid (_loc, \"true\")))),\n                       (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\n            let des: FAst.exp =\n              `Constraint\n                (_loc,\n                  (`Record\n                     (_loc,\n                       (`Sem\n                          (_loc,\n                            (`RecBind\n                               (_loc, (`Lid (_loc, \"tag\")), (`Vrn (_loc, v)))),\n                            (`RecBind\n                               (_loc, (`Lid (_loc, \"word\")),\n                                 (`App (_loc, (`Uid (_loc, \"A\")), z)))))))),\n                  (`Dot\n                     (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"descr\"))))) in\n            let des_str =\n              Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), p)) in\n            let (pattern,bounds) =\n              match (loc, y) with\n              | (None ,None ) ->\n                  ((Some\n                      (`Constraint\n                         (_loc,\n                           (`Alias\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"kind\")), z)),\n                                          (`Any _loc))))), p)),\n                           (`Dot\n                              (_loc, (`Uid (_loc, \"Tokenf\")),\n                                (`Lid (_loc, \"ant\"))))) : FAst.pat )), [])\n              | (Some (lloc,ll),None ) ->\n                  let l: FAst.pat = `Lid (lloc, ll) in\n                  ((Some\n                      (`Constraint\n                         (_loc,\n                           (`Alias\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"kind\")), z)),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"loc\")), l)),\n                                               (`Any _loc))))))), p)),\n                           (`Dot\n                              (_loc, (`Uid (_loc, \"Tokenf\")),\n                                (`Lid (_loc, \"ant\"))))) : FAst.pat )),\n                    [(lloc, ll)])\n              | (None ,Some ((xloc,u) as v)) ->\n                  ((Some\n                      (`Constraint\n                         (xloc,\n                           (`Alias\n                              (xloc,\n                                (`Record\n                                   (xloc,\n                                     (`Sem\n                                        (xloc,\n                                          (`RecBind\n                                             (xloc, (`Lid (xloc, \"kind\")),\n                                               (`Alias\n                                                  (xloc, z, (`Lid (xloc, u)))))),\n                                          (`Any xloc))))), p)),\n                           (`Dot\n                              (xloc, (`Uid (xloc, \"Tokenf\")),\n                                (`Lid (xloc, \"ant\"))))) : FAst.pat )), \n                    [v])\n              | (Some (lloc,ll),Some ((xloc,u) as v)) ->\n                  let l: FAst.pat = `Lid (lloc, ll) in\n                  ((Some\n                      (`Constraint\n                         (xloc,\n                           (`Alias\n                              (xloc,\n                                (`Record\n                                   (xloc,\n                                     (`Sem\n                                        (xloc,\n                                          (`RecBind\n                                             (xloc, (`Lid (xloc, \"kind\")),\n                                               (`Alias\n                                                  (xloc, z, (`Lid (xloc, u)))))),\n                                          (`Sem\n                                             (xloc,\n                                               (`RecBind\n                                                  (xloc,\n                                                    (`Lid (xloc, \"loc\")), l)),\n                                               (`Any xloc))))))), p)),\n                           (`Dot\n                              (xloc, (`Uid (xloc, \"Tokenf\")),\n                                (`Lid (xloc, \"ant\"))))) : FAst.pat )),\n                    [(lloc, ll); v]) in\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Token (_loc, pred, des, des_str));\n                   styp =\n                     (`Dot\n                        (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"ant\"))));\n                   pattern;\n                   bounds;\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(ps : 'or_strs)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_4, __fan_0) with
                   | (({ loc = xloc; txt = s;_} : Tokenf.txt),({ txt = v;_} :
                                                                Tokenf.txt))
                       ->
                       (let p = `Lid (xloc, s) in
                        (match ps with
                         | (vs,loc,y) ->
                             vs |>
                               (List.map
                                  (fun (x : Tokenf.txt)  ->
                                     let (x,xloc) = ((x.txt), (x.loc)) in
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
                                       `Constraint
                                         (_loc,
                                           (`Record
                                              (_loc,
                                                (`Sem
                                                   (_loc,
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid (_loc, "tag")),
                                                          (`Vrn (_loc, v)))),
                                                     (`RecBind
                                                        (_loc,
                                                          (`Lid
                                                             (_loc, "word")),
                                                          (`App
                                                             (_loc,
                                                               (`Uid
                                                                  (_loc, "A")),
                                                               z)))))))),
                                           (`Dot
                                              (_loc, (`Uid (_loc, "Tokenf")),
                                                (`Lid (_loc, "descr"))))) in
                                     let des_str =
                                       Gram_pat.to_string
                                         (`App (_loc, (`Vrn (_loc, v)), p)) in
                                     let (pattern,bounds) =
                                       match (loc, y) with
                                       | (None ,None ) ->
                                           ((Some
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
                                                                    z)),
                                                                   (`Any _loc))))),
                                                         p)),
                                                    (`Dot
                                                       (_loc,
                                                         (`Uid
                                                            (_loc, "Tokenf")),
                                                         (`Lid (_loc, "ant"))))) : 
                                               FAst.pat )), [])
                                       | (Some (lloc,ll),None ) ->
                                           let l: FAst.pat = `Lid (lloc, ll) in
                                           ((Some
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
                                                                    z)),
                                                                   (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "loc")),
                                                                    l)),
                                                                    (`Any
                                                                    _loc))))))),
                                                         p)),
                                                    (`Dot
                                                       (_loc,
                                                         (`Uid
                                                            (_loc, "Tokenf")),
                                                         (`Lid (_loc, "ant"))))) : 
                                               FAst.pat )), [(lloc, ll)])
                                       | (None ,Some ((xloc,u) as v)) ->
                                           ((Some
                                               (`Constraint
                                                  (xloc,
                                                    (`Alias
                                                       (xloc,
                                                         (`Record
                                                            (xloc,
                                                              (`Sem
                                                                 (xloc,
                                                                   (`RecBind
                                                                    (xloc,
                                                                    (`Lid
                                                                    (xloc,
                                                                    "kind")),
                                                                    (`Alias
                                                                    (xloc, z,
                                                                    (`Lid
                                                                    (xloc, u)))))),
                                                                   (`Any xloc))))),
                                                         p)),
                                                    (`Dot
                                                       (xloc,
                                                         (`Uid
                                                            (xloc, "Tokenf")),
                                                         (`Lid (xloc, "ant"))))) : 
                                               FAst.pat )), [v])
                                       | (Some (lloc,ll),Some
                                          ((xloc,u) as v)) ->
                                           let l: FAst.pat = `Lid (lloc, ll) in
                                           ((Some
                                               (`Constraint
                                                  (xloc,
                                                    (`Alias
                                                       (xloc,
                                                         (`Record
                                                            (xloc,
                                                              (`Sem
                                                                 (xloc,
                                                                   (`RecBind
                                                                    (xloc,
                                                                    (`Lid
                                                                    (xloc,
                                                                    "kind")),
                                                                    (`Alias
                                                                    (xloc, z,
                                                                    (`Lid
                                                                    (xloc, u)))))),
                                                                   (`Sem
                                                                    (xloc,
                                                                    (`RecBind
                                                                    (xloc,
                                                                    (`Lid
                                                                    (xloc,
                                                                    "loc")),
                                                                    l)),
                                                                    (`Any
                                                                    xloc))))))),
                                                         p)),
                                                    (`Dot
                                                       (xloc,
                                                         (`Uid
                                                            (xloc, "Tokenf")),
                                                         (`Lid (xloc, "ant"))))) : 
                                               FAst.pat )), [(lloc, ll); v]) in
                                     ({
                                        kind = KNormal;
                                        symbol =
                                          {
                                            text =
                                              (`Token
                                                 (_loc, pred, des, des_str));
                                            styp =
                                              (`Dot
                                                 (_loc,
                                                   (`Uid (_loc, "Tokenf")),
                                                   (`Lid (_loc, "ant"))));
                                            pattern;
                                            bounds;
                                            outer_pattern = None
                                          }
                                      } : Gram_def.psymbol )))) : 'simple )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
          `Keyword ")"],
           ("match v with\n| (vs,loc,None ) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let (bounds,pattern) =\n              match loc with\n              | Some (loc,l) ->\n                  ([(loc, l)],\n                    (Some\n                       (`Constraint\n                          (loc,\n                            (`Record\n                               (loc,\n                                 (`Sem\n                                    (loc,\n                                      (`RecBind\n                                         (loc, (`Lid (loc, \"loc\")),\n                                           (`Lid (loc, l)))), (`Any loc))))),\n                            (`Dot\n                               (loc, (`Uid (loc, \"Tokenf\")),\n                                 (`Lid (loc, \"txt\"))))) : FAst.pat )))\n              | None  -> ([], None) in\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Keyword ((x.loc), (x.txt)));\n                   styp =\n                     (`Dot\n                        (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n                   bounds;\n                   pattern;\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n| (vs,loc,Some ((xloc,v) as b)) ->\n    let p: FAst.pat = `Lid (xloc, v) in\n    let (bounds,pattern) =\n      match loc with\n      | None  ->\n          ([b],\n            (Some\n               (`Constraint\n                  (_loc,\n                    (`Record\n                       (_loc,\n                         (`Sem\n                            (_loc,\n                              (`RecBind (_loc, (`Lid (_loc, \"txt\")), p)),\n                              (`Any _loc))))),\n                    (`Dot\n                       (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))))) : \n               FAst.pat )))\n      | Some (loc,l) ->\n          let lp: FAst.pat = `Lid (loc, l) in\n          ([(loc, l); b],\n            (Some\n               (`Constraint\n                  (_loc,\n                    (`Record\n                       (_loc,\n                         (`Sem\n                            (_loc,\n                              (`RecBind (_loc, (`Lid (_loc, \"txt\")), p)),\n                              (`Sem\n                                 (_loc,\n                                   (`RecBind (_loc, (`Lid (_loc, \"loc\")), lp)),\n                                   (`Any _loc))))))),\n                    (`Dot\n                       (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))))) : \n               FAst.pat ))) in\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            ({\n               kind = KNormal;\n               symbol =\n                 {\n                   text = (`Keyword ((x.loc), (x.txt)));\n                   styp =\n                     (`Dot\n                        (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n                   bounds;\n                   pattern;\n                   outer_pattern = None\n                 }\n             } : Gram_def.psymbol )))\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(v : 'or_strs)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (match v with
                    | (vs,loc,None ) ->
                        vs |>
                          (List.map
                             (fun (x : Tokenf.txt)  ->
                                let (bounds,pattern) =
                                  match loc with
                                  | Some (loc,l) ->
                                      ([(loc, l)],
                                        (Some
                                           (`Constraint
                                              (loc,
                                                (`Record
                                                   (loc,
                                                     (`Sem
                                                        (loc,
                                                          (`RecBind
                                                             (loc,
                                                               (`Lid
                                                                  (loc,
                                                                    "loc")),
                                                               (`Lid (loc, l)))),
                                                          (`Any loc))))),
                                                (`Dot
                                                   (loc,
                                                     (`Uid (loc, "Tokenf")),
                                                     (`Lid (loc, "txt"))))) : 
                                           FAst.pat )))
                                  | None  -> ([], None) in
                                ({
                                   kind = KNormal;
                                   symbol =
                                     {
                                       text = (`Keyword ((x.loc), (x.txt)));
                                       styp =
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Tokenf")),
                                              (`Lid (_loc, "txt"))));
                                       bounds;
                                       pattern;
                                       outer_pattern = None
                                     }
                                 } : Gram_def.psymbol )))
                    | (vs,loc,Some ((xloc,v) as b)) ->
                        let p: FAst.pat = `Lid (xloc, v) in
                        let (bounds,pattern) =
                          match loc with
                          | None  ->
                              ([b],
                                (Some
                                   (`Constraint
                                      (_loc,
                                        (`Record
                                           (_loc,
                                             (`Sem
                                                (_loc,
                                                  (`RecBind
                                                     (_loc,
                                                       (`Lid (_loc, "txt")),
                                                       p)), (`Any _loc))))),
                                        (`Dot
                                           (_loc, (`Uid (_loc, "Tokenf")),
                                             (`Lid (_loc, "txt"))))) : 
                                   FAst.pat )))
                          | Some (loc,l) ->
                              let lp: FAst.pat = `Lid (loc, l) in
                              ([(loc, l); b],
                                (Some
                                   (`Constraint
                                      (_loc,
                                        (`Record
                                           (_loc,
                                             (`Sem
                                                (_loc,
                                                  (`RecBind
                                                     (_loc,
                                                       (`Lid (_loc, "txt")),
                                                       p)),
                                                  (`Sem
                                                     (_loc,
                                                       (`RecBind
                                                          (_loc,
                                                            (`Lid
                                                               (_loc, "loc")),
                                                            lp)),
                                                       (`Any _loc))))))),
                                        (`Dot
                                           (_loc, (`Uid (_loc, "Tokenf")),
                                             (`Lid (_loc, "txt"))))) : 
                                   FAst.pat ))) in
                        vs |>
                          (List.map
                             (fun (x : Tokenf.txt)  ->
                                ({
                                   kind = KNormal;
                                   symbol =
                                     {
                                       text = (`Keyword ((x.loc), (x.txt)));
                                       styp =
                                         (`Dot
                                            (_loc, (`Uid (_loc, "Tokenf")),
                                              (`Lid (_loc, "txt"))));
                                       bounds;
                                       pattern;
                                       outer_pattern = None
                                     }
                                 } : Gram_def.psymbol ))) : 'simple )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (level_str : 'level_str Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "Level";
           `Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")],
            ("s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    match __fan_1 with
                    | ({ txt = s;_} : Tokenf.txt) -> (s : 'level_str )))))]) : 
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
            ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None; bounds = [] }\n }]\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'single_symbol) 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = l;_} : Tokenf.txt) ->
                        let sep = None in
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
                                outer_pattern = None;
                                bounds = []
                              }
                          }] : 'symbol )))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None; bounds = [] }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = l;_} : Tokenf.txt) ->
                       let sep = None in
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
                               outer_pattern = None;
                               bounds = []
                             }
                         }] : 'symbol )))));
         ([`Keyword "L0";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None; bounds = [] }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = l;_} : Tokenf.txt) ->
                       let sep = Some sep in
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
                               outer_pattern = None;
                               bounds = []
                             }
                         }] : 'symbol )))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{\n   kind = KNormal;\n   symbol = { text; styp; pattern = None; outer_pattern = None; bounds = [] }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = l;_} : Tokenf.txt) ->
                       let sep = Some sep in
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
                               outer_pattern = None;
                               bounds = []
                             }
                         }] : 'symbol )))));
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
         ([`Keyword "?";
          `Keyword "[";
          `Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ));
          `Keyword "]"],
           ("assert false\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:_  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (assert false : 'symbol )))));
         ([`Keyword "TRY";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   symbol =\n     {\n       text;\n       styp = (s.styp);\n       pattern = None;\n       outer_pattern = None;\n       bounds = (s.bounds)\n     }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = p;_} : Tokenf.txt) ->
                       (let v = (_loc, (s.text)) in
                        let text = if p = "TRY" then `Try v else `Peek v in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp = (s.styp);
                               pattern = None;
                               outer_pattern = None;
                               bounds = (s.bounds)
                             }
                         }] : 'symbol )))));
         ([`Keyword "PEEK";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   symbol =\n     {\n       text;\n       styp = (s.styp);\n       pattern = None;\n       outer_pattern = None;\n       bounds = (s.bounds)\n     }\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = p;_} : Tokenf.txt) ->
                       (let v = (_loc, (s.text)) in
                        let text = if p = "TRY" then `Try v else `Peek v in
                        [{
                           kind = KNormal;
                           symbol =
                             {
                               text;
                               styp = (s.styp);
                               pattern = None;
                               outer_pattern = None;
                               bounds = (s.bounds)
                             }
                         }] : 'symbol )))));
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
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid i")],
           ("List.map\n  (fun (s : Gram_def.psymbol)  ->\n     { s with symbol = { (s.symbol) with outer_pattern = (Some (xloc, i)) } })\n  ss\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(ss : 'symbol)  (_loc : Locf.t)  ->
                   match __fan_2 with
                   | ({ loc = xloc; txt = i;_} : Tokenf.txt) ->
                       (List.map
                          (fun (s : Gram_def.psymbol)  ->
                             {
                               s with
                               symbol =
                                 {
                                   (s.symbol) with
                                   outer_pattern = (Some (xloc, i))
                                 }
                             }) ss : 'psymbol )))))]) : Gramf.olevel ))
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let opt_action: 'opt_action Gramf.t = grammar_entry_create "opt_action" in
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
            ("(fun f  -> f true)\n  (fun safe  ->\n     let (gram,old) = rest in\n     let items = Listf.filter_map (fun x  -> x) el in\n     let res = make _loc { items; gram; safe } in\n     let () = module_name := old in res)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'entry list) 
                    ~__fan_0:(rest : 'extend_header)  (_loc : Locf.t)  ->
                    ((fun f  -> f true)
                       (fun safe  ->
                          let (gram,old) = rest in
                          let items = Listf.filter_map (fun x  -> x) el in
                          let res = make _loc { items; gram; safe } in
                          let () = module_name := old in res) : 'extend_body )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (unsafe_extend_body : 'unsafe_extend_body Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (extend_header : 'extend_header Gramf.t ));
           `List1 (`Nterm (Gramf.obj (entry : 'entry Gramf.t )))],
            ("(fun f  -> f false)\n  (fun safe  ->\n     let (gram,old) = rest in\n     let items = Listf.filter_map (fun x  -> x) el in\n     let res = make _loc { items; gram; safe } in\n     let () = module_name := old in res)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'entry list) 
                    ~__fan_0:(rest : 'extend_header)  (_loc : Locf.t)  ->
                    ((fun f  -> f false)
                       (fun safe  ->
                          let (gram,old) = rest in
                          let items = Listf.filter_map (fun x  -> x) el in
                          let res = make _loc { items; gram; safe } in
                          let () = module_name := old in res) : 'unsafe_extend_body )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)),
                ({ tag = `Uid; word = Any } : Tokenf.descr ), "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)),
               ({ tag = `Uid; word = Any } : Tokenf.descr ), "Uid")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       (`Uid (_loc, x) : 'qualuid )))))]) : Gramf.olevel ));
  Gramf.extend_single (qualid : 'qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)),
                ({ tag = `Uid; word = Any } : Tokenf.descr ), "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )))));
         ([`Token
             (((function | `Lid _ -> true | _ -> false)),
               ({ tag = `Lid; word = Any } : Tokenf.descr ), "Lid")],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = i;_} : Tokenf.txt) ->
                       (`Lid (_loc, i) : 'qualid )))))]) : Gramf.olevel ));
  Gramf.extend_single (t_qualid : 't_qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Uid _ -> true | _ -> false)),
                ({ tag = `Uid; word = Any } : Tokenf.descr ), "Uid");
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 't_qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )))));
         ([`Token
             (((function | `Uid _ -> true | _ -> false)),
               ({ tag = `Uid; word = Any } : Tokenf.descr ), "Uid");
          `Keyword ".";
          `Token
            (((function
               | `Lid ({ txt = "t";_} : Tokenf.txt) -> true
               | _ -> false)),
              ({ tag = `Lid; word = (A "t") } : Tokenf.descr ), "`Lid \"t\"")],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match (__fan_2, __fan_0) with
                   | (({ txt = "t";_} : Tokenf.txt),({ txt = x;_} :
                                                      Tokenf.txt))
                       -> (`Uid (_loc, x) : 't_qualid )
                   | _ -> assert false))))]) : Gramf.olevel ));
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
            ("let x =\n  match (name : Tokenf.txt option ) with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), (x.txt)) with\n       | None  -> Locf.failf x.loc \"lang `%s' not resolved\" x.txt\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(il : 'qualid)  (_loc : Locf.t)  ->
                    let name = None in
                    (let x =
                       match (name : Tokenf.txt option ) with
                       | Some x ->
                           let old = !Ast_quotation.default in
                           (match Ast_quotation.resolve_name
                                    ((`Sub []), (x.txt))
                            with
                            | None  ->
                                Locf.failf x.loc "lang `%s' not resolved"
                                  x.txt
                            | Some x ->
                                (Ast_quotation.default := (Some x); `name old))
                       | None  -> `non in
                     (x, (mk_name il)) : 'entry_name )))));
         ([`Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
          `Token
            (((function | `Str _ -> true | _ -> false)),
              ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")],
           ("let x =\n  match (name : Tokenf.txt option ) with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), (x.txt)) with\n       | None  -> Locf.failf x.loc \"lang `%s' not resolved\" x.txt\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(name : Tokenf.txt)  ~__fan_0:(il : 'qualid) 
                   (_loc : Locf.t)  ->
                   let name = Some name in
                   (let x =
                      match (name : Tokenf.txt option ) with
                      | Some x ->
                          let old = !Ast_quotation.default in
                          (match Ast_quotation.resolve_name
                                   ((`Sub []), (x.txt))
                           with
                           | None  ->
                               Locf.failf x.loc "lang `%s' not resolved"
                                 x.txt
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
         ([`Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
          `Keyword "@";
          `Keyword "Local";
          `Keyword ":";
          `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = true; pos; levels })\n",
             (Gramf.mk_action
                (fun ~__fan_4:(levels : 'level_list)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:_  ~__fan_0:(rest : 'entry_name)  (_loc : Locf.t)
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
         ([`Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
          `Keyword "@";
          `Keyword "Local";
          `Keyword ":";
          `Nterm (Gramf.obj (position : 'position Gramf.t ));
          `Nterm (Gramf.obj (level_list : 'level_list Gramf.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some { name = p; local = true; pos; levels })\n",
             (Gramf.mk_action
                (fun ~__fan_5:(levels : 'level_list) 
                   ~__fan_4:(pos : 'position)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:_  ~__fan_0:(rest : 'entry_name)  (_loc : Locf.t)
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
         ([`Token
             (((function | `Lid _ -> true | _ -> false)),
               ({ tag = `Lid; word = Any } : Tokenf.descr ), "Lid");
          `Keyword "@";
          `Keyword "Inline";
          `Keyword ":";
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("Hashtbl.add inline_rules x rules; None\n",
             (Gramf.mk_action
                (fun ~__fan_4:(rules : 'rule_list)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((Hashtbl.add inline_rules x rules; None) : 'entry )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (position : 'position Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "First"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Last"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Before"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "After"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Level"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'position )))))]) : 
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
                    let assoc = None in
                    let label = None in ({ label; assoc; rules } : 'level )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str");
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = label;_} : Tokenf.txt) ->
                       let assoc = None in
                       let label = Some label in
                       ({ label; assoc; rules } : 'level )))));
         ([`Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list)  ~__fan_0:(assoc : 'assoc)
                    (_loc : Locf.t)  ->
                   let assoc = Some assoc in
                   let label = None in ({ label; assoc; rules } : 'level )))));
         ([`Token
             (((function | `Str _ -> true | _ -> false)),
               ({ tag = `Str; word = Any } : Tokenf.descr ), "Str");
          `Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(rules : 'rule_list)  ~__fan_1:(assoc : 'assoc)
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = label;_} : Tokenf.txt) ->
                       let assoc = Some assoc in
                       let label = Some label in
                       ({ label; assoc; rules } : 'level )))))]) : Gramf.olevel ));
  Gramf.extend_single (assoc : 'assoc Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "LA"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = x;_} : Tokenf.txt) ->
                        ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))));
         ([`Keyword "RA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))));
         ([`Keyword "NA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | ({ txt = x;_} : Tokenf.txt) ->
                       ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))))]) : 
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
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x")],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | ({ loc = xloc; txt = x;_} : Tokenf.txt) ->
                       let action = None in
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
                                      }) rules) : 'rule )))));
         ([`Keyword "@";
          `Token
            (((function | `Lid _ -> true | _ -> false)),
              ({ tag = `Lid; word = Any } : Tokenf.descr ), "`Lid x");
          `Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_2:(action : 'opt_action) 
                   ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   match __fan_1 with
                   | ({ loc = xloc; txt = x;_} : Tokenf.txt) ->
                       let action = Some action in
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
                                      }) rules) : 'rule )))))]) : Gramf.olevel ));
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
                ({ tag = `Quot; word = Any } : Tokenf.descr ), "`Quot _")],
            ("if x.name = Tokenf.empty_name\nthen let expander loc _ s = Parsef.exp loc s in Tokenf.quot_expand expander x\nelse Ast_quotation.expand x Dyn_tag.exp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | (x : Tokenf.quot) ->
                        (if x.name = Tokenf.empty_name
                         then
                           let expander loc _ s = Parsef.exp loc s in
                           Tokenf.quot_expand expander x
                         else Ast_quotation.expand x Dyn_tag.exp : 'opt_action )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (string : 'string Gramf.t )
    (None,
      ((None, None,
         [([`Token
              (((function | `Str _ -> true | _ -> false)),
                ({ tag = `Str; word = Any } : Tokenf.descr ), "Str")],
            ("(`Str (_loc, s) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    match __fan_0 with
                    | ({ txt = s;_} : Tokenf.txt) ->
                        ((`Str (_loc, s) : FAst.exp ) : 'string )))));
         ([`Token
             (((function
                | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                | _ -> false)),
               ({ tag = `Ant; word = (A "") } : Tokenf.descr ), "`Ant s")],
           ("Tokenf.ant_expand Parsef.exp s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | (({ kind = "";_} as s) : Tokenf.ant) ->
                       (Tokenf.ant_expand Parsef.exp s : 'string )
                   | _ -> assert false))))]) : Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "extend")
    ~entry:extend_body ();
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "unsafe_extend")
    ~entry:unsafe_extend_body ()
