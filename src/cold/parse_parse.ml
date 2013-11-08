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
type matrix = Gram_def.osymbol list Gram_def.decorate list 
let extend_header = Gramf.mk_dynamic g "extend_header"
let left_rule: matrix list Gramf.t = Gramf.mk_dynamic g "left_rule"
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
let symbol: matrix Gramf.t = Gramf.mk_dynamic g "symbol"
let rule = Gramf.mk_dynamic g "rule"
let meta_rule = Gramf.mk_dynamic g "meta_rule"
let rule_list = Gramf.mk_dynamic g "rule_list"
let psymbol: matrix Gramf.t = Gramf.mk_dynamic g "psymbol"
let level = Gramf.mk_dynamic g "level"
let level_list = Gramf.mk_dynamic g "level_list"
let entry: Gram_def.entry option Gramf.t = Gramf.mk_dynamic g "entry"
let extend_body = Gramf.mk_dynamic g "extend_body"
let unsafe_extend_body = Gramf.mk_dynamic g "unsafe_extend_body"
let simple: matrix Gramf.t = Gramf.mk_dynamic g "simple"
let single_symbol: Gram_def.osymbol Gramf.t =
  Gramf.mk_dynamic g "single_symbol"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let or_strs: 'or_strs Gramf.t = grammar_entry_create "or_strs"
  and sep_symbol: 'sep_symbol Gramf.t = grammar_entry_create "sep_symbol" in
  Gramf.extend_single (single_symbol : 'single_symbol Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`Constraint\n                                               (_loc,\n                                                 (`App\n                                                    (_loc, (`Vrn (_loc, v)),\n                                                      (`Any _loc))),\n                                                 (`Dot\n                                                    (_loc,\n                                                      (`Uid (_loc, \"Tokenf\")),\n                                                      (`Lid (_loc, \"t\")))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Empty\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let v = __fan_0.txt in
                    ({
                       text =
                         (`Token
                            (_loc,
                              (`Constraint
                                 (_loc,
                                   (`Record
                                      (_loc,
                                        (`Sem
                                           (_loc,
                                             (`RecBind
                                                (_loc, (`Lid (_loc, "pred")),
                                                  (`Fun
                                                     (_loc,
                                                       (`Bar
                                                          (_loc,
                                                            (`Case
                                                               (_loc,
                                                                 (`Constraint
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "t")))))),
                                                                 (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                            (`Case
                                                               (_loc,
                                                                 (`Any _loc),
                                                                 (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                             (`RecBind
                                                (_loc,
                                                  (`Lid (_loc, "descr")),
                                                  (`Record
                                                     (_loc,
                                                       (`Sem
                                                          (_loc,
                                                            (`RecBind
                                                               (_loc,
                                                                 (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                 (`Vrn
                                                                    (_loc, v)))),
                                                            (`Sem
                                                               (_loc,
                                                                 (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Empty")))),
                                                                 (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                   (`Dot
                                      (_loc, (`Uid (_loc, "Tokenf")),
                                        (`Lid (_loc, "pattern"))))) : 
                              FAst.exp )));
                       styp =
                         (`Dot
                            (_loc, (`Uid (_loc, "Tokenf")),
                              (`Lid (_loc, "txt"))));
                       bounds = [];
                       outer_pattern = None
                     } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`App\n                                                    (_loc,\n                                                      (`Uid (_loc, \"A\")),\n                                                      (`Str (_loc, x)))))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`App\n                                                    (_loc,\n                                                      (`Uid (_loc, \"A\")),\n                                                      (`Str (_loc, x)))))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Str";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`App\n                                                    (_loc,\n                                                      (`Uid (_loc, \"A\")),\n                                                      (`Str (_loc, x)))))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Lid"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Uid"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int32"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int64"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Nativeint"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Flo"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Chr"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Label"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Optlabel"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Str"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Pre"],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int32";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Int64";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Nativeint";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Flo";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Chr";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Label";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Optlabel";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Str";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Pre";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds;
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        [((lloc, loc), (Some "loc"));
                        ((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        [((lloc, loc), (Some "loc"));
                        ((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        [((lloc, loc), (Some "loc"));
                        ((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        [((lloc, loc), (Some "loc"));
                        ((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [((lloc, loc), (Some "loc"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [((lloc, loc), (Some "loc"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [((lloc, loc), (Some "loc"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid\n                                                              (_loc, \"txt\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [((lloc, loc), (Some "loc"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Quot";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [((loc, x), None)];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "quot"))));
                      bounds = [((loc, x), None)];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "DirQuotation";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Any _loc))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`Uid (_loc, \"Any\")))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [((loc, x), None)];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "quot"))));
                      bounds = [((loc, x), None)];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Inf";
          `Keyword "(";
          `Token
            ({
               pred = ((function | `Int _ -> true | _ -> false));
               descr = { tag = `Int; word = Any; tag_name = "Int" }
             } : Tokenf.pattern );
          `Keyword ",";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"level\")),\n                                                                    (`Int\n                                                                    (_loc,\n                                                                    level)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid (_loc, \"op\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`App\n                                                    (_loc,\n                                                      (`Uid (_loc, \"Level\")),\n                                                      (`Int (_loc, level)))))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds = [((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let level = __fan_2.txt in
                   let xloc = __fan_4.loc in
                   let x = __fan_4.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "op")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Level")),
                                                                    (`Int
                                                                    (_loc,
                                                                    level)))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds = [((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "Inf";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword "(";
          `Token
            ({
               pred = ((function | `Int _ -> true | _ -> false));
               descr = { tag = `Int; word = Any; tag_name = "Int" }
             } : Tokenf.pattern );
          `Keyword ",";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("{\n  text =\n    (`Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`Sem\n                      (_loc,\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"pred\")),\n                             (`Fun\n                                (_loc,\n                                  (`Bar\n                                     (_loc,\n                                       (`Case\n                                          (_loc,\n                                            (`App\n                                               (_loc, (`Vrn (_loc, v)),\n                                                 (`Constraint\n                                                    (_loc,\n                                                      (`Record\n                                                         (_loc,\n                                                           (`Sem\n                                                              (_loc,\n                                                                (`RecBind\n                                                                   (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"level\")),\n                                                                    (`Int\n                                                                    (_loc,\n                                                                    level)))),\n                                                                (`Any _loc))))),\n                                                      (`Dot\n                                                         (_loc,\n                                                           (`Uid\n                                                              (_loc,\n                                                                \"Tokenf\")),\n                                                           (`Lid (_loc, \"op\")))))))),\n                                            (`Lid (_loc, \"true\")))),\n                                       (`Case\n                                          (_loc, (`Any _loc),\n                                            (`Lid (_loc, \"false\")))))))))),\n                        (`RecBind\n                           (_loc, (`Lid (_loc, \"descr\")),\n                             (`Record\n                                (_loc,\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag\")),\n                                            (`Vrn (_loc, v)))),\n                                       (`Sem\n                                          (_loc,\n                                            (`RecBind\n                                               (_loc, (`Lid (_loc, \"word\")),\n                                                 (`App\n                                                    (_loc,\n                                                      (`Uid (_loc, \"Level\")),\n                                                      (`Int (_loc, level)))))),\n                                            (`RecBind\n                                               (_loc,\n                                                 (`Lid (_loc, \"tag_name\")),\n                                                 (`Str (_loc, v)))))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds = [((lloc, l), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.txt)  ~__fan_5:_ 
                   ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let level = __fan_4.txt in
                   let xloc = __fan_6.loc in
                   let x = __fan_6.txt in
                   ({
                      text =
                        (`Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`Sem
                                          (_loc,
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "pred")),
                                                 (`Fun
                                                    (_loc,
                                                      (`Bar
                                                         (_loc,
                                                           (`Case
                                                              (_loc,
                                                                (`App
                                                                   (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "op")))))))),
                                                                (`Lid
                                                                   (_loc,
                                                                    "true")))),
                                                           (`Case
                                                              (_loc,
                                                                (`Any _loc),
                                                                (`Lid
                                                                   (_loc,
                                                                    "false")))))))))),
                                            (`RecBind
                                               (_loc, (`Lid (_loc, "descr")),
                                                 (`Record
                                                    (_loc,
                                                      (`Sem
                                                         (_loc,
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag")),
                                                                (`Vrn
                                                                   (_loc, v)))),
                                                           (`Sem
                                                              (_loc,
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Level")),
                                                                    (`Int
                                                                    (_loc,
                                                                    level)))))),
                                                                (`RecBind
                                                                   (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds =
                        [((lloc, l), (Some "loc"));
                        ((xloc, x), (Some "txt"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern )],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   ({
                      text = (`Keyword (_loc, s));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern );
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("{\n  text = (`Keyword (_loc, s));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((xloc, i), (Some \"loc\"))];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   ({
                      text = (`Keyword (_loc, s));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds = [((xloc, i), (Some "loc"))];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("{\n  text = (`Nterm (_loc, n, s));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let s = None in
                   ({
                      text = (`Nterm (_loc, n, s));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Keyword "Level";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("{\n  text = (`Nterm (_loc, n, s));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let s = __fan_2.txt in
                   let s = Some s in
                   ({
                      text = (`Nterm (_loc, n, s));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))));
         ([`Keyword "S"],
           ("{ text = (`Self _loc); styp = (`Self _loc); bounds = []; outer_pattern = None\n}\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ({
                      text = (`Self _loc);
                      styp = (`Self _loc);
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol )))))]) : Gramf.olevel ));
  Gramf.extend_single (or_strs : 'or_strs Gramf.t )
    (None,
      ((None, None,
         [([`List1sep
              ((`Token
                  ({
                     pred = ((function | `Str _ -> true | _ -> false));
                     descr = { tag = `Str; word = Any; tag_name = "Str" }
                   } : Tokenf.pattern )), (`Keyword "|"))],
            ("(xs, None, None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                    ((xs, None, None) : 'or_strs )))));
         ([`List1sep
             ((`Token
                 ({
                    pred = ((function | `Str _ -> true | _ -> false));
                    descr = { tag = `Str; word = Any; tag_name = "Str" }
                  } : Tokenf.pattern )), (`Keyword "|"));
          `Keyword "as";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(xs, None, (Some (xloc, s)))\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                   let xloc = __fan_2.loc in
                   let s = __fan_2.txt in
                   ((xs, None, (Some (xloc, s))) : 'or_strs )))));
         ([`List1sep
             ((`Token
                 ({
                    pred = ((function | `Str _ -> true | _ -> false));
                    descr = { tag = `Str; word = Any; tag_name = "Str" }
                  } : Tokenf.pattern )), (`Keyword "|"));
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword "as";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(xs, (Some (lloc, l)), (Some (xloc, s)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let xloc = __fan_4.loc in
                   let s = __fan_4.txt in
                   ((xs, (Some (lloc, l)), (Some (xloc, s))) : 'or_strs )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (simple : 'simple Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "EOI"],
            ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`Constraint\n                                                 (_loc,\n                                                   (`App\n                                                      (_loc,\n                                                        (`Vrn (_loc, v)),\n                                                        (`Any _loc))),\n                                                   (`Dot\n                                                      (_loc,\n                                                        (`Uid\n                                                           (_loc, \"Tokenf\")),\n                                                        (`Lid (_loc, \"t\")))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Empty\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let v = __fan_0.txt in
                    ((fun (txt : Gram_def.osymbol)  ->
                        [({ kind = Gram_def.KNormal; txt = [txt] } : 
                        Gram_def.osymbol list Gram_def.decorate )])
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`Constraint
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "t")))))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Empty")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds = [];
                         outer_pattern = None
                       } : 'simple )))));
         ([`Keyword "Lid";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`App\n                                                      (_loc,\n                                                        (`Uid (_loc, \"A\")),\n                                                        (`Str (_loc, x)))))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Uid";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`App\n                                                      (_loc,\n                                                        (`Uid (_loc, \"A\")),\n                                                        (`Str (_loc, x)))))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Str";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`App\n                                                      (_loc,\n                                                        (`Uid (_loc, \"A\")),\n                                                        (`Str (_loc, x)))))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc, x)))))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Lid"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Uid"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int32"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int64"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Nativeint"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Flo"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Chr"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Label"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Optlabel"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Str"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Pre"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Lid";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Uid";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int32";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Int64";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Nativeint";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Flo";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Chr";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Label";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Optlabel";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Str";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Pre";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (`Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`Sem\n                         (_loc,\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"pred\")),\n                                (`Fun\n                                   (_loc,\n                                     (`Bar\n                                        (_loc,\n                                          (`Case\n                                             (_loc,\n                                               (`App\n                                                  (_loc, (`Vrn (_loc, v)),\n                                                    (`Any _loc))),\n                                               (`Lid (_loc, \"true\")))),\n                                          (`Case\n                                             (_loc, (`Any _loc),\n                                               (`Lid (_loc, \"false\")))))))))),\n                           (`RecBind\n                              (_loc, (`Lid (_loc, \"descr\")),\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"tag\")),\n                                               (`Vrn (_loc, v)))),\n                                          (`Sem\n                                             (_loc,\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"word\")),\n                                                    (`Uid (_loc, \"Any\")))),\n                                               (`RecBind\n                                                  (_loc,\n                                                    (`Lid (_loc, \"tag_name\")),\n                                                    (`Str (_loc, v)))))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let xloc = Some xloc in
                   let x = Some x in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      (let bounds =
                         match (x, xloc) with
                         | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                         | _ -> [] in
                       {
                         text =
                           (`Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
                                          (`Sem
                                             (_loc,
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "pred")),
                                                    (`Fun
                                                       (_loc,
                                                         (`Bar
                                                            (_loc,
                                                              (`Case
                                                                 (_loc,
                                                                   (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                              (`Case
                                                                 (_loc,
                                                                   (`Any _loc),
                                                                   (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                               (`RecBind
                                                  (_loc,
                                                    (`Lid (_loc, "descr")),
                                                    (`Record
                                                       (_loc,
                                                         (`Sem
                                                            (_loc,
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                   (`Vrn
                                                                    (_loc, v)))),
                                                              (`Sem
                                                                 (_loc,
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                   (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds;
                         outer_pattern = None
                       }) : 'simple )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          [((lloc, loc), (Some "loc"));
                          ((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          [((lloc, loc), (Some "loc"));
                          ((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          [((lloc, loc), (Some "loc"));
                          ((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          [((lloc, loc), (Some "loc"));
                          ((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Lid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [((lloc, loc), (Some "loc"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Uid";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [((lloc, loc), (Some "loc"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Str";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [((lloc, loc), (Some "loc"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Pre";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"txt\")),\n                                                                    (`Str\n                                                                    (_loc, x)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"txt\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_3:(__fan_3 : Tokenf.txt) 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "txt")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [((lloc, loc), (Some "loc"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Quot";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n    bounds = [((loc, x), None)];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "quot"))));
                        bounds = [((loc, x), None)];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "DirQuotation";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Any _loc))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`Uid (_loc, \"Any\")))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n    bounds = [((loc, x), None)];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
                                                                    (`Any
                                                                    _loc))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Any")))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "quot"))));
                        bounds = [((loc, x), None)];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Inf";
          `Keyword "(";
          `Token
            ({
               pred = ((function | `Int _ -> true | _ -> false));
               descr = { tag = `Int; word = Any; tag_name = "Int" }
             } : Tokenf.pattern );
          `Keyword ",";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"level\")),\n                                                                    (`Int\n                                                                    (_loc,\n                                                                    level)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"op\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`App\n                                                      (_loc,\n                                                        (`Uid (_loc, \"Level\")),\n                                                        (`Int (_loc, level)))))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds = [((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let level = __fan_2.txt in
                   let xloc = __fan_4.loc in
                   let x = __fan_4.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "op")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Level")),
                                                                    (`Int
                                                                    (_loc,
                                                                    level)))))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds = [((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Inf";
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword "(";
          `Token
            ({
               pred = ((function | `Int _ -> true | _ -> false));
               descr = { tag = `Int; word = Any; tag_name = "Int" }
             } : Tokenf.pattern );
          `Keyword ",";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (`Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`Sem\n                        (_loc,\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"pred\")),\n                               (`Fun\n                                  (_loc,\n                                    (`Bar\n                                       (_loc,\n                                         (`Case\n                                            (_loc,\n                                              (`App\n                                                 (_loc, (`Vrn (_loc, v)),\n                                                   (`Constraint\n                                                      (_loc,\n                                                        (`Record\n                                                           (_loc,\n                                                             (`Sem\n                                                                (_loc,\n                                                                  (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"level\")),\n                                                                    (`Int\n                                                                    (_loc,\n                                                                    level)))),\n                                                                  (`Any _loc))))),\n                                                        (`Dot\n                                                           (_loc,\n                                                             (`Uid\n                                                                (_loc,\n                                                                  \"Tokenf\")),\n                                                             (`Lid\n                                                                (_loc, \"op\")))))))),\n                                              (`Lid (_loc, \"true\")))),\n                                         (`Case\n                                            (_loc, (`Any _loc),\n                                              (`Lid (_loc, \"false\")))))))))),\n                          (`RecBind\n                             (_loc, (`Lid (_loc, \"descr\")),\n                               (`Record\n                                  (_loc,\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag\")),\n                                              (`Vrn (_loc, v)))),\n                                         (`Sem\n                                            (_loc,\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"word\")),\n                                                   (`App\n                                                      (_loc,\n                                                        (`Uid (_loc, \"Level\")),\n                                                        (`Int (_loc, level)))))),\n                                              (`RecBind\n                                                 (_loc,\n                                                   (`Lid (_loc, \"tag_name\")),\n                                                   (`Str (_loc, v)))))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds = [((lloc, l), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.txt)  ~__fan_5:_ 
                   ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let level = __fan_4.txt in
                   let xloc = __fan_6.loc in
                   let x = __fan_6.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (`Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`Sem
                                            (_loc,
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "pred")),
                                                   (`Fun
                                                      (_loc,
                                                        (`Bar
                                                           (_loc,
                                                             (`Case
                                                                (_loc,
                                                                  (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "op")))))))),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                             (`Case
                                                                (_loc,
                                                                  (`Any _loc),
                                                                  (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                              (`RecBind
                                                 (_loc,
                                                   (`Lid (_loc, "descr")),
                                                   (`Record
                                                      (_loc,
                                                        (`Sem
                                                           (_loc,
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                  (`Vrn
                                                                    (_loc, v)))),
                                                             (`Sem
                                                                (_loc,
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Level")),
                                                                    (`Int
                                                                    (_loc,
                                                                    level)))))),
                                                                  (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds =
                          [((lloc, l), (Some "loc"));
                          ((xloc, x), (Some "txt"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (`Keyword (_loc, s));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern );
          `Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (`Keyword (_loc, s));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((xloc, i), (Some \"loc\"))];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (`Keyword (_loc, s));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds = [((xloc, i), (Some "loc"))];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ))],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (`Nterm (_loc, n, s));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let s = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (`Nterm (_loc, n, s));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Nterm (Gramf.obj (name : 'name Gramf.t ));
          `Keyword "Level";
          `Token
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (`Nterm (_loc, n, s));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(n : 'name)  (_loc : Locf.t)  ->
                   let s = __fan_2.txt in
                   let s = Some s in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (`Nterm (_loc, n, s));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "S"],
           ("(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (`Self _loc);\n    styp = (`Self _loc);\n    bounds = [];\n    outer_pattern = None\n  }\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (`Self _loc);
                        styp = (`Self _loc);
                        bounds = [];
                        outer_pattern = None
                      } : 'simple )))));
         ([`Keyword "Ant";
          `Keyword "(";
          `Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
          `Keyword ",";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("match ps with\n| (vs,loc,y) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let bounds =\n              match (loc, y) with\n              | (None ,None ) -> [((xloc, s), None)]\n              | (Some (lloc,ll),None ) ->\n                  [((lloc, ll), (Some \"loc\")); ((xloc, s), None)]\n              | (None ,Some v) -> [(v, (Some \"kind\")); ((xloc, s), None)]\n              | (Some (lloc,ll),Some v) ->\n                  [(v, (Some \"kind\"));\n                  ((lloc, ll), (Some \"loc\"));\n                  ((xloc, s), None)] in\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text =\n                      (`Token\n                         (_loc,\n                           (`Constraint\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"pred\")),\n                                               (`Fun\n                                                  (_loc,\n                                                    (`Bar\n                                                       (_loc,\n                                                         (`Case\n                                                            (_loc,\n                                                              (`App\n                                                                 (_loc,\n                                                                   (`Vrn\n                                                                    (_loc, v)),\n                                                                   (`Constraint\n                                                                    (_loc,\n                                                                    (`Record\n                                                                    (_loc,\n                                                                    (`Sem\n                                                                    (_loc,\n                                                                    (`RecBind\n                                                                    (_loc,\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"kind\")),\n                                                                    (`Str\n                                                                    (_loc,\n                                                                    (x.txt))))),\n                                                                    (`Any\n                                                                    _loc))))),\n                                                                    (`Dot\n                                                                    (_loc,\n                                                                    (`Uid\n                                                                    (_loc,\n                                                                    \"Tokenf\")),\n                                                                    (`Lid\n                                                                    (_loc,\n                                                                    \"ant\")))))))),\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"true\")))),\n                                                         (`Case\n                                                            (_loc,\n                                                              (`Any _loc),\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"false\")))))))))),\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"descr\")),\n                                               (`Record\n                                                  (_loc,\n                                                    (`Sem\n                                                       (_loc,\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"tag\")),\n                                                              (`Vrn (_loc, v)))),\n                                                         (`Sem\n                                                            (_loc,\n                                                              (`RecBind\n                                                                 (_loc,\n                                                                   (`Lid\n                                                                    (_loc,\n                                                                    \"word\")),\n                                                                   (`App\n                                                                    (_loc,\n                                                                    (`Uid\n                                                                    (_loc,\n                                                                    \"A\")),\n                                                                    (`Str\n                                                                    (_loc,\n                                                                    (x.txt))))))),\n                                                              (`RecBind\n                                                                 (_loc,\n                                                                   (`Lid\n                                                                    (_loc,\n                                                                    \"tag_name\")),\n                                                                   (`Str\n                                                                    (_loc, v)))))))))))))))),\n                                (`Dot\n                                   (_loc, (`Uid (_loc, \"Tokenf\")),\n                                     (`Lid (_loc, \"pattern\"))))) : FAst.exp )));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"ant\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.txt)  ~__fan_3:_ 
                   ~__fan_2:(ps : 'or_strs)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = __fan_4.loc in
                   let s = __fan_4.txt in
                   (match ps with
                    | (vs,loc,y) ->
                        vs |>
                          (List.map
                             (fun (x : Tokenf.txt)  ->
                                let bounds =
                                  match (loc, y) with
                                  | (None ,None ) -> [((xloc, s), None)]
                                  | (Some (lloc,ll),None ) ->
                                      [((lloc, ll), (Some "loc"));
                                      ((xloc, s), None)]
                                  | (None ,Some v) ->
                                      [(v, (Some "kind")); ((xloc, s), None)]
                                  | (Some (lloc,ll),Some v) ->
                                      [(v, (Some "kind"));
                                      ((lloc, ll), (Some "loc"));
                                      ((xloc, s), None)] in
                                ({
                                   kind = KNormal;
                                   txt =
                                     [{
                                        text =
                                          (`Token
                                             (_loc,
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
                                                                    "pred")),
                                                                   (`Fun
                                                                    (_loc,
                                                                    (`Bar
                                                                    (_loc,
                                                                    (`Case
                                                                    (_loc,
                                                                    (`App
                                                                    (_loc,
                                                                    (`Vrn
                                                                    (_loc, v)),
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
                                                                    "kind")),
                                                                    (`Str
                                                                    (_loc,
                                                                    (x.txt))))),
                                                                    (`Any
                                                                    _loc))))),
                                                                    (`Dot
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "Tokenf")),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "ant")))))))),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "true")))),
                                                                    (`Case
                                                                    (_loc,
                                                                    (`Any
                                                                    _loc),
                                                                    (`Lid
                                                                    (_loc,
                                                                    "false")))))))))),
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "descr")),
                                                                   (`Record
                                                                    (_loc,
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag")),
                                                                    (`Vrn
                                                                    (_loc, v)))),
                                                                    (`Sem
                                                                    (_loc,
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "word")),
                                                                    (`App
                                                                    (_loc,
                                                                    (`Uid
                                                                    (_loc,
                                                                    "A")),
                                                                    (`Str
                                                                    (_loc,
                                                                    (x.txt))))))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))))),
                                                    (`Dot
                                                       (_loc,
                                                         (`Uid
                                                            (_loc, "Tokenf")),
                                                         (`Lid
                                                            (_loc, "pattern"))))) : 
                                               FAst.exp )));
                                        styp =
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Tokenf")),
                                               (`Lid (_loc, "ant"))));
                                        bounds;
                                        outer_pattern = None
                                      }]
                                 } : Gram_def.osymbol list Gram_def.decorate ))) : 
                     'simple )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
          `Keyword ")"],
           ("match v with\n| (vs,loc,None ) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let bounds =\n              match loc with\n              | Some (loc,l) -> [((loc, l), (Some \"loc\"))]\n              | None  -> [] in\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text = (`Keyword ((x.loc), (x.txt)));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"txt\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n| (vs,loc,Some b) ->\n    let bounds =\n      match loc with\n      | None  -> [(b, (Some \"txt\"))]\n      | Some (loc,l) -> [((loc, l), (Some \"loc\")); (b, (Some \"txt\"))] in\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text = (`Keyword ((x.loc), (x.txt)));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"txt\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(v : 'or_strs)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (match v with
                    | (vs,loc,None ) ->
                        vs |>
                          (List.map
                             (fun (x : Tokenf.txt)  ->
                                let bounds =
                                  match loc with
                                  | Some (loc,l) ->
                                      [((loc, l), (Some "loc"))]
                                  | None  -> [] in
                                ({
                                   kind = KNormal;
                                   txt =
                                     [{
                                        text = (`Keyword ((x.loc), (x.txt)));
                                        styp =
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Tokenf")),
                                               (`Lid (_loc, "txt"))));
                                        bounds;
                                        outer_pattern = None
                                      }]
                                 } : Gram_def.osymbol list Gram_def.decorate )))
                    | (vs,loc,Some b) ->
                        let bounds =
                          match loc with
                          | None  -> [(b, (Some "txt"))]
                          | Some (loc,l) ->
                              [((loc, l), (Some "loc")); (b, (Some "txt"))] in
                        vs |>
                          (List.map
                             (fun (x : Tokenf.txt)  ->
                                ({
                                   kind = KNormal;
                                   txt =
                                     [{
                                        text = (`Keyword ((x.loc), (x.txt)));
                                        styp =
                                          (`Dot
                                             (_loc, (`Uid (_loc, "Tokenf")),
                                               (`Lid (_loc, "txt"))));
                                        bounds;
                                        outer_pattern = None
                                      }]
                                 } : Gram_def.osymbol list Gram_def.decorate ))) : 
                   'simple )))))]) : Gramf.olevel ));
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
            ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'single_symbol) 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let l = __fan_0.txt in
                    let sep = None in
                    (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                     let text =
                       `List
                         (_loc, (if l = "L0" then false else true), s, sep) in
                     [{
                        kind = KNormal;
                        txt =
                          [{ text; styp; bounds = []; outer_pattern = None }]
                      }] : 'symbol )))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = None in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let text =
                      `List
                        (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol )))));
         ([`Keyword "L0";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = Some sep in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let text =
                      `List
                        (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol )))));
         ([`Keyword "L1";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
          `Nterm (Gramf.obj (sep_symbol : 'sep_symbol Gramf.t ))],
           ("let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = `List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_2:(sep : 'sep_symbol) 
                   ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = Some sep in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let text =
                      `List
                        (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("[{ kind = KNone; txt = [s] }; { kind = KSome; txt = [s] }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   ([{ kind = KNone; txt = [s] };
                    { kind = KSome; txt = [s] }] : 'symbol )))));
         ([`Keyword "?";
          `Keyword "[";
          `List1sep
            ((`Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))),
              (`Keyword ";"));
          `Keyword "]"],
           ("[{ kind = KNone; txt = s }; { kind = KSome; txt = s }]\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(s : 'single_symbol list) 
                   ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   ([{ kind = KNone; txt = s }; { kind = KSome; txt = s }] : 
                   'symbol )))));
         ([`Keyword "TRY";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   txt =\n     [{ text; styp = (s.styp); bounds = (s.bounds); outer_pattern = None }]\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (let v = (_loc, (s.text)) in
                    let text = if p = "TRY" then `Try v else `Peek v in
                    [{
                       kind = KNormal;
                       txt =
                         [{
                            text;
                            styp = (s.styp);
                            bounds = (s.bounds);
                            outer_pattern = None
                          }]
                     }] : 'symbol )))));
         ([`Keyword "PEEK";
          `Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))],
           ("let v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Try v else `Peek v in\n[{\n   kind = KNormal;\n   txt =\n     [{ text; styp = (s.styp); bounds = (s.bounds); outer_pattern = None }]\n }]\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'single_symbol) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (let v = (_loc, (s.text)) in
                    let text = if p = "TRY" then `Try v else `Peek v in
                    [{
                       kind = KNormal;
                       txt =
                         [{
                            text;
                            styp = (s.styp);
                            bounds = (s.bounds);
                            outer_pattern = None
                          }]
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
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("List.map\n  (fun (x : Gram_def.osymbol list Gram_def.decorate)  ->\n     match x.txt with\n     | v::[] ->\n         { x with txt = [{ v with outer_pattern = (Some (xloc, i)) }] }\n     | _ -> Locf.failf xloc \"as can not be applied here\") ss\n",
             (Gramf.mk_action
                (fun ~__fan_2:(__fan_2 : Tokenf.txt)  ~__fan_1:_ 
                   ~__fan_0:(ss : 'symbol)  (_loc : Locf.t)  ->
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   (List.map
                      (fun (x : Gram_def.osymbol list Gram_def.decorate)  ->
                         match x.txt with
                         | v::[] ->
                             {
                               x with
                               txt =
                                 [{ v with outer_pattern = (Some (xloc, i)) }]
                             }
                         | _ -> Locf.failf xloc "as can not be applied here")
                      ss : 'psymbol )))))]) : Gramf.olevel ))
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
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualuid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )))));
         ([`Token
             ({
                pred = ((function | `Uid _ -> true | _ -> false));
                descr = { tag = `Uid; word = Any; tag_name = "Uid" }
              } : Tokenf.pattern )],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in (`Uid (_loc, x) : 'qualuid )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (qualid : 'qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )))));
         ([`Token
             ({
                pred = ((function | `Lid _ -> true | _ -> false));
                descr = { tag = `Lid; word = Any; tag_name = "Lid" }
              } : Tokenf.pattern )],
           ("`Lid (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in (`Lid (_loc, i) : 'qualid )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (t_qualid : 't_qualid Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 't_qualid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )))));
         ([`Token
             ({
                pred = ((function | `Uid _ -> true | _ -> false));
                descr = { tag = `Uid; word = Any; tag_name = "Uid" }
              } : Tokenf.pattern );
          `Keyword ".";
          `Token
            ({
               pred =
                 ((function
                   | `Lid ({ txt = "t";_} : Tokenf.txt) -> true
                   | _ -> false));
               descr = { tag = `Lid; word = (A "t"); tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("`Uid (_loc, x)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let x = __fan_0.txt in (`Uid (_loc, x) : 't_qualid )))))]) : 
      Gramf.olevel ));
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
            ({
               pred = ((function | `Str _ -> true | _ -> false));
               descr = { tag = `Str; word = Any; tag_name = "Str" }
             } : Tokenf.pattern )],
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
             ({
                pred = ((function | `Lid _ -> true | _ -> false));
                descr = { tag = `Lid; word = Any; tag_name = "Lid" }
              } : Tokenf.pattern );
          `Keyword "@";
          `Keyword "Inline";
          `Keyword ":";
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("Hashtbl.add inline_rules x rules; None\n",
             (Gramf.mk_action
                (fun ~__fan_4:(rules : 'rule_list)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   (Hashtbl.add inline_rules x rules; None : 'entry )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (position : 'position Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "First"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Last"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Before"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "After"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   ((`Vrn (_loc, x) : FAst.exp ) : 'position )))));
         ([`Keyword "Level"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
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
                    let label = None in
                    let assoc = None in ({ label; assoc; rules } : 'level )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let label = __fan_0.txt in
                   let label = Some label in
                   let assoc = None in ({ label; assoc; rules } : 'level )))));
         ([`Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_1:(rules : 'rule_list)  ~__fan_0:(assoc : 'assoc)
                    (_loc : Locf.t)  ->
                   let label = None in
                   let assoc = Some assoc in
                   ({ label; assoc; rules } : 'level )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
          `Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))],
           ("{ label; assoc; rules }\n",
             (Gramf.mk_action
                (fun ~__fan_2:(rules : 'rule_list)  ~__fan_1:(assoc : 'assoc)
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let label = __fan_0.txt in
                   let label = Some label in
                   let assoc = Some assoc in
                   ({ label; assoc; rules } : 'level )))))]) : Gramf.olevel ));
  Gramf.extend_single (assoc : 'assoc Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "LA"],
            ("(`Vrn (_loc, x) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))));
         ([`Keyword "RA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))));
         ([`Keyword "NA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )))))]) : Gramf.olevel ));
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
            ("let rec cross (prod : matrix list) =\n  (match prod with\n   | [] -> [[]]\n   | (x : matrix)::xs ->\n       (cross xs) |>\n         (Listf.concat_map\n            (fun (acc : Gram_def.osymbol list Gram_def.decorate list)  ->\n               x |>\n                 (List.map\n                    (fun (zs : Gram_def.osymbol list Gram_def.decorate)  ->\n                       zs :: acc)))) : Gram_def.osymbol list\n                                         Gram_def.decorate list list ) in\n(List.map (fun prod  -> mk_prule ~prod ~action)) @@ (cross prod)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(prod : 'left_rule)  (_loc : Locf.t)  ->
                    let action = None in
                    (let rec cross (prod : matrix list) =
                       (match prod with
                        | [] -> [[]]
                        | (x : matrix)::xs ->
                            (cross xs) |>
                              (Listf.concat_map
                                 (fun
                                    (acc :
                                      Gram_def.osymbol list Gram_def.decorate
                                        list)
                                     ->
                                    x |>
                                      (List.map
                                         (fun
                                            (zs :
                                              Gram_def.osymbol list
                                                Gram_def.decorate)
                                             -> zs :: acc)))) : Gram_def.osymbol
                                                                  list
                                                                  Gram_def.decorate
                                                                  list list ) in
                     (List.map (fun prod  -> mk_prule ~prod ~action)) @@
                       (cross prod) : 'rule )))));
         ([`Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ));
          `Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))],
           ("let rec cross (prod : matrix list) =\n  (match prod with\n   | [] -> [[]]\n   | (x : matrix)::xs ->\n       (cross xs) |>\n         (Listf.concat_map\n            (fun (acc : Gram_def.osymbol list Gram_def.decorate list)  ->\n               x |>\n                 (List.map\n                    (fun (zs : Gram_def.osymbol list Gram_def.decorate)  ->\n                       zs :: acc)))) : Gram_def.osymbol list\n                                         Gram_def.decorate list list ) in\n(List.map (fun prod  -> mk_prule ~prod ~action)) @@ (cross prod)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(action : 'opt_action) 
                   ~__fan_0:(prod : 'left_rule)  (_loc : Locf.t)  ->
                   let action = Some action in
                   (let rec cross (prod : matrix list) =
                      (match prod with
                       | [] -> [[]]
                       | (x : matrix)::xs ->
                           (cross xs) |>
                             (Listf.concat_map
                                (fun
                                   (acc :
                                     Gram_def.osymbol list Gram_def.decorate
                                       list)
                                    ->
                                   x |>
                                     (List.map
                                        (fun
                                           (zs :
                                             Gram_def.osymbol list
                                               Gram_def.decorate)
                                            -> zs :: acc)))) : Gram_def.osymbol
                                                                 list
                                                                 Gram_def.decorate
                                                                 list list ) in
                    (List.map (fun prod  -> mk_prule ~prod ~action)) @@
                      (cross prod) : 'rule )))));
         ([`Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern )],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let action = None in
                   (let rules =
                      match query_inline x with
                      | Some x -> x
                      | None  ->
                          Locf.failf xloc "inline rules %s not found" x in
                    match action with
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
                                     (Some (`App (_loc, a, b) : FAst.exp ))
                                 }) rules : 'rule )))));
         ([`Keyword "@";
          `Token
            ({
               pred = ((function | `Lid _ -> true | _ -> false));
               descr = { tag = `Lid; word = Any; tag_name = "Lid" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))],
           ("let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | None  -> { x with action = (Some a) }\n         | Some b ->\n             { x with action = (Some (`App (_loc, a, b) : FAst.exp )) })\n      rules\n",
             (Gramf.mk_action
                (fun ~__fan_2:(action : 'opt_action) 
                   ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let action = Some action in
                   (let rules =
                      match query_inline x with
                      | Some x -> x
                      | None  ->
                          Locf.failf xloc "inline rules %s not found" x in
                    match action with
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
                                     (Some (`App (_loc, a, b) : FAst.exp ))
                                 }) rules : 'rule )))))]) : Gramf.olevel ));
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
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("if x.name = Tokenf.empty_name\nthen let expander loc _ s = Parsef.exp loc s in Tokenf.quot_expand expander x\nelse Ast_quotation.expand x Dyn_tag.exp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
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
              ({
                 pred = ((function | `Str _ -> true | _ -> false));
                 descr = { tag = `Str; word = Any; tag_name = "Str" }
               } : Tokenf.pattern )],
            ("(`Str (_loc, s) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    ((`Str (_loc, s) : FAst.exp ) : 'string )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("Tokenf.ant_expand Parsef.exp s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (Tokenf.ant_expand Parsef.exp s : 'string )))))]) : 
      Gramf.olevel ))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "extend")
    ~entry:extend_body ();
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "unsafe_extend")
    ~entry:unsafe_extend_body ()
