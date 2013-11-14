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
              "RA";
              "+";
              "*";
              "?";
              "=";
              "_";
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
let rules = Gramf.mk_dynamic g "rules"
let symbol: matrix Gramf.t = Gramf.mk_dynamic g "symbol"
let rule = Gramf.mk_dynamic g "rule"
let meta_rule = Gramf.mk_dynamic g "meta_rule"
let rule_list = Gramf.mk_dynamic g "rule_list"
let psymbol: matrix Gramf.t = Gramf.mk_dynamic g "psymbol"
let level = Gramf.mk_dynamic g "level"
let entry: Gram_def.entry option Gramf.t = Gramf.mk_dynamic g "entry"
let extend_body = Gramf.mk_dynamic g "extend_body"
let unsafe_extend_body = Gramf.mk_dynamic g "unsafe_extend_body"
let simple: matrix Gramf.t = Gramf.mk_dynamic g "simple"
let single_symbol: Gram_def.osymbol Gramf.t =
  Gramf.mk_dynamic g "single_symbol"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let a_int: 'a_int Gramf.t = grammar_entry_create "a_int"
  and or_strs: 'or_strs Gramf.t = grammar_entry_create "or_strs"
  and single_symbol_as: 'single_symbol_as Gramf.t =
    grammar_entry_create "single_symbol_as" in
  Gramf.extend_single (a_int : 'a_int Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
                 Tokenf.pattern )];
            annot = "(`Int (_loc, level) : FAst.exp )\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let level = __fan_0.txt in
                    ((`Int (_loc, level) : FAst.exp ) : 'a_int ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'a_int ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Tokenf.ant_expand Parsef.exp x\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Tokenf.ant_expand Parsef.exp x : 'a_int ) : Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'a_int ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (single_symbol : 'single_symbol Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "EOI"); tag_name = "Key" }
                  } : Tokenf.pattern )];
            annot =
              "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let v = __fan_0.txt in
                    ({
                       text =
                         (Token
                            (_loc,
                              (`Constraint
                                 (_loc,
                                   (`Record
                                      (_loc,
                                        (`RecBind
                                           (_loc, (`Lid (_loc, "descr")),
                                             (`Record
                                                (_loc,
                                                  (`Sem
                                                     (_loc,
                                                       (`RecBind
                                                          (_loc,
                                                            (`Lid
                                                               (_loc, "tag")),
                                                            (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                     } : 'single_symbol ) : Tokenf.txt ->
                                              Locf.t -> 'single_symbol ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Int"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int32"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int64"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Nativeint"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Flo"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Chr"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Label"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Optlabel"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let xloc = None in
                   let x = None in
                   (let bounds =
                      match (x, xloc) with
                      | (Some x,Some xloc) -> [((xloc, x), (Some "txt"))]
                      | _ -> [] in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Int"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int32"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int64"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Nativeint"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Flo"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Chr"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Label"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Optlabel"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let bounds =\n  match (x, xloc) with\n  | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n  | _ -> [] in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds;\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let xloc = __fan_3.loc in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, x)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds = [((lloc, loc), (Some \"loc\"))];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let loc = __fan_2.txt in
                   let x = __fan_3.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Quot"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [((loc, x), None)];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     {
                       tag = `Key;
                       word = (A "DirQuotation");
                       tag_name = "Key"
                     }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`Uid (_loc, \"Any\")))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n  bounds = [((loc, x), None)];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                   (_loc, v)))))))))))))),
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
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"Level\")),\n                                                 z)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds =\n    ((match (lloc, l) with\n      | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n      | _ -> []) @ [((xloc, x), (Some \"txt\"))]);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.txt)  _  (z : 'a_int)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = None in
                   let l = None in
                   let xloc = __fan_4.loc in
                   let x = __fan_4.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc, v)))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds =
                        ((match (lloc, l) with
                          | (Some lloc,Some l) -> [((lloc, l), (Some "loc"))]
                          | _ -> []) @ [((xloc, x), (Some "txt"))]);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 'a_int ->
                                                   Tokenf.txt ->
                                                     Tokenf.txt ->
                                                       Locf.t ->
                                                         'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"Level\")),\n                                                 z)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds =\n    ((match (lloc, l) with\n      | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n      | _ -> []) @ [((xloc, x), (Some \"txt\"))]);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.txt)  _  (z : 'a_int)  _ 
                   (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let lloc = Some lloc in
                   let l = Some l in
                   let xloc = __fan_6.loc in
                   let x = __fan_6.txt in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc, v)))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds =
                        ((match (lloc, l) with
                          | (Some lloc,Some l) -> [((lloc, l), (Some "loc"))]
                          | _ -> []) @ [((xloc, x), (Some "txt"))]);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 'a_int ->
                                                   Tokenf.txt ->
                                                     Tokenf.txt ->
                                                       Tokenf.txt ->
                                                         Tokenf.txt ->
                                                           Locf.t ->
                                                             'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"Level\")),\n                                                 z)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds =\n    (match (lloc, l) with\n     | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun _  _  _  (z : 'a_int)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = None in
                   let l = None in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc, v)))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds =
                        (match (lloc, l) with
                         | (Some lloc,Some l) -> [((lloc, l), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 'a_int ->
                                                   Tokenf.txt ->
                                                     Tokenf.txt ->
                                                       Locf.t ->
                                                         'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, v)))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"Level\")),\n                                                 z)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, v)))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n  bounds =\n    (match (lloc, l) with\n     | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun _  _  _  (z : 'a_int)  _  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let lloc = Some lloc in
                   let l = Some l in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc, v)))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "op"))));
                      bounds =
                        (match (lloc, l) with
                         | (Some lloc,Some l) -> [((lloc, l), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 'a_int ->
                                                   Tokenf.txt ->
                                                     Tokenf.txt ->
                                                       Tokenf.txt ->
                                                         Tokenf.txt ->
                                                           Locf.t ->
                                                             'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, \"Key\")))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, s)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, \"Key\")))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds =\n    (match (i, xloc) with\n     | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = None in
                   let i = None in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn
                                                              (_loc, "Key")))),
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
                                                                    (_loc, s)))))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc,
                                                                    "Key")))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        (match (i, xloc) with
                         | (Some i,Some xloc) -> [((xloc, i), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, \"Key\")))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")),\n                                                 (`Str (_loc, s)))))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, \"Key\")))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds =\n    (match (i, xloc) with\n     | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   let xloc = Some xloc in
                   let i = Some i in
                   ({
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn
                                                              (_loc, "Key")))),
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
                                                                    (_loc, s)))))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc,
                                                                    "Key")))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        (match (i, xloc) with
                         | (Some i,Some xloc) -> [((xloc, i), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "key"); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot =
             "let e = Tokenf.ant_expand Parsef.exp x in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, \"Key\")))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")), e)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, \"Key\")))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds =\n    (match (i, xloc) with\n     | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   let xloc = None in
                   let i = None in
                   (let e = Tokenf.ant_expand Parsef.exp x in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn
                                                              (_loc, "Key")))),
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
                                                                    "A")), e)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc,
                                                                    "Key")))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        (match (i, xloc) with
                         | (Some i,Some xloc) -> [((xloc, i), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.ant ->
                                             Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "key"); tag_name = "Ant" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let e = Tokenf.ant_expand Parsef.exp x in\n{\n  text =\n    (Token\n       (_loc,\n         (`Constraint\n            (_loc,\n              (`Record\n                 (_loc,\n                   (`RecBind\n                      (_loc, (`Lid (_loc, \"descr\")),\n                        (`Record\n                           (_loc,\n                             (`Sem\n                                (_loc,\n                                  (`RecBind\n                                     (_loc, (`Lid (_loc, \"tag\")),\n                                       (`Vrn (_loc, \"Key\")))),\n                                  (`Sem\n                                     (_loc,\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"word\")),\n                                            (`App\n                                               (_loc, (`Uid (_loc, \"A\")), e)))),\n                                       (`RecBind\n                                          (_loc, (`Lid (_loc, \"tag_name\")),\n                                            (`Str (_loc, \"Key\")))))))))))))),\n              (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n         FAst.exp )));\n  styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n  bounds =\n    (match (i, xloc) with\n     | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n     | _ -> []);\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.ant) 
                   (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   let xloc = Some xloc in
                   let i = Some i in
                   (let e = Tokenf.ant_expand Parsef.exp x in
                    {
                      text =
                        (Token
                           (_loc,
                             (`Constraint
                                (_loc,
                                  (`Record
                                     (_loc,
                                       (`RecBind
                                          (_loc, (`Lid (_loc, "descr")),
                                            (`Record
                                               (_loc,
                                                 (`Sem
                                                    (_loc,
                                                      (`RecBind
                                                         (_loc,
                                                           (`Lid
                                                              (_loc, "tag")),
                                                           (`Vrn
                                                              (_loc, "Key")))),
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
                                                                    "A")), e)))),
                                                           (`RecBind
                                                              (_loc,
                                                                (`Lid
                                                                   (_loc,
                                                                    "tag_name")),
                                                                (`Str
                                                                   (_loc,
                                                                    "Key")))))))))))))),
                                  (`Dot
                                     (_loc, (`Uid (_loc, "Tokenf")),
                                       (`Lid (_loc, "pattern"))))) : 
                             FAst.exp )));
                      styp =
                        (`Dot
                           (_loc, (`Uid (_loc, "Tokenf")),
                             (`Lid (_loc, "txt"))));
                      bounds =
                        (match (i, xloc) with
                         | (Some i,Some xloc) -> [((xloc, i), (Some "loc"))]
                         | _ -> []);
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               Tokenf.ant ->
                                                 Locf.t -> 'single_symbol ))
         };
         {
           symbols = [Nterm (Gramf.obj (name : 'name Gramf.t ))];
           annot =
             "{\n  text =\n    (Nterm\n       (_loc, n,\n         (match s with | None  -> None | Some s -> Some (int_of_string s))));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (n : 'name)  (_loc : Locf.t)  ->
                   let s = None in
                   ({
                      text =
                        (Nterm
                           (_loc, n,
                             (match s with
                              | None  -> None
                              | Some s -> Some (int_of_string s))));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol ) : 'name -> Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Nterm (Gramf.obj (name : 'name Gramf.t ));
             Token
               ({
                  descr =
                    { tag = `Key; word = (A "Level"); tag_name = "Key" }
                } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
               Tokenf.pattern )];
           annot =
             "{\n  text =\n    (Nterm\n       (_loc, n,\n         (match s with | None  -> None | Some s -> Some (int_of_string s))));\n  styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n  bounds = [];\n  outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (n : 'name)  (_loc : Locf.t) 
                   ->
                   let s = __fan_2.txt in
                   let s = Some s in
                   ({
                      text =
                        (Nterm
                           (_loc, n,
                             (match s with
                              | None  -> None
                              | Some s -> Some (int_of_string s))));
                      styp =
                        (`Quote
                           (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Tokenf.txt ->
                                               'name ->
                                                 Locf.t -> 'single_symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "S"); tag_name = "Key" } } : 
                Tokenf.pattern )];
           annot =
             "{ text = (Self _loc); styp = (`Self _loc); bounds = []; outer_pattern = None\n}\n";
           fn =
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ({
                      text = (Self _loc);
                      styp = (`Self _loc);
                      bounds = [];
                      outer_pattern = None
                    } : 'single_symbol ) : Tokenf.txt ->
                                             Locf.t -> 'single_symbol ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (or_strs : 'or_strs Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [List1sep
                 ((Token
                     ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                      } : Tokenf.pattern )),
                   (Token
                      ({
                         descr =
                           { tag = `Key; word = (A "|"); tag_name = "Key" }
                       } : Tokenf.pattern )))];
            annot = "(xs, None, None)\n";
            fn =
              (Gramf.mk_action
                 (fun (xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                    ((xs, None, None) : 'or_strs ) : Tokenf.txt list ->
                                                       Locf.t -> 'or_strs ))
          };
         {
           symbols =
             [List1sep
                ((Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern )),
                  (Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot = "(xs, None, (Some (xloc, s)))\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (xs : Tokenf.txt list) 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_2.loc in
                   let s = __fan_2.txt in
                   ((xs, None, (Some (xloc, s))) : 'or_strs ) : Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    list ->
                                                                    Locf.t ->
                                                                    'or_strs ))
         };
         {
           symbols =
             [List1sep
                ((Token
                    ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                    Tokenf.pattern )),
                  (Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot = "(xs, (Some (lloc, l)), (Some (xloc, s)))\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_4 : Tokenf.txt)  _  (__fan_2 : Tokenf.txt)  _ 
                   (xs : Tokenf.txt list)  (_loc : Locf.t)  ->
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let xloc = __fan_4.loc in
                   let s = __fan_4.txt in
                   ((xs, (Some (lloc, l)), (Some (xloc, s))) : 'or_strs ) : 
                Tokenf.txt ->
                  Tokenf.txt ->
                    Tokenf.txt ->
                      Tokenf.txt -> Tokenf.txt list -> Locf.t -> 'or_strs ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (simple : 'simple Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "EOI"); tag_name = "Key" }
                  } : Tokenf.pattern )];
            annot =
              "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let v = __fan_0.txt in
                    ((fun (txt : Gram_def.osymbol)  ->
                        [({ kind = Gram_def.KNormal; txt = [txt] } : 
                        Gram_def.osymbol list Gram_def.decorate )])
                       {
                         text =
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       } : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Int"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int32"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int64"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Nativeint"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Flo"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Chr"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Label"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Optlabel"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Int"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int32"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Int64"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Nativeint"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Flo"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Chr"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Label"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Optlabel"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  (let bounds =\n     match (x, xloc) with\n     | (Some x,Some xloc) -> [((xloc, x), (Some \"txt\"))]\n     | _ -> [] in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, v)))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`Uid (_loc, \"Any\")))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, v)))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds;\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
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
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                              (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\")); ((xloc, x), (Some \"txt\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Lid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Uid"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Str"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Pre"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, x)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds = [((lloc, loc), (Some \"loc\"))];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_3 : Tokenf.txt)  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "Quot"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n    bounds = [((loc, x), None)];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     {
                       tag = `Key;
                       word = (A "DirQuotation");
                       tag_name = "Key"
                     }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`Uid (_loc, \"Any\")))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"quot\"))));\n    bounds = [((loc, x), None)];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let loc = __fan_1.loc in
                   let x = __fan_1.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    (_loc, v)))))))))))))),
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
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc,\n                                                   (`Uid (_loc, \"Level\")), z)))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds =\n      ((match (lloc, l) with\n        | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n        | _ -> []) @ [((xloc, x), (Some \"txt\"))]);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.txt)  _  (z : 'a_int)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = None in
                   let l = None in
                   let xloc = __fan_4.loc in
                   let x = __fan_4.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc, v)))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds =
                          ((match (lloc, l) with
                            | (Some lloc,Some l) ->
                                [((lloc, l), (Some "loc"))]
                            | _ -> []) @ [((xloc, x), (Some "txt"))]);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            'a_int ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc,\n                                                   (`Uid (_loc, \"Level\")), z)))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds =\n      ((match (lloc, l) with\n        | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n        | _ -> []) @ [((xloc, x), (Some \"txt\"))]);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun _  (__fan_6 : Tokenf.txt)  _  (z : 'a_int)  _ 
                   (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let lloc = Some lloc in
                   let l = Some l in
                   let xloc = __fan_6.loc in
                   let x = __fan_6.txt in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc, v)))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds =
                          ((match (lloc, l) with
                            | (Some lloc,Some l) ->
                                [((lloc, l), (Some "loc"))]
                            | _ -> []) @ [((xloc, x), (Some "txt"))]);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            'a_int ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Tokenf.txt ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc,\n                                                   (`Uid (_loc, \"Level\")), z)))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds =\n      (match (lloc, l) with\n       | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n       | _ -> []);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun _  _  _  (z : 'a_int)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = None in
                   let l = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc, v)))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds =
                          (match (lloc, l) with
                           | (Some lloc,Some l) ->
                               [((lloc, l), (Some "loc"))]
                           | _ -> []);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            'a_int ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Inf"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (a_int : 'a_int Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol list\n                                                  Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, v)))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc,\n                                                   (`Uid (_loc, \"Level\")), z)))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, v)))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"op\"))));\n    bounds =\n      (match (lloc, l) with\n       | (Some lloc,Some l) -> [((lloc, l), (Some \"loc\"))]\n       | _ -> []);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun _  _  _  (z : 'a_int)  _  (__fan_2 : Tokenf.txt)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let v = __fan_0.txt in
                   let lloc = __fan_2.loc in
                   let l = __fan_2.txt in
                   let lloc = Some lloc in
                   let l = Some l in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = Gram_def.KNormal; txt = [txt] } : Gram_def.osymbol
                                                                    list
                                                                    Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn (_loc, v)))),
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
                                                                    z)))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc, v)))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "op"))));
                        bounds =
                          (match (lloc, l) with
                           | (Some lloc,Some l) ->
                               [((lloc, l), (Some "loc"))]
                           | _ -> []);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            'a_int ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Tokenf.txt ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, \"Key\")))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, s)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, \"Key\")))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds =\n      (match (i, xloc) with\n       | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n       | _ -> []);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = None in
                   let i = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn
                                                                (_loc, "Key")))),
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
                                                                    (_loc, s)))))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc,
                                                                    "Key")))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          (match (i, xloc) with
                           | (Some i,Some xloc) ->
                               [((xloc, i), (Some "loc"))]
                           | _ -> []);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text =\n      (Token\n         (_loc,\n           (`Constraint\n              (_loc,\n                (`Record\n                   (_loc,\n                     (`RecBind\n                        (_loc, (`Lid (_loc, \"descr\")),\n                          (`Record\n                             (_loc,\n                               (`Sem\n                                  (_loc,\n                                    (`RecBind\n                                       (_loc, (`Lid (_loc, \"tag\")),\n                                         (`Vrn (_loc, \"Key\")))),\n                                    (`Sem\n                                       (_loc,\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"word\")),\n                                              (`App\n                                                 (_loc, (`Uid (_loc, \"A\")),\n                                                   (`Str (_loc, s)))))),\n                                         (`RecBind\n                                            (_loc, (`Lid (_loc, \"tag_name\")),\n                                              (`Str (_loc, \"Key\")))))))))))))),\n                (`Dot\n                   (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n           FAst.exp )));\n    styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n    bounds =\n      (match (i, xloc) with\n       | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n       | _ -> []);\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   let xloc = Some xloc in
                   let i = Some i in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text =
                          (Token
                             (_loc,
                               (`Constraint
                                  (_loc,
                                    (`Record
                                       (_loc,
                                         (`RecBind
                                            (_loc, (`Lid (_loc, "descr")),
                                              (`Record
                                                 (_loc,
                                                   (`Sem
                                                      (_loc,
                                                        (`RecBind
                                                           (_loc,
                                                             (`Lid
                                                                (_loc, "tag")),
                                                             (`Vrn
                                                                (_loc, "Key")))),
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
                                                                    (_loc, s)))))),
                                                             (`RecBind
                                                                (_loc,
                                                                  (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                  (`Str
                                                                    (_loc,
                                                                    "Key")))))))))))))),
                                    (`Dot
                                       (_loc, (`Uid (_loc, "Tokenf")),
                                         (`Lid (_loc, "pattern"))))) : 
                               FAst.exp )));
                        styp =
                          (`Dot
                             (_loc, (`Uid (_loc, "Tokenf")),
                               (`Lid (_loc, "txt"))));
                        bounds =
                          (match (i, xloc) with
                           | (Some i,Some xloc) ->
                               [((xloc, i), (Some "loc"))]
                           | _ -> []);
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "key"); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  (let e = Tokenf.ant_expand Parsef.exp x in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, \"Key\")))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`App\n                                                  (_loc, (`Uid (_loc, \"A\")),\n                                                    e)))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, \"Key\")))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds =\n       (match (i, xloc) with\n        | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n        | _ -> []);\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   let xloc = None in
                   let i = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      (let e = Tokenf.ant_expand Parsef.exp x in
                       {
                         text =
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                                 (_loc,
                                                                   "Key")))),
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
                                                                    "A")), e)))),
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                   (`Str
                                                                    (_loc,
                                                                    "Key")))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds =
                           (match (i, xloc) with
                            | (Some i,Some xloc) ->
                                [((xloc, i), (Some "loc"))]
                            | _ -> []);
                         outer_pattern = None
                       }) : 'simple ) : Tokenf.ant -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "key"); tag_name = "Ant" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  (let e = Tokenf.ant_expand Parsef.exp x in\n   {\n     text =\n       (Token\n          (_loc,\n            (`Constraint\n               (_loc,\n                 (`Record\n                    (_loc,\n                      (`RecBind\n                         (_loc, (`Lid (_loc, \"descr\")),\n                           (`Record\n                              (_loc,\n                                (`Sem\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"tag\")),\n                                          (`Vrn (_loc, \"Key\")))),\n                                     (`Sem\n                                        (_loc,\n                                          (`RecBind\n                                             (_loc, (`Lid (_loc, \"word\")),\n                                               (`App\n                                                  (_loc, (`Uid (_loc, \"A\")),\n                                                    e)))),\n                                          (`RecBind\n                                             (_loc,\n                                               (`Lid (_loc, \"tag_name\")),\n                                               (`Str (_loc, \"Key\")))))))))))))),\n                 (`Dot\n                    (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"pattern\"))))) : \n            FAst.exp )));\n     styp = (`Dot (_loc, (`Uid (_loc, \"Tokenf\")), (`Lid (_loc, \"txt\"))));\n     bounds =\n       (match (i, xloc) with\n        | (Some i,Some xloc) -> [((xloc, i), (Some \"loc\"))]\n        | _ -> []);\n     outer_pattern = None\n   })\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.ant) 
                   (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   let xloc = __fan_2.loc in
                   let i = __fan_2.txt in
                   let xloc = Some xloc in
                   let i = Some i in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      (let e = Tokenf.ant_expand Parsef.exp x in
                       {
                         text =
                           (Token
                              (_loc,
                                (`Constraint
                                   (_loc,
                                     (`Record
                                        (_loc,
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
                                                                 (_loc,
                                                                   "Key")))),
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
                                                                    "A")), e)))),
                                                              (`RecBind
                                                                 (_loc,
                                                                   (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                   (`Str
                                                                    (_loc,
                                                                    "Key")))))))))))))),
                                     (`Dot
                                        (_loc, (`Uid (_loc, "Tokenf")),
                                          (`Lid (_loc, "pattern"))))) : 
                                FAst.exp )));
                         styp =
                           (`Dot
                              (_loc, (`Uid (_loc, "Tokenf")),
                                (`Lid (_loc, "txt"))));
                         bounds =
                           (match (i, xloc) with
                            | (Some i,Some xloc) ->
                                [((xloc, i), (Some "loc"))]
                            | _ -> []);
                         outer_pattern = None
                       }) : 'simple ) : Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.ant -> Locf.t -> 'simple ))
         };
         {
           symbols = [Nterm (Gramf.obj (name : 'name Gramf.t ))];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text =\n      (Nterm\n         (_loc, n,\n           (match s with | None  -> None | Some s -> Some (int_of_string s))));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (n : 'name)  (_loc : Locf.t)  ->
                   let s = None in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text =
                          (Nterm
                             (_loc, n,
                               (match s with
                                | None  -> None
                                | Some s -> Some (int_of_string s))));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple ) : 'name -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Nterm (Gramf.obj (name : 'name Gramf.t ));
             Token
               ({
                  descr =
                    { tag = `Key; word = (A "Level"); tag_name = "Key" }
                } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
               Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text =\n      (Nterm\n         (_loc, n,\n           (match s with | None  -> None | Some s -> Some (int_of_string s))));\n    styp = (`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (n : 'name)  (_loc : Locf.t) 
                   ->
                   let s = __fan_2.txt in
                   let s = Some s in
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text =
                          (Nterm
                             (_loc, n,
                               (match s with
                                | None  -> None
                                | Some s -> Some (int_of_string s))));
                        styp =
                          (`Quote
                             (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))));
                        bounds = [];
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt ->
                                        Tokenf.txt ->
                                          'name -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "S"); tag_name = "Key" } } : 
                Tokenf.pattern )];
           annot =
             "(fun (txt : Gram_def.osymbol)  ->\n   [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol list\n                                         Gram_def.decorate )])\n  {\n    text = (Self _loc);\n    styp = (`Self _loc);\n    bounds = [];\n    outer_pattern = None\n  }\n";
           fn =
             (Gramf.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ((fun (txt : Gram_def.osymbol)  ->
                       [({ kind = KNormal; txt = [txt] } : Gram_def.osymbol
                                                             list
                                                             Gram_def.decorate )])
                      {
                        text = (Self _loc);
                        styp = (`Self _loc);
                        bounds = [];
                        outer_pattern = None
                      } : 'simple ) : Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "Ant"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "match ps with\n| (vs,loc,y) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let bounds =\n              match (loc, y) with\n              | (None ,None ) -> [((xloc, s), None)]\n              | (Some (lloc,ll),None ) ->\n                  [((lloc, ll), (Some \"loc\")); ((xloc, s), None)]\n              | (None ,Some v) -> [(v, (Some \"kind\")); ((xloc, s), None)]\n              | (Some (lloc,ll),Some v) ->\n                  [(v, (Some \"kind\"));\n                  ((lloc, ll), (Some \"loc\"));\n                  ((xloc, s), None)] in\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text =\n                      (Token\n                         (_loc,\n                           (`Constraint\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"descr\")),\n                                          (`Record\n                                             (_loc,\n                                               (`Sem\n                                                  (_loc,\n                                                    (`RecBind\n                                                       (_loc,\n                                                         (`Lid (_loc, \"tag\")),\n                                                         (`Vrn (_loc, v)))),\n                                                    (`Sem\n                                                       (_loc,\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"word\")),\n                                                              (`App\n                                                                 (_loc,\n                                                                   (`Uid\n                                                                    (_loc,\n                                                                    \"Kind\")),\n                                                                   (`Str\n                                                                    (_loc,\n                                                                    (x.txt))))))),\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"tag_name\")),\n                                                              (`Str (_loc, v)))))))))))))),\n                                (`Dot\n                                   (_loc, (`Uid (_loc, \"Tokenf\")),\n                                     (`Lid (_loc, \"pattern\"))))) : FAst.exp )));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"ant\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n";
           fn =
             (Gramf.mk_action
                (fun _  (__fan_4 : Tokenf.txt)  _  (ps : 'or_strs)  _ 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
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
                                          (Token
                                             (_loc,
                                               (`Constraint
                                                  (_loc,
                                                    (`Record
                                                       (_loc,
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
                                                                    "Kind")),
                                                                    (`Str
                                                                    (_loc,
                                                                    (x.txt))))))),
                                                                    (`RecBind
                                                                    (_loc,
                                                                    (`Lid
                                                                    (_loc,
                                                                    "tag_name")),
                                                                    (`Str
                                                                    (_loc, v)))))))))))))),
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
                     'simple ) : Tokenf.txt ->
                                   Tokenf.txt ->
                                     Tokenf.txt ->
                                       'or_strs ->
                                         Tokenf.txt ->
                                           Tokenf.txt -> Locf.t -> 'simple ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
                Tokenf.pattern );
             Nterm (Gramf.obj (or_strs : 'or_strs Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "match v with\n| (vs,loc,None ) ->\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            let bounds =\n              match loc with\n              | Some (loc,l) -> [((loc, l), (Some \"loc\"))]\n              | None  -> [] in\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text =\n                      (Token\n                         ((x.loc),\n                           (`Constraint\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"descr\")),\n                                          (`Record\n                                             (_loc,\n                                               (`Sem\n                                                  (_loc,\n                                                    (`RecBind\n                                                       (_loc,\n                                                         (`Lid (_loc, \"tag\")),\n                                                         (`Vrn (_loc, \"Key\")))),\n                                                    (`Sem\n                                                       (_loc,\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"word\")),\n                                                              (`App\n                                                                 (_loc,\n                                                                   (`Uid\n                                                                    (_loc,\n                                                                    \"A\")),\n                                                                   (`Str\n                                                                    (_loc,\n                                                                    (x.txt))))))),\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"tag_name\")),\n                                                              (`Str\n                                                                 (_loc,\n                                                                   \"Key\")))))))))))))),\n                                (`Dot\n                                   (_loc, (`Uid (_loc, \"Tokenf\")),\n                                     (`Lid (_loc, \"pattern\"))))) : FAst.exp )));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"txt\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n| (vs,loc,Some b) ->\n    let bounds =\n      match loc with\n      | None  -> [(b, (Some \"txt\"))]\n      | Some (loc,l) -> [((loc, l), (Some \"loc\")); (b, (Some \"txt\"))] in\n    vs |>\n      (List.map\n         (fun (x : Tokenf.txt)  ->\n            ({\n               kind = KNormal;\n               txt =\n                 [{\n                    text =\n                      (Token\n                         ((x.loc),\n                           (`Constraint\n                              (_loc,\n                                (`Record\n                                   (_loc,\n                                     (`RecBind\n                                        (_loc, (`Lid (_loc, \"descr\")),\n                                          (`Record\n                                             (_loc,\n                                               (`Sem\n                                                  (_loc,\n                                                    (`RecBind\n                                                       (_loc,\n                                                         (`Lid (_loc, \"tag\")),\n                                                         (`Vrn (_loc, \"Key\")))),\n                                                    (`Sem\n                                                       (_loc,\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"word\")),\n                                                              (`App\n                                                                 (_loc,\n                                                                   (`Uid\n                                                                    (_loc,\n                                                                    \"A\")),\n                                                                   (`Str\n                                                                    (_loc,\n                                                                    (x.txt))))))),\n                                                         (`RecBind\n                                                            (_loc,\n                                                              (`Lid\n                                                                 (_loc,\n                                                                   \"tag_name\")),\n                                                              (`Str\n                                                                 (_loc,\n                                                                   \"Key\")))))))))))))),\n                                (`Dot\n                                   (_loc, (`Uid (_loc, \"Tokenf\")),\n                                     (`Lid (_loc, \"pattern\"))))) : FAst.exp )));\n                    styp =\n                      (`Dot\n                         (_loc, (`Uid (_loc, \"Tokenf\")),\n                           (`Lid (_loc, \"txt\"))));\n                    bounds;\n                    outer_pattern = None\n                  }]\n             } : Gram_def.osymbol list Gram_def.decorate )))\n";
           fn =
             (Gramf.mk_action
                (fun _  (v : 'or_strs)  _  (_loc : Locf.t)  ->
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
                                        text =
                                          (Token
                                             ((x.loc),
                                               (`Constraint
                                                  (_loc,
                                                    (`Record
                                                       (_loc,
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
                                                                    (_loc,
                                                                    "Key")))),
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
                                                                    (_loc,
                                                                    "Key")))))))))))))),
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
                                        text =
                                          (Token
                                             ((x.loc),
                                               (`Constraint
                                                  (_loc,
                                                    (`Record
                                                       (_loc,
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
                                                                    (_loc,
                                                                    "Key")))),
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
                                                                    (_loc,
                                                                    "Key")))))))))))))),
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
                                               (`Lid (_loc, "txt"))));
                                        bounds;
                                        outer_pattern = None
                                      }]
                                 } : Gram_def.osymbol list Gram_def.decorate ))) : 
                   'simple ) : Tokenf.txt ->
                                 'or_strs -> Tokenf.txt -> Locf.t -> 'simple ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (single_symbol_as : 'single_symbol_as Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
            annot = "t\n";
            fn =
              (Gramf.mk_action
                 (fun (t : 'single_symbol)  (_loc : Locf.t)  ->
                    (t : 'single_symbol_as ) : 'single_symbol ->
                                                 Locf.t -> 'single_symbol_as ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot = "{ t with outer_pattern = (Some (xloc, s)) }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (t : 'single_symbol) 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_2.loc in
                   let s = __fan_2.txt in
                   ({ t with outer_pattern = (Some (xloc, s)) } : 'single_symbol_as ) : 
                Tokenf.txt ->
                  Tokenf.txt -> 'single_symbol -> Locf.t -> 'single_symbol_as ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (symbol : 'symbol Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "L0"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
            annot =
              "let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet (text :Gram_def.text)=\n  List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n";
            fn =
              (Gramf.mk_action
                 (fun (s : 'single_symbol)  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let l = __fan_0.txt in
                    let sep = None in
                    (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                     let (text :Gram_def.text)=
                       List
                         (_loc, (if l = "L0" then false else true), s, sep) in
                     [{
                        kind = KNormal;
                        txt =
                          [{ text; styp; bounds = []; outer_pattern = None }]
                      }] : 'symbol ) : 'single_symbol ->
                                         Tokenf.txt -> Locf.t -> 'symbol ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "L1"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet (text :Gram_def.text)=\n  List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n";
           fn =
             (Gramf.mk_action
                (fun (s : 'single_symbol)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = None in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let (text :Gram_def.text)=
                      List (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol ) : 'single_symbol ->
                                        Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "L0"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "SEP"); tag_name = "Key" }
                } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet (text :Gram_def.text)=\n  List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n";
           fn =
             (Gramf.mk_action
                (fun (sep : 'single_symbol)  _  (s : 'single_symbol) 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = Some sep in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let (text :Gram_def.text)=
                      List (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol ) : 'single_symbol ->
                                        Tokenf.txt ->
                                          'single_symbol ->
                                            Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "L1"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "SEP"); tag_name = "Key" }
                } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "let styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet (text :Gram_def.text)=\n  List (_loc, (if l = \"L0\" then false else true), s, sep) in\n[{ kind = KNormal; txt = [{ text; styp; bounds = []; outer_pattern = None }]\n }]\n";
           fn =
             (Gramf.mk_action
                (fun (sep : 'single_symbol)  _  (s : 'single_symbol) 
                   (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let l = __fan_0.txt in
                   let sep = Some sep in
                   (let styp = `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                    let (text :Gram_def.text)=
                      List (_loc, (if l = "L0" then false else true), s, sep) in
                    [{
                       kind = KNormal;
                       txt =
                         [{ text; styp; bounds = []; outer_pattern = None }]
                     }] : 'symbol ) : 'single_symbol ->
                                        Tokenf.txt ->
                                          'single_symbol ->
                                            Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "[{ kind = KNone; txt = [s] }; { kind = KSome; txt = [s] }]\n";
           fn =
             (Gramf.mk_action
                (fun (s : 'single_symbol)  _  (_loc : Locf.t)  ->
                   ([{ kind = KNone; txt = [s] };
                    { kind = KSome; txt = [s] }] : 'symbol ) : 'single_symbol
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
               Tokenf.pattern );
             List1sep
               ((Nterm
                   (Gramf.obj (single_symbol_as : 'single_symbol_as Gramf.t ))),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "[{ kind = KNone; txt = s }; { kind = KSome; txt = s }]\n";
           fn =
             (Gramf.mk_action
                (fun _  (s : 'single_symbol_as list)  _  _  (_loc : Locf.t) 
                   ->
                   ([{ kind = KNone; txt = s }; { kind = KSome; txt = s }] : 
                   'symbol ) : Tokenf.txt ->
                                 'single_symbol_as list ->
                                   Tokenf.txt ->
                                     Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "TRY"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "let v = (_loc, (s.text)) in\nlet (text :Gram_def.text)= if p = \"TRY\" then Try v else Peek v in\n[{\n   kind = KNormal;\n   txt =\n     [{ text; styp = (s.styp); bounds = (s.bounds); outer_pattern = None }]\n }]\n";
           fn =
             (Gramf.mk_action
                (fun (s : 'single_symbol)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (let v = (_loc, (s.text)) in
                    let (text :Gram_def.text)=
                      if p = "TRY" then Try v else Peek v in
                    [{
                       kind = KNormal;
                       txt =
                         [{
                            text;
                            styp = (s.styp);
                            bounds = (s.bounds);
                            outer_pattern = None
                          }]
                     }] : 'symbol ) : 'single_symbol ->
                                        Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Key; word = (A "PEEK"); tag_name = "Key" }
                 } : Tokenf.pattern );
             Nterm (Gramf.obj (single_symbol : 'single_symbol Gramf.t ))];
           annot =
             "let v = (_loc, (s.text)) in\nlet (text :Gram_def.text)= if p = \"TRY\" then Try v else Peek v in\n[{\n   kind = KNormal;\n   txt =\n     [{ text; styp = (s.styp); bounds = (s.bounds); outer_pattern = None }]\n }]\n";
           fn =
             (Gramf.mk_action
                (fun (s : 'single_symbol)  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (let v = (_loc, (s.text)) in
                    let (text :Gram_def.text)=
                      if p = "TRY" then Try v else Peek v in
                    [{
                       kind = KNormal;
                       txt =
                         [{
                            text;
                            styp = (s.styp);
                            bounds = (s.bounds);
                            outer_pattern = None
                          }]
                     }] : 'symbol ) : 'single_symbol ->
                                        Tokenf.txt -> Locf.t -> 'symbol ))
         };
         {
           symbols = [Nterm (Gramf.obj (simple : 'simple Gramf.t ))];
           annot = "p\n";
           fn =
             (Gramf.mk_action
                (fun (p : 'simple)  (_loc : Locf.t)  -> (p : 'symbol ) : 
                'simple -> Locf.t -> 'symbol ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (psymbol : 'psymbol Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (symbol : 'symbol Gramf.t ))];
            annot = "ss\n";
            fn =
              (Gramf.mk_action
                 (fun (ss : 'symbol)  (_loc : Locf.t)  -> (ss : 'psymbol ) : 
                 'symbol -> Locf.t -> 'psymbol ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (symbol : 'symbol Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "List.map\n  (fun (x : Gram_def.osymbol list Gram_def.decorate)  ->\n     match x.txt with\n     | v::[] ->\n         { x with txt = [{ v with outer_pattern = (Some (xloc, i)) }] }\n     | _ -> Locf.failf xloc \"as can not be applied here\") ss\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.txt)  _  (ss : 'symbol) 
                   (_loc : Locf.t)  ->
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
                      ss : 'psymbol ) : Tokenf.txt ->
                                          Tokenf.txt ->
                                            'symbol -> Locf.t -> 'psymbol ))
         }]
     } : Gramf.olevel )
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let opt_action: 'opt_action Gramf.t = grammar_entry_create "opt_action" in
  Gramf.extend_single (extend_header : 'extend_header Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" }
                  } : Tokenf.pattern );
              Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Nterm (Gramf.obj (t_qualid : 't_qualid Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
                Tokenf.pattern )];
            annot =
              "let old = gm () in let () = module_name := t in ((Some i), old)\n";
            fn =
              (Gramf.mk_action
                 (fun _  (t : 't_qualid)  _  (i : 'qualid)  _ 
                    (_loc : Locf.t)  ->
                    (let old = gm () in
                     let () = module_name := t in ((Some i), old) : 'extend_header ) : 
                 Tokenf.txt ->
                   't_qualid ->
                     Tokenf.txt ->
                       'qualid -> Tokenf.txt -> Locf.t -> 'extend_header ))
          };
         {
           symbols = [Nterm (Gramf.obj (qualuid : 'qualuid Gramf.t ))];
           annot =
             "let old = gm () in let () = module_name := t in (None, old)\n";
           fn =
             (Gramf.mk_action
                (fun (t : 'qualuid)  (_loc : Locf.t)  ->
                   (let old = gm () in
                    let () = module_name := t in (None, old) : 'extend_header ) : 
                'qualuid -> Locf.t -> 'extend_header ))
         };
         {
           symbols = [];
           annot = "(None, (gm ()))\n";
           fn =
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> ((None, (gm ())) : 'extend_header ) : 
                Locf.t -> 'extend_header ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (extend_body : 'extend_body Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (extend_header : 'extend_header Gramf.t ));
              List1 (Nterm (Gramf.obj (entry : 'entry Gramf.t )))];
            annot =
              "(fun f  -> f true)\n  (fun safe  ->\n     let (gram,old) = rest in\n     let items = Listf.filter_map (fun x  -> x) el in\n     let res = make _loc { items; gram; safe } in\n     let () = module_name := old in res)\n";
            fn =
              (Gramf.mk_action
                 (fun (el : 'entry list)  (rest : 'extend_header) 
                    (_loc : Locf.t)  ->
                    ((fun f  -> f true)
                       (fun safe  ->
                          let (gram,old) = rest in
                          let items = Listf.filter_map (fun x  -> x) el in
                          let res = make _loc { items; gram; safe } in
                          let () = module_name := old in res) : 'extend_body ) : 
                 'entry list -> 'extend_header -> Locf.t -> 'extend_body ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (unsafe_extend_body : 'unsafe_extend_body Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (extend_header : 'extend_header Gramf.t ));
              List1 (Nterm (Gramf.obj (entry : 'entry Gramf.t )))];
            annot =
              "(fun f  -> f false)\n  (fun safe  ->\n     let (gram,old) = rest in\n     let items = Listf.filter_map (fun x  -> x) el in\n     let res = make _loc { items; gram; safe } in\n     let () = module_name := old in res)\n";
            fn =
              (Gramf.mk_action
                 (fun (el : 'entry list)  (rest : 'extend_header) 
                    (_loc : Locf.t)  ->
                    ((fun f  -> f false)
                       (fun safe  ->
                          let (gram,old) = rest in
                          let items = Listf.filter_map (fun x  -> x) el in
                          let res = make _loc { items; gram; safe } in
                          let () = module_name := old in res) : 'unsafe_extend_body ) : 
                 'entry list ->
                   'extend_header -> Locf.t -> 'unsafe_extend_body ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (qualuid : 'qualuid Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
            fn =
              (Gramf.mk_action
                 (fun (xs : 'qualuid)  _  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid ) : 
                 'qualuid -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'qualuid ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern )];
           annot = "`Uid (_loc, x)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in (`Uid (_loc, x) : 'qualuid ) : 
                Tokenf.txt -> Locf.t -> 'qualuid ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (qualid : 'qualid Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
            fn =
              (Gramf.mk_action
                 (fun (xs : 'qualid)  _  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid ) : 
                 'qualid -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'qualid ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
           annot = "`Lid (_loc, i)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in (`Lid (_loc, i) : 'qualid ) : 
                Tokenf.txt -> Locf.t -> 'qualid ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (t_qualid : 't_qualid Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "`Dot (_loc, (`Uid (_loc, x)), xs)\n";
            fn =
              (Gramf.mk_action
                 (fun (xs : 't_qualid)  _  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid ) : 
                 't_qualid -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 't_qualid ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = (A "t"); tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot = "`Uid (_loc, x)\n";
           fn =
             (Gramf.mk_action
                (fun _  _  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let x = __fan_0.txt in (`Uid (_loc, x) : 't_qualid ) : 
                Tokenf.txt -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 't_qualid ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (name : 'name Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (qualid : 'qualid Gramf.t ))];
            annot = "mk_name il\n";
            fn =
              (Gramf.mk_action
                 (fun (il : 'qualid)  (_loc : Locf.t)  ->
                    (mk_name il : 'name ) : 'qualid -> Locf.t -> 'name ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (entry_name : 'entry_name Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (qualid : 'qualid Gramf.t ))];
            annot =
              "let x =\n  match (name : Tokenf.txt option ) with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), (x.txt)) with\n       | None  -> Locf.failf x.loc \"lang `%s' not resolved\" x.txt\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n";
            fn =
              (Gramf.mk_action
                 (fun (il : 'qualid)  (_loc : Locf.t)  ->
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
                     (x, (mk_name il)) : 'entry_name ) : 'qualid ->
                                                           Locf.t ->
                                                             'entry_name ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (qualid : 'qualid Gramf.t ));
             Token
               ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
               Tokenf.pattern )];
           annot =
             "let x =\n  match (name : Tokenf.txt option ) with\n  | Some x ->\n      let old = !Ast_quotation.default in\n      (match Ast_quotation.resolve_name ((`Sub []), (x.txt)) with\n       | None  -> Locf.failf x.loc \"lang `%s' not resolved\" x.txt\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name il))\n";
           fn =
             (Gramf.mk_action
                (fun (name : Tokenf.txt)  (il : 'qualid)  (_loc : Locf.t)  ->
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
                    (x, (mk_name il)) : 'entry_name ) : Tokenf.txt ->
                                                          'qualid ->
                                                            Locf.t ->
                                                              'entry_name ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (entry : 'entry Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Nterm (Gramf.obj (level : 'level Gramf.t ))];
            annot =
              "let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\nSome { name = p; local = false; pos; level }\n";
            fn =
              (Gramf.mk_action
                 (fun (level : 'level)  _  (rest : 'entry_name) 
                    (_loc : Locf.t)  ->
                    let pos = None in
                    (let (n,p) = rest in
                     (match n with
                      | `name old -> Ast_quotation.default := old
                      | _ -> ());
                     Some { name = p; local = false; pos; level } : 'entry ) : 
                 'level -> Tokenf.txt -> 'entry_name -> Locf.t -> 'entry ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (position : 'position Gramf.t ));
             Nterm (Gramf.obj (level : 'level Gramf.t ))];
           annot =
             "let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\nSome { name = p; local = false; pos; level }\n";
           fn =
             (Gramf.mk_action
                (fun (level : 'level)  (pos : 'position)  _ 
                   (rest : 'entry_name)  (_loc : Locf.t)  ->
                   let pos = Some pos in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    Some { name = p; local = false; pos; level } : 'entry ) : 
                'level ->
                  'position -> Tokenf.txt -> 'entry_name -> Locf.t -> 'entry ))
         };
         {
           symbols =
             [Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({
                  descr =
                    { tag = `Key; word = (A "Local"); tag_name = "Key" }
                } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (level : 'level Gramf.t ))];
           annot =
             "let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\nSome { name = p; local = true; pos; level }\n";
           fn =
             (Gramf.mk_action
                (fun (level : 'level)  _  _  _  (rest : 'entry_name) 
                   (_loc : Locf.t)  ->
                   let pos = None in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    Some { name = p; local = true; pos; level } : 'entry ) : 
                'level ->
                  Tokenf.txt ->
                    Tokenf.txt ->
                      Tokenf.txt -> 'entry_name -> Locf.t -> 'entry ))
         };
         {
           symbols =
             [Nterm (Gramf.obj (entry_name : 'entry_name Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({
                  descr =
                    { tag = `Key; word = (A "Local"); tag_name = "Key" }
                } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (position : 'position Gramf.t ));
             Nterm (Gramf.obj (level : 'level Gramf.t ))];
           annot =
             "let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\nSome { name = p; local = true; pos; level }\n";
           fn =
             (Gramf.mk_action
                (fun (level : 'level)  (pos : 'position)  _  _  _ 
                   (rest : 'entry_name)  (_loc : Locf.t)  ->
                   let pos = Some pos in
                   (let (n,p) = rest in
                    (match n with
                     | `name old -> Ast_quotation.default := old
                     | _ -> ());
                    Some { name = p; local = true; pos; level } : 'entry ) : 
                'level ->
                  'position ->
                    Tokenf.txt ->
                      Tokenf.txt ->
                        Tokenf.txt -> 'entry_name -> Locf.t -> 'entry ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({
                  descr =
                    { tag = `Key; word = (A "Inline"); tag_name = "Key" }
                } : Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))];
           annot = "Hashtbl.add inline_rules x rules; None\n";
           fn =
             (Gramf.mk_action
                (fun (rules : 'rule_list)  _  _  _  (__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let x = __fan_0.txt in
                   (Hashtbl.add inline_rules x rules; None : 'entry ) : 
                'rule_list ->
                  Tokenf.txt ->
                    Tokenf.txt ->
                      Tokenf.txt -> Tokenf.txt -> Locf.t -> 'entry ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (position : 'position Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
                 Tokenf.pattern )];
            annot = "(`Int (_loc, x) : FAst.exp )\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    ((`Int (_loc, x) : FAst.exp ) : 'position ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'position ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Tokenf.ant_expand Parsef.exp x\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Tokenf.ant_expand Parsef.exp x : 'position ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'position ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (level : 'level Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))];
            annot = "{ assoc; rules }\n";
            fn =
              (Gramf.mk_action
                 (fun (rules : 'rule_list)  (_loc : Locf.t)  ->
                    let assoc = None in ({ assoc; rules } : 'level ) : 
                 'rule_list -> Locf.t -> 'level ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (assoc : 'assoc Gramf.t ));
             Nterm (Gramf.obj (rule_list : 'rule_list Gramf.t ))];
           annot = "{ assoc; rules }\n";
           fn =
             (Gramf.mk_action
                (fun (rules : 'rule_list)  (assoc : 'assoc)  (_loc : Locf.t) 
                   -> let assoc = Some assoc in ({ assoc; rules } : 'level ) : 
                'rule_list -> 'assoc -> Locf.t -> 'level ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (assoc : 'assoc Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "RA"); tag_name = "Key" }
                  } : Tokenf.pattern )];
            annot = "(`Lid (_loc, \"false\") : FAst.exp )\n";
            fn =
              (Gramf.mk_action
                 (fun _  (_loc : Locf.t)  ->
                    ((`Lid (_loc, "false") : FAst.exp ) : 'assoc ) : 
                 Tokenf.txt -> Locf.t -> 'assoc ))
          };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "bool"); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Tokenf.ant_expand Parsef.exp x\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Tokenf.ant_expand Parsef.exp x : 'assoc ) : Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'assoc ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (rule_list : 'rule_list Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" }
                  } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
                Tokenf.pattern )];
            annot = "[]\n";
            fn =
              (Gramf.mk_action
                 (fun _  _  (_loc : Locf.t)  -> ([] : 'rule_list ) : 
                 Tokenf.txt -> Tokenf.txt -> Locf.t -> 'rule_list ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
                Tokenf.pattern );
             List1sep
               ((Nterm (Gramf.obj (rule : 'rule Gramf.t ))),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Listf.concat ruless\n";
           fn =
             (Gramf.mk_action
                (fun _  (ruless : 'rule list)  _  (_loc : Locf.t)  ->
                   (Listf.concat ruless : 'rule_list ) : Tokenf.txt ->
                                                           'rule list ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'rule_list ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (rule : 'rule Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ))];
            annot =
              "let rec cross (prod : matrix list) =\n  (match prod with\n   | [] -> [[]]\n   | (x : matrix)::xs ->\n       (cross xs) |>\n         (Listf.concat_map\n            (fun (acc : Gram_def.osymbol list Gram_def.decorate list)  ->\n               x |>\n                 (List.map\n                    (fun (zs : Gram_def.osymbol list Gram_def.decorate)  ->\n                       zs :: acc)))) : Gram_def.osymbol list\n                                         Gram_def.decorate list list ) in\nlet (action :Gram_def.action)=\n  match action with | None  -> E None | Some v -> v in\n(List.map (fun prod  -> mk_prule ~prod ~action)) @@ (cross prod)\n";
            fn =
              (Gramf.mk_action
                 (fun (prod : 'left_rule)  (_loc : Locf.t)  ->
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
                     let (action :Gram_def.action)=
                       match action with | None  -> E None | Some v -> v in
                     (List.map (fun prod  -> mk_prule ~prod ~action)) @@
                       (cross prod) : 'rule ) : 'left_rule -> Locf.t -> 'rule ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (left_rule : 'left_rule Gramf.t ));
             Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))];
           annot =
             "let rec cross (prod : matrix list) =\n  (match prod with\n   | [] -> [[]]\n   | (x : matrix)::xs ->\n       (cross xs) |>\n         (Listf.concat_map\n            (fun (acc : Gram_def.osymbol list Gram_def.decorate list)  ->\n               x |>\n                 (List.map\n                    (fun (zs : Gram_def.osymbol list Gram_def.decorate)  ->\n                       zs :: acc)))) : Gram_def.osymbol list\n                                         Gram_def.decorate list list ) in\nlet (action :Gram_def.action)=\n  match action with | None  -> E None | Some v -> v in\n(List.map (fun prod  -> mk_prule ~prod ~action)) @@ (cross prod)\n";
           fn =
             (Gramf.mk_action
                (fun (action : 'opt_action)  (prod : 'left_rule) 
                   (_loc : Locf.t)  ->
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
                    let (action :Gram_def.action)=
                      match action with | None  -> E None | Some v -> v in
                    (List.map (fun prod  -> mk_prule ~prod ~action)) @@
                      (cross prod) : 'rule ) : 'opt_action ->
                                                 'left_rule ->
                                                   Locf.t -> 'rule ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )];
           annot =
             "let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | E (None ) -> { x with action = a }\n         | E (Some b) ->\n             {\n               x with\n               action =\n                 ((match (a : Gram_def.action ) with\n                   | E (None ) -> E (Some b)\n                   | E (Some a) -> E (Some (`App (_loc, a, b) : FAst.exp ))\n                   | Ant _ -> assert false))\n             }\n         | _ -> assert false) rules\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
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
                             | E (None ) -> { x with action = a }
                             | E (Some b) ->
                                 {
                                   x with
                                   action =
                                     ((match (a : Gram_def.action ) with
                                       | E (None ) -> E (Some b)
                                       | E (Some a) ->
                                           E
                                             (Some
                                                (`App (_loc, a, b) : 
                                                FAst.exp ))
                                       | Ant _ -> assert false))
                                 }
                             | _ -> assert false) rules : 'rule ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'rule ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (opt_action : 'opt_action Gramf.t ))];
           annot =
             "let rules =\n  match query_inline x with\n  | Some x -> x\n  | None  -> Locf.failf xloc \"inline rules %s not found\" x in\nmatch action with\n| None  -> rules\n| Some a ->\n    List.map\n      (fun (x : Gram_def.rule)  ->\n         match x.action with\n         | E (None ) -> { x with action = a }\n         | E (Some b) ->\n             {\n               x with\n               action =\n                 ((match (a : Gram_def.action ) with\n                   | E (None ) -> E (Some b)\n                   | E (Some a) -> E (Some (`App (_loc, a, b) : FAst.exp ))\n                   | Ant _ -> assert false))\n             }\n         | _ -> assert false) rules\n";
           fn =
             (Gramf.mk_action
                (fun (action : 'opt_action)  (__fan_1 : Tokenf.txt)  _ 
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
                             | E (None ) -> { x with action = a }
                             | E (Some b) ->
                                 {
                                   x with
                                   action =
                                     ((match (a : Gram_def.action ) with
                                       | E (None ) -> E (Some b)
                                       | E (Some a) ->
                                           E
                                             (Some
                                                (`App (_loc, a, b) : 
                                                FAst.exp ))
                                       | Ant _ -> assert false))
                                 }
                             | _ -> assert false) rules : 'rule ) : 'opt_action
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'rule ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (left_rule : 'left_rule Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (psymbol : 'psymbol Gramf.t ))];
            annot = "[x]\n";
            fn =
              (Gramf.mk_action
                 (fun (x : 'psymbol)  (_loc : Locf.t)  -> ([x] : 'left_rule ) : 
                 'psymbol -> Locf.t -> 'left_rule ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (psymbol : 'psymbol Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Self];
           annot = "x :: xs\n";
           fn =
             (Gramf.mk_action
                (fun (xs : 'left_rule)  _  (x : 'psymbol)  (_loc : Locf.t) 
                   -> (x :: xs : 'left_rule ) : 'left_rule ->
                                                  Tokenf.txt ->
                                                    'psymbol ->
                                                      Locf.t -> 'left_rule ))
         };
         {
           symbols = [];
           annot = "[]\n";
           fn =
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> ([] : 'left_rule ) : Locf.t ->
                                                                'left_rule ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (opt_action : 'opt_action Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
                 Tokenf.pattern )];
            annot = "E (Some (Parsef.expand_exp x))\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (E (Some (Parsef.expand_exp x)) : 'opt_action ) : 
                 Tokenf.quot -> Locf.t -> 'opt_action ))
          };
         {
           symbols =
             [Token
                ({
                   descr =
                     { tag = `Ant; word = (Kind "fn"); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Ant x\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in (Ant x : 'opt_action ) : Tokenf.ant ->
                                                                 Locf.t ->
                                                                   'opt_action ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                 } : Tokenf.pattern )];
           annot = "Ant x\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let x = __fan_0 in (Ant x : 'opt_action ) : Tokenf.ant ->
                                                                 Locf.t ->
                                                                   'opt_action ))
         }]
     } : Gramf.olevel )
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "extend")
    ~entry:extend_body ();
  Ast_quotation.of_exp ~lexer:Lex_gram.from_stream ~name:(d, "unsafe_extend")
    ~entry:unsafe_extend_body ()
