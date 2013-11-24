let as_cset = Translate_lex.as_cset
let regexp_for_string = Translate_lex.regexp_for_string
let remove_as = Translate_lex.remove_as
let meta_cset _loc (x : Fcset.t) =
  Fan_ops.meta_list
    (fun _loc  (a,b)  ->
       (`Par
          (_loc,
            (`Com
               (_loc, (`Int (_loc, (string_of_int a))),
                 (`Int (_loc, (string_of_int b)))))) : Astf.ep )) _loc x
let rec meta_concrete_regexp _loc (x : Translate_lex.concrete_regexp) =
  match x with
  | Epsilon  -> (`Uid (_loc, "Epsilon") : Astf.ep )
  | Eof  -> (`Uid (_loc, "Eof") : Astf.ep )
  | Characters a ->
      (`App (_loc, (`Uid (_loc, "Characters")), (meta_cset _loc a)) : 
      Astf.ep )
  | Sequence (a0,a1) ->
      (`App
         (_loc,
           (`App
              (_loc, (`Uid (_loc, "Sequence")),
                (meta_concrete_regexp _loc a0))),
           (meta_concrete_regexp _loc a1)) : Astf.ep )
  | Alternative (a0,a1) ->
      (`App
         (_loc,
           (`App
              (_loc, (`Uid (_loc, "Alternative")),
                (meta_concrete_regexp _loc a0))),
           (meta_concrete_regexp _loc a1)) : Astf.ep )
  | Repetition a ->
      (`App
         (_loc, (`Uid (_loc, "Repetition")), (meta_concrete_regexp _loc a)) : 
      Astf.ep )
  | Bind (a,(loc,s)) ->
      (`App
         (_loc, (`Uid (_loc, "Bind")),
           (`Par
              (_loc,
                (`Com
                   (_loc, (meta_concrete_regexp _loc a),
                     (`Par
                        (_loc,
                          (`Com
                             (_loc, (Ast_gen.meta_here _loc loc),
                               (`Str (loc, (String.escaped s)) : Astf.ep )))))))))) : 
      Astf.ep )
exception UnboundRegexp
exception UnboundCase
let regexp = Gramf.mk "regexp"
let char_class = Gramf.mk "char_class"
let char_class1 = Gramf.mk "char_class1"
let lex = Gramf.mk "lex"
let declare_regexp = Gramf.mk "declare_regexp"
let lex_fan = Gramf.mk "lex_fan"
let case = Gramf.mk "case"
let make_automata shortest l =
  Compile_lex.output_entry @@
    (Lexgen.make_single_dfa { shortest; clauses = (Listf.concat l) })
let make_lex nt a b =
  Gramf.extend_single (nt : 'nt Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" }
                  } : Tokenf.pattern );
              List0sep
                ((Nterm (Gramf.obj (case : 'case Gramf.t ))),
                  (Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern )))];
            annot = "";
            fn =
              (Gramf.mk_action
                 (a : 'case list -> Tokenf.txt -> Locf.t -> 'nt ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "<"); tag_name = "Key" } } : 
                Tokenf.pattern );
             List0sep
               ((Nterm (Gramf.obj (case : 'case Gramf.t ))),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )))];
           annot = "";
           fn =
             (Gramf.mk_action
                (b : 'case list -> Tokenf.txt -> Locf.t -> 'nt ))
         }]
     } : Gramf.olevel )
let _ =
  make_lex lex (fun l  _  _  -> make_automata false l)
    (fun l  _  _  -> make_automata true l);
  make_lex lex_fan
    (fun l  _  _loc  ->
       let e = make_automata false l in
       (`Constraint
          (_loc, e,
            (`Arrow
               (_loc,
                 (`Dot
                    (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lexbuf")))),
                 (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
         Astf.exp ))
    (fun l  _  _loc  ->
       let e = make_automata true l in
       (`Constraint
          (_loc, e,
            (`Arrow
               (_loc,
                 (`Dot
                    (_loc, (`Uid (_loc, "Lexing")), (`Lid (_loc, "lexbuf")))),
                 (`Dot (_loc, (`Uid (_loc, "Tokenf")), (`Lid (_loc, "t"))))))) : 
         Astf.exp ))
let _ =
  Gramf.extend_single (case : 'case Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (regexp : 'regexp Gramf.t ));
              Token
                ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
                Tokenf.pattern )];
            annot = "[(r, (Parsef.expand_exp x))]\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_1 : Tokenf.quot)  (r : 'regexp)  (_loc : Locf.t)
                     ->
                    let x = __fan_1 in
                    ([(r, (Parsef.expand_exp x))] : 'case ) : Tokenf.quot ->
                                                                'regexp ->
                                                                  Locf.t ->
                                                                    'case ))
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
             "let res =\n  try Hashtbl.find Predef_lex.named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres { tokens_opt = None; quot_opt = y; loc = xloc }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = None in
                   (let res =
                      try Hashtbl.find Predef_lex.named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res { tokens_opt = None; quot_opt = y; loc = xloc } : 
                     'case ) : Tokenf.txt -> Tokenf.txt -> Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find Predef_lex.named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres { tokens_opt = None; quot_opt = y; loc = xloc }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_2 : Tokenf.quot)  (__fan_1 : Tokenf.txt)  _ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = __fan_2 in
                   let y = Some y in
                   (let res =
                      try Hashtbl.find Predef_lex.named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res { tokens_opt = None; quot_opt = y; loc = xloc } : 
                     'case ) : Tokenf.quot ->
                                 Tokenf.txt -> Tokenf.txt -> Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             List1sep
               ((Token
                   ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                   Tokenf.pattern )),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find Predef_lex.named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres { tokens_opt = (Some l); quot_opt = y; loc = xloc }\n";
           fn =
             (Gramf.mk_action
                (fun _  (l : Tokenf.txt list)  _  (__fan_1 : Tokenf.txt)  _ 
                   (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = None in
                   (let res =
                      try Hashtbl.find Predef_lex.named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res { tokens_opt = (Some l); quot_opt = y; loc = xloc } : 
                     'case ) : Tokenf.txt ->
                                 Tokenf.txt list ->
                                   Tokenf.txt ->
                                     Tokenf.txt ->
                                       Tokenf.txt -> Locf.t -> 'case ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "@"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             List1sep
               ((Token
                   ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                   Tokenf.pattern )),
                 (Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern )));
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )];
           annot =
             "let res =\n  try Hashtbl.find Predef_lex.named_cases x\n  with\n  | Not_found  ->\n      (Fan_warnings.emitf xloc.loc_start \"Reference to unbound case name %s\"\n         x;\n       raise UnboundCase) in\nres { tokens_opt = (Some l); quot_opt = y; loc = xloc }\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_5 : Tokenf.quot)  _  (l : Tokenf.txt list)  _ 
                   (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                   let xloc = __fan_1.loc in
                   let x = __fan_1.txt in
                   let y = __fan_5 in
                   let y = Some y in
                   (let res =
                      try Hashtbl.find Predef_lex.named_cases x
                      with
                      | Not_found  ->
                          (Fan_warnings.emitf xloc.loc_start
                             "Reference to unbound case name %s" x;
                           raise UnboundCase) in
                    res { tokens_opt = (Some l); quot_opt = y; loc = xloc } : 
                     'case ) : Tokenf.quot ->
                                 Tokenf.txt ->
                                   Tokenf.txt list ->
                                     Tokenf.txt ->
                                       Tokenf.txt ->
                                         Tokenf.txt -> Locf.t -> 'case ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 10);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" }
                 } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
            annot = "Bind (r1, (xloc, y))\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_2 : Tokenf.txt)  _  (r1 : 'regexp) 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_2.loc in
                    let y = __fan_2.txt in (Bind (r1, (xloc, y)) : 'regexp ) : 
                 Tokenf.txt -> Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 20);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot =
              "let s1 = as_cset r1 in let s2 = as_cset r2 in Characters (Fcset.diff s1 s2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (let s1 = as_cset r1 in
                     let s2 = as_cset r2 in Characters (Fcset.diff s1 s2) : 
                    'regexp ) : 'regexp ->
                                  Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 30);
       lassoc = true;
       productions =
         [{
            symbols =
              [Self;
              Token
                ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Self];
            annot = "Alternative (r1, r2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Alternative (r1, r2) : 'regexp ) : 'regexp ->
                                                          Tokenf.txt ->
                                                            'regexp ->
                                                              Locf.t ->
                                                                'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 40);
       lassoc = true;
       productions =
         [{
            symbols = [Self; Self];
            annot = "Sequence (r1, r2)\n";
            fn =
              (Gramf.mk_action
                 (fun (r2 : 'regexp)  (r1 : 'regexp)  (_loc : Locf.t)  ->
                    (Sequence (r1, r2) : 'regexp ) : 'regexp ->
                                                       'regexp ->
                                                         Locf.t -> 'regexp ))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (regexp : 'regexp Gramf.t )
    ({
       label = (Some 50);
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                 Tokenf.pattern )];
            annot =
              "Characters (Fcset.singleton (Char.code @@ (Escape.char c)))\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let c = __fan_0.txt in
                    (Characters
                       (Fcset.singleton (Char.code @@ (Escape.char c))) : 
                      'regexp ) : Tokenf.txt -> Locf.t -> 'regexp ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
                Tokenf.pattern )];
           annot = "regexp_for_string @@ (Escape.string s)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (regexp_for_string @@ (Escape.string s) : 'regexp ) : 
                Tokenf.txt -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
                Tokenf.pattern );
             Nterm (Gramf.obj (char_class : 'char_class Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Characters cc\n";
           fn =
             (Gramf.mk_action
                (fun _  (cc : 'char_class)  _  (_loc : Locf.t)  ->
                   (Characters cc : 'regexp ) : Tokenf.txt ->
                                                  'char_class ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Repetition r1\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Repetition r1 : 'regexp ) : Tokenf.txt ->
                                                  'regexp ->
                                                    Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Alternative (Epsilon, r1)\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Alternative (Epsilon, r1) : 'regexp ) : Tokenf.txt ->
                                                              'regexp ->
                                                                Locf.t ->
                                                                  'regexp ))
         };
         {
           symbols =
             [Self;
             Token
               ({ descr = { tag = `Key; word = (A "+"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "Sequence ((Repetition (remove_as r1)), r1)\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  (_loc : Locf.t)  ->
                   (Sequence ((Repetition (remove_as r1)), r1) : 'regexp ) : 
                Tokenf.txt -> 'regexp -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
                Tokenf.pattern );
             Self;
             Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "r1\n";
           fn =
             (Gramf.mk_action
                (fun _  (r1 : 'regexp)  _  (_loc : Locf.t)  ->
                   (r1 : 'regexp ) : Tokenf.txt ->
                                       'regexp ->
                                         Tokenf.txt -> Locf.t -> 'regexp ))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern )];
           annot =
             "try Hashtbl.find Predef_lex.named_regexps x\nwith\n| Not_found  ->\n    (Fan_warnings.emitf xloc.loc_start\n       \"Reference to unbound regexp name `%s'\" x;\n     raise UnboundRegexp)\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let xloc = __fan_0.loc in
                   let x = __fan_0.txt in
                   (try Hashtbl.find Predef_lex.named_regexps x
                    with
                    | Not_found  ->
                        (Fan_warnings.emitf xloc.loc_start
                           "Reference to unbound regexp name `%s'" x;
                         raise UnboundRegexp) : 'regexp ) : Tokenf.txt ->
                                                              Locf.t ->
                                                                'regexp ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (char_class : 'char_class Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "^"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
            annot = "Fcset.complement r\n";
            fn =
              (Gramf.mk_action
                 (fun (r : 'char_class1)  _  (_loc : Locf.t)  ->
                    (Fcset.complement r : 'char_class ) : 'char_class1 ->
                                                            Tokenf.txt ->
                                                              Locf.t ->
                                                                'char_class ))
          };
         {
           symbols =
             [Nterm (Gramf.obj (char_class1 : 'char_class1 Gramf.t ))];
           annot = "r\n";
           fn =
             (Gramf.mk_action
                (fun (r : 'char_class1)  (_loc : Locf.t)  ->
                   (r : 'char_class ) : 'char_class1 -> Locf.t -> 'char_class ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (char_class1 : 'char_class1 Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                 Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
                Tokenf.pattern );
              Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
            annot =
              "let c1 = Char.code @@ (Escape.char c1) in\nlet c2 = Char.code @@ (Escape.char c2) in Fcset.interval c1 c2\n";
            fn =
              (Gramf.mk_action
                 (fun (__fan_2 : Tokenf.txt)  _  (__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let c1 = __fan_0.txt in
                    let c2 = __fan_2.txt in
                    (let c1 = Char.code @@ (Escape.char c1) in
                     let c2 = Char.code @@ (Escape.char c2) in
                     Fcset.interval c1 c2 : 'char_class1 ) : Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'char_class1 ))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
                Tokenf.pattern )];
           annot = "Fcset.singleton (Char.code @@ (Escape.char c1))\n";
           fn =
             (Gramf.mk_action
                (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let c1 = __fan_0.txt in
                   (Fcset.singleton (Char.code @@ (Escape.char c1)) : 
                     'char_class1 ) : Tokenf.txt -> Locf.t -> 'char_class1 ))
         };
         {
           symbols = [Self; Self];
           annot = "Fcset.union cc1 cc2\n";
           fn =
             (Gramf.mk_action
                (fun (cc2 : 'char_class1)  (cc1 : 'char_class1) 
                   (_loc : Locf.t)  -> (Fcset.union cc1 cc2 : 'char_class1 ) : 
                'char_class1 -> 'char_class1 -> Locf.t -> 'char_class1 ))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (declare_regexp : 'declare_regexp Gramf.t )
    ({
       label = None;
       lassoc = true;
       productions =
         [{
            symbols =
              [Token
                 ({
                    descr =
                      { tag = `Key; word = (A "let"); tag_name = "Key" }
                  } : Tokenf.pattern );
              Token
                ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
                Tokenf.pattern );
              Token
                ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
                Tokenf.pattern );
              Nterm (Gramf.obj (regexp : 'regexp Gramf.t ))];
            annot =
              "if Hashtbl.mem Predef_lex.named_regexps x\nthen\n  (Fan_warnings.emitf xloc.loc_start\n     \"fanlex (warning): multiple definition of named regexp '%s'\n\" x;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : Astf.stru ))\nelse\n  (Hashtbl.add Predef_lex.named_regexps x r;\n   (`StExp (_loc, (`Uid (_loc, \"()\"))) : Astf.stru ))\n";
            fn =
              (Gramf.mk_action
                 (fun (r : 'regexp)  _  (__fan_1 : Tokenf.txt)  _ 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (if Hashtbl.mem Predef_lex.named_regexps x
                     then
                       (Fan_warnings.emitf xloc.loc_start
                          "fanlex (warning): multiple definition of named regexp '%s'\n"
                          x;
                        (`StExp (_loc, (`Uid (_loc, "()"))) : Astf.stru ))
                     else
                       (Hashtbl.add Predef_lex.named_regexps x r;
                        (`StExp (_loc, (`Uid (_loc, "()"))) : Astf.stru )) : 
                      'declare_regexp ) : 'regexp ->
                                            Tokenf.txt ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Locf.t -> 'declare_regexp ))
          };
         {
           symbols = [Self; Self];
           annot = "x\n";
           fn =
             (Gramf.mk_action
                (fun (x : 'declare_regexp)  _  (_loc : Locf.t)  ->
                   (x : 'declare_regexp ) : 'declare_regexp ->
                                              'declare_regexp ->
                                                Locf.t -> 'declare_regexp ))
         }]
     } : Gramf.olevel )
let () =
  let domain = Ns.lang in
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
    ~name:{ domain; name = "lex" } ~entry:lex ();
  Ast_quotation.of_exp ~lexer:Lex_lex.from_stream
    ~name:{ domain; name = "lex_fan" } ~entry:lex_fan ();
  Ast_quotation.of_stru ~lexer:Lex_lex.from_stream
    ~name:{ domain; name = "regex" } ~entry:declare_regexp ();
  Ast_quotation.add_quotation ~lexer:Lex_lex.from_stream
    { domain; name = "re" } regexp ~mexp:meta_concrete_regexp
    ~mpat:meta_concrete_regexp
    ~exp_filter:(fun x  -> (x : Astf.ep  :>Astf.exp))
    ~pat_filter:(fun x  -> (x : Astf.ep  :>Astf.pat))
