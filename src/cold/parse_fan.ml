let mk_ant = Tokenf.mk_ant
let ident_of_ctyp = Fan_ops.ident_of_ctyp
let (<+>) = Ast_gen.( <+> ) 
let apply = Ast_gen.apply
let dot = Ast_gen.dot
let bar_of_list = Ast_gen.bar_of_list
let and_of_list = Ast_gen.and_of_list
let com_of_list = Ast_gen.com_of_list
let appl_of_list = Ast_gen.appl_of_list
open FAst
open! Syntaxf
let pos_exps = Gramf.mk "pos_exps"
let make_case exp pat =
  let grammar_entry_create x = Gramf.mk x in
  let pat_as_pat_opt: 'pat_as_pat_opt Gramf.t =
    grammar_entry_create "pat_as_pat_opt" in
  Gramf.extend_single (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Alias (_loc, p1, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(s : 'a_lident)  ~__fan_1:_ 
                    ~__fan_0:(p1 : 'pat)  (_loc : Locf.t)  ->
                    (`Alias (_loc, p1, s) : 'pat_as_pat_opt )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (p : 'pat_as_pat_opt )))))]) : Gramf.olevel ));
  Gramf.extend_single (case : 'case Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `List1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
               (`Token
                  ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" }
                   } : Tokenf.pattern )))],
            ("bar_of_list l\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case0 list)  ~__fan_0:_  (_loc : Locf.t)
                     -> (bar_of_list l : 'case )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                   (_loc : Locf.t)  -> (`Case (_loc, p, e) : 'case )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (case0 : 'case0 Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "case"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'case0 )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant s : 'case0 )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "when"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, (mk_ant s), w, e)\n",
             (Gramf.mk_action
                (fun ~__fan_4:(e : 'exp)  ~__fan_3:_  ~__fan_2:(w : 'exp) 
                   ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                   (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`CaseWhen (_loc, (mk_ant s), w, e) : 'case0 )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, (mk_ant s), e)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (`Case (_loc, (mk_ant s), e) : 'case0 )))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "when"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, p, w, e)\n",
             (Gramf.mk_action
                (fun ~__fan_4:(e : 'exp)  ~__fan_3:_  ~__fan_2:(w : 'exp) 
                   ~__fan_1:_  ~__fan_0:(p : 'pat_as_pat_opt) 
                   (_loc : Locf.t)  -> (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                   ~__fan_0:(p : 'pat_as_pat_opt)  (_loc : Locf.t)  ->
                   (`Case (_loc, p, e) : 'case0 )))))]) : Gramf.olevel ));
  Gramf.extend_single (case_quot : 'case_quot Gramf.t )
    (None,
      ((None, None,
         [([`List1sep
              ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                (`Token
                   ({
                      descr =
                        { tag = `Key; word = (A "|"); tag_name = "Key" }
                    } : Tokenf.pattern )))],
            ("bar_of_list x\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(x : 'case0 list)  (_loc : Locf.t)  ->
                    (bar_of_list x : 'case_quot )))))]) : Gramf.olevel ))
let make_semi atom nt =
  Gramf.extend_single (nt : 'nt Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (atom : 'atom Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(b2 : 'nt)  ~__fan_1:_  ~__fan_0:(b1 : 'atom) 
                    (_loc : Locf.t)  -> (`Sem (_loc, b1, b2) : 'nt )))));
         ([`Nterm (Gramf.obj (atom : 'atom Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun ~__fan_0:(b1 : 'atom)  (_loc : Locf.t)  -> (b1 : 'nt )))));
         ([`Nterm (Gramf.obj (atom : 'atom Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("b1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(b1 : 'atom)  (_loc : Locf.t)  ->
                   (b1 : 'nt )))))]) : Gramf.olevel ))
let make_comma atom nt =
  Gramf.extend_single (nt : 'nt Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Com (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'nt)  ~__fan_1:_  ~__fan_0:(p1 : 'nt) 
                    (_loc : Locf.t)  -> (`Com (_loc, p1, p2) : 'nt )))));
         ([`Nterm (Gramf.obj (atom : 'atom Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'atom)  (_loc : Locf.t)  -> (p : 'nt )))))]) : 
      Gramf.olevel ))
let make_pat exp =
  let grammar_entry_create x = Gramf.mk x in
  let pat_constr: 'pat_constr Gramf.t = grammar_entry_create "pat_constr" in
  Gramf.extend_single (pat_quot : 'pat_quot Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ))],
            ("`Com (_loc, x, y)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(y : 'comma_pat)  ~__fan_1:_ 
                    ~__fan_0:(x : 'pat)  (_loc : Locf.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ))],
           ("`Sem (_loc, x, y)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(y : 'sem_pat)  ~__fan_1:_  ~__fan_0:(x : 'pat)
                    (_loc : Locf.t)  -> (`Sem (_loc, x, y) : 'pat_quot )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun ~__fan_0:(x : 'pat)  (_loc : Locf.t)  ->
                   (x : 'pat_quot )))))]) : Gramf.olevel ));
  Gramf.extend_single (pat_constr : 'pat_constr Gramf.t )
    (None,
      ((None, None,
         [([`Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("(i : vid  :>pat)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'module_longident)  (_loc : Locf.t)  ->
                    ((i : vid  :>pat) : 'pat_constr )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))))]) : 
      Gramf.olevel ));
  Gramf.extend (pat : 'pat Gramf.t )
    (None,
      ([((Some "|"), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`Bar (_loc, p1, p2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(p2 : 'pat)  ~__fan_1:_  ~__fan_0:(p1 : 'pat)
                      (_loc : Locf.t)  -> (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A ".."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`PaRng (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'pat)  ~__fan_1:_  ~__fan_0:(p1 : 'pat) 
                    (_loc : Locf.t)  -> (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "::"), (Some `RA),
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "::"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p1, p2)))))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'pat)  ~__fan_1:_  ~__fan_0:(p1 : 'pat) 
                    (_loc : Locf.t)  ->
                    (`App
                       (_loc, (`Uid (_loc, "::")),
                         (`Par (_loc, (`Com (_loc, p1, p2))))) : 'pat )))))]);
       ((Some "apply"), (Some `LA),
         [([`Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t )); `Self],
            ("(`App (_loc, p1, p2) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(p2 : 'pat)  ~__fan_0:(p1 : 'pat_constr) 
                    (_loc : Locf.t)  ->
                    ((`App (_loc, p1, p2) : FAst.pat ) : 'pat )))));
         ([`Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p1 : 'pat_constr)  (_loc : Locf.t)  ->
                   (p1 : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "lazy"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self],
           ("`Lazy (_loc, p)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "int"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "int32"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "int64"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "flo"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "chr"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "nativeint"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "str"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "int'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "int32'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "int64'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "nativeint'"); tag_name = "Ant"
                  }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "flo'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "chr'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "str'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`int"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`int32"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`int64"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`nativeint"); tag_name = "Ant"
                  }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`flo"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`chr"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "`str"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i : vid  :>pat)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'vid)  (_loc : Locf.t)  ->
                   ((i : vid  :>pat) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
             Tokenf.pattern )],
           ("`Int (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Int32; word = Any; tag_name = "Int32" } } : 
             Tokenf.pattern )],
           ("`Int32 (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int32 (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Int64; word = Any; tag_name = "Int64" } } : 
             Tokenf.pattern )],
           ("`Int64 (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int64 (_loc, s) : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
              } : Tokenf.pattern )],
           ("`Nativeint (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Nativeint (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" } } : 
             Tokenf.pattern )],
           ("`Flo (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Flo (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
             Tokenf.pattern )],
           ("`Chr (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Chr (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
             Tokenf.pattern )],
           ("`Str (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Str (_loc, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
            Tokenf.pattern )],
           ("`Int (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Int32; word = Any; tag_name = "Int32" } } : 
            Tokenf.pattern )],
           ("`Int32 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int32 (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Int64; word = Any; tag_name = "Int64" } } : 
            Tokenf.pattern )],
           ("`Int64 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int64 (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({
               descr =
                 { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
             } : Tokenf.pattern )],
           ("`Nativeint (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Nativeint (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" } } : 
            Tokenf.pattern )],
           ("`Flo (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Flo (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Uid (_loc, \"[]\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "[]") : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (sem_pat_for_list : 'sem_pat_for_list Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("s\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(s : 'sem_pat_for_list)  ~__fan_0:_
                    (_loc : Locf.t)  -> (s : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "[|"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "|]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ArrayEmpty _loc : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "[|"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "|]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Array (_loc, pl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(pl : 'sem_pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`Array (_loc, pl) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Record (_loc, pl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(pl : 'label_pat_list)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`Record (_loc, pl) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Uid (_loc, \"()\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "()") : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(m : 'a_uident)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(pt : 'mtyp)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Ant; word = (Kind "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.ant)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_4 in
                   (`ModuleConstraint (_loc, m, (mk_ant s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(p : 'pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (p : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Constraint (_loc, p, t)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Constraint (_loc, p, t) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Alias (_loc, p, s)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(s : 'a_lident)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Alias (_loc, p, s) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Par (_loc, (`Com (_loc, p, pl)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(pl : 'comma_pat)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Par (_loc, (`Com (_loc, p, pl))) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("`ClassPath (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'type_longident)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ClassPath (_loc, i) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Label (_loc, i, p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(p : 'pat)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Label (_loc, i, p) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Label; word = Any; tag_name = "Label" } } : 
             Tokenf.pattern );
          `Self],
           ("`Label (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   (`Label (_loc, (`Lid (_loc, i)), p) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
             Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.pat : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
             Tokenf.pattern )],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`LabelS (_loc, i) : 'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'pat_tcon)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                     'pat )))));
         ([`Token
             ({
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                   ~__fan_2:(p : 'pat_tcon)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   let e = Some e in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                     'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_ 
                   ~__fan_2:_  ~__fan_1:(i : 'a_lident)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(e : 'exp)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Ant; word = (Kind "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.ant)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let s = __fan_6 in
                   (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`OptLablS (_loc, i) : 'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'pat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                   ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'pat )))))])] : Gramf.olevel list ));
  Gramf.extend_single (ipat : 'ipat Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("(`Record (_loc, pl) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(pl : 'label_pat_list)  ~__fan_0:_
                     (_loc : Locf.t)  ->
                    ((`Record (_loc, pl) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Uid (_loc, \"()\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "()") : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match pt with\n| None  -> `ModuleUnpack (_loc, m)\n| Some pt -> `ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(m : 'a_uident)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   let pt = None in
                   (match pt with
                    | None  -> `ModuleUnpack (_loc, m)
                    | Some pt ->
                        `ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                     'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match pt with\n| None  -> `ModuleUnpack (_loc, m)\n| Some pt -> `ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(pt : 'mtyp)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let pt = Some pt in
                   (match pt with
                    | None  -> `ModuleUnpack (_loc, m)
                    | Some pt ->
                        `ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                     'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Ant; word = (Kind "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.ant)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_4 in
                   (`ModuleConstraint (_loc, m, (mk_ant s)) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(p : 'pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (p : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(s : 'a_lident)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Alias (_loc, p, s) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (comma_ipat : 'comma_ipat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("(`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(pl : 'comma_ipat)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Par (_loc, (`Com (_loc, p, pl))) : FAst.pat ) : 
                   'ipat )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(s : alident  :>pat)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(s : 'a_lident)  (_loc : Locf.t)  ->
                   ((s : alident  :>pat) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Label; word = Any; tag_name = "Label" } } : 
             Tokenf.pattern );
          `Self],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'ipat)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                     'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_3:(p : 'ipat)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
             Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.pat : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
             Tokenf.pattern )],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`LabelS (_loc, i) : 'ipat )))));
         ([`Token
             ({
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'pat_tcon)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                     'ipat )))));
         ([`Token
             ({
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                   ~__fan_2:(p : 'pat_tcon)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   let e = Some e in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                     'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_ 
                   ~__fan_2:_  ~__fan_1:(i : 'a_lident)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(e : 'exp)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Ant; word = (Kind "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.ant)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let s = __fan_6 in
                   (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`OptLablS (_loc, i) : 'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'ipat )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                   ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'ipat )))))]) : Gramf.olevel ));
  Gramf.extend_single (sem_pat_for_list : 'sem_pat_for_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p, pl)))))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(pl : 'sem_pat_for_list)  ~__fan_1:_ 
                    ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                    (`App
                       (_loc, (`Uid (_loc, "::")),
                         (`Par (_loc, (`Com (_loc, p, pl))))) : 'sem_pat_for_list )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (`App
                      (_loc, (`Uid (_loc, "::")),
                        (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                   'sem_pat_for_list )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (`App
                      (_loc, (`Uid (_loc, "::")),
                        (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                   'sem_pat_for_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (pat_tcon : 'pat_tcon Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Constraint (_loc, p, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                    (_loc : Locf.t)  ->
                    (`Constraint (_loc, p, t) : 'pat_tcon )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (p : 'pat_tcon )))))]) : Gramf.olevel ));
  Gramf.extend_single (ipat_tcon : 'ipat_tcon Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat_tcon )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i : alident  :>pat)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i : alident  :>pat) : 'ipat_tcon )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("(`Constraint (_loc, (i : alident  :>pat), t) : pat )\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, (i : alident  :>pat), t) : pat ) : 
                   'ipat_tcon )))))]) : Gramf.olevel ));
  Gramf.extend_single (label_pat_list : 'label_pat_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'label_pat_list)  ~__fan_1:_ 
                    ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(p1 : 'label_pat) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:_  ~__fan_1:_ 
                   ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ))],
           ("p1\n",
             (Gramf.mk_action
                (fun ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t)  ->
                   (p1 : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("p1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t) 
                   -> (p1 : 'label_pat_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (label_pat : 'label_pat Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"pat" s : 'label_pat )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'label_pat )))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
           ("let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'label_longident)  (_loc : Locf.t)  ->
                   let p = None in
                   (let p =
                      match p with
                      | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                      | Some p -> p in
                    `RecBind (_loc, i, p) : 'label_pat )))));
         ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ))],
           ("let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(p : 'pat)  ~__fan_1:_ 
                   ~__fan_0:(i : 'label_longident)  (_loc : Locf.t)  ->
                   let p = Some p in
                   (let p =
                      match p with
                      | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                      | Some p -> p in
                    `RecBind (_loc, i, p) : 'label_pat )))))]) : Gramf.olevel ))
let () =
  make_semi field_exp field_exp_list;
  make_semi exp sem_exp;
  make_semi label_exp label_exp_list;
  make_semi pat sem_pat;
  make_semi clfield clfield_quot;
  make_semi clsigi clsigi_quot;
  make_comma pat comma_pat;
  make_comma ipat comma_ipat;
  make_comma exp comma_exp;
  make_case exp pat;
  make_pat exp
let apply () =
  (Gramf.extend_single (mexp_quot : 'mexp_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'mexp)  (_loc : Locf.t)  ->
                     (x : 'mexp_quot )))))]) : Gramf.olevel ));
   Gramf.extend (mbind0 : 'mbind0 Gramf.t )
     (None,
       ([(None, (Some `RA),
           [([`Token
                ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
                Tokenf.pattern );
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("`Functor (_loc, m, mt, mb)\n",
                (Gramf.mk_action
                   (fun ~__fan_5:(mb : 'mbind0)  ~__fan_4:_ 
                      ~__fan_3:(mt : 'mtyp)  ~__fan_2:_ 
                      ~__fan_1:(m : 'a_uident)  ~__fan_0:_  (_loc : Locf.t) 
                      -> (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
             ("`Constraint (_loc, me, mt)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(me : 'mexp)  ~__fan_2:_ 
                     ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Constraint (_loc, me, mt) : 'mbind0 )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
             ("me\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (me : 'mbind0 )))))])] : Gramf.olevel list ));
   Gramf.extend (mexp : 'mexp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Token
                ({
                   descr =
                     { tag = `Key; word = (A "functor"); tag_name = "Key" }
                 } : Tokenf.pattern );
             `Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Token
               ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("`Functor (_loc, i, t, me)\n",
                (Gramf.mk_action
                   (fun ~__fan_7:(me : 'mexp)  ~__fan_6:_  ~__fan_5:_ 
                      ~__fan_4:(t : 'mtyp)  ~__fan_3:_ 
                      ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                      (_loc : Locf.t)  ->
                      (`Functor (_loc, i, t, me) : 'mexp )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "struct"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("match st with | Some st -> `Struct (_loc, st) | None  -> `StructEnd _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     let st = None in
                     (match st with
                      | Some st -> `Struct (_loc, st)
                      | None  -> `StructEnd _loc : 'mexp )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "struct"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (strus : 'strus Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("match st with | Some st -> `Struct (_loc, st) | None  -> `StructEnd _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_2:_  ~__fan_1:(st : 'strus)  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     let st = Some st in
                     (match st with
                      | Some st -> `Struct (_loc, st)
                      | None  -> `StructEnd _loc : 'mexp )))))]);
        ((Some "apply"), None,
          [([`Self; `Self],
             ("`App (_loc, me1, me2)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(me2 : 'mexp)  ~__fan_0:(me1 : 'mexp) 
                     (_loc : Locf.t)  -> (`App (_loc, me1, me2) : 'mexp )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"mexp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"mexp" s : 'mexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mexp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mexp" s : 'mexp )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mexp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mexp : 'mexp )))));
          ([`Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("(i :>mexp)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'module_longident)  (_loc : Locf.t)  ->
                    ((i :>mexp) : 'mexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Constraint (_loc, me, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(mt : 'mtyp)  ~__fan_2:_ 
                    ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, me, mt) : 'mexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("me\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(me : 'mexp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (me : 'mexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`PackageModule (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:_
                     (_loc : Locf.t)  -> (`PackageModule (_loc, e) : 
                    'mexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p)))))\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(p : 'mtyp)  ~__fan_3:_ 
                    ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`PackageModule
                       (_loc, (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                    'mexp )))))])] : Gramf.olevel list )));
  (Gramf.extend_single (mbind_quot : 'mbind_quot Gramf.t )
     (None,
       ((None, None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`And (_loc, b1, b2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(b2 : 'mbind_quot)  ~__fan_1:_ 
                     ~__fan_0:(b1 : 'mbind_quot)  (_loc : Locf.t)  ->
                     (`And (_loc, b1, b2) : 'mbind_quot )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind_quot )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind_quot )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`Constraint (_loc, m, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mt : 'mtyp)  ~__fan_1:_ 
                    ~__fan_0:(m : 'a_uident)  (_loc : Locf.t)  ->
                    (`Constraint (_loc, m, mt) : 'mbind_quot )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`ModuleBind (_loc, m, mt, me)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(me : 'mexp)  ~__fan_3:_ 
                    ~__fan_2:(mt : 'mtyp)  ~__fan_1:_ 
                    ~__fan_0:(m : 'a_uident)  (_loc : Locf.t)  ->
                    (`ModuleBind (_loc, m, mt, me) : 'mbind_quot )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (mbind : 'mbind Gramf.t )
     (None,
       ((None, None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`And (_loc, b1, b2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(b2 : 'mbind)  ~__fan_1:_ 
                     ~__fan_0:(b1 : 'mbind)  (_loc : Locf.t)  ->
                     (`And (_loc, b1, b2) : 'mbind )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mbind\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mbind : 'mbind )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`ModuleBind (_loc, m, mt, me)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(me : 'mexp)  ~__fan_3:_ 
                    ~__fan_2:(mt : 'mtyp)  ~__fan_1:_ 
                    ~__fan_0:(m : 'a_uident)  (_loc : Locf.t)  ->
                    (`ModuleBind (_loc, m, mt, me) : 'mbind )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single
     (module_rec_declaration : 'module_rec_declaration Gramf.t )
     (None,
       ((None, None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`And (_loc, m1, m2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(m2 : 'module_rec_declaration)  ~__fan_1:_ 
                     ~__fan_0:(m1 : 'module_rec_declaration)  (_loc : Locf.t)
                      -> (`And (_loc, m1, m2) : 'module_rec_declaration )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"mbind" s : 'module_rec_declaration )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"mbind" s : 'module_rec_declaration )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mbind\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mbind : 'module_rec_declaration )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`Constraint (_loc, m, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mt : 'mtyp)  ~__fan_1:_ 
                    ~__fan_0:(m : 'a_uident)  (_loc : Locf.t)  ->
                    (`Constraint (_loc, m, mt) : 'module_rec_declaration )))))]) : 
       Gramf.olevel )));
  (Gramf.extend_single (constr_quot : 'constr_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (constr : 'constr Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'constr)  (_loc : Locf.t)  ->
                     (x : 'constr_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (constr : 'constr Gramf.t )
     (None,
       ((None, None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`And (_loc, wc1, wc2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(wc2 : 'constr)  ~__fan_1:_ 
                     ~__fan_0:(wc1 : 'constr)  (_loc : Locf.t)  ->
                     (`And (_loc, wc1, wc2) : 'constr )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"constr\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"constr" s : 'constr )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "constr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"constr\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"constr" s : 'constr )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.constr\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.constr : 'constr )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("match p with\n| Some _ -> `TypeEqPriv (_loc, t1, t2)\n| None  -> `TypeEq (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'type_longident_and_parameters) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    let p = None in
                    (match p with
                     | Some _ -> `TypeEqPriv (_loc, t1, t2)
                     | None  -> `TypeEq (_loc, t1, t2) : 'constr )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Key; word = (A "private"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("match p with\n| Some _ -> `TypeEqPriv (_loc, t1, t2)\n| None  -> `TypeEq (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(t2 : 'ctyp)  ~__fan_3:(p : Tokenf.txt) 
                    ~__fan_2:_ 
                    ~__fan_1:(t1 : 'type_longident_and_parameters) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    let p = Some p in
                    (match p with
                     | Some _ -> `TypeEqPriv (_loc, t1, t2)
                     | None  -> `TypeEq (_loc, t1, t2) : 'constr )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`TypeSubst (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'type_longident_and_parameters) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`TypeSubst (_loc, t1, t2) : 'constr )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gramf.t ))],
            ("let i = (i1 : vid  :>ident) in\nif v = \"=\" then `ModuleEq (_loc, i, i2) else `ModuleSubst (_loc, i, i2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(i2 : 'module_longident_with_app) 
                    ~__fan_2:(__fan_2 : Tokenf.txt) 
                    ~__fan_1:(i1 : 'module_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let v = __fan_2.txt in
                    (let i = (i1 : vid  :>ident) in
                     if v = "="
                     then `ModuleEq (_loc, i, i2)
                     else `ModuleSubst (_loc, i, i2) : 'constr )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (module_longident_with_app : 'module_longident_with_app
                                               Gramf.t ))],
            ("let i = (i1 : vid  :>ident) in\nif v = \"=\" then `ModuleEq (_loc, i, i2) else `ModuleSubst (_loc, i, i2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(i2 : 'module_longident_with_app) 
                    ~__fan_2:(__fan_2 : Tokenf.txt) 
                    ~__fan_1:(i1 : 'module_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let v = __fan_2.txt in
                    (let i = (i1 : vid  :>ident) in
                     if v = "="
                     then `ModuleEq (_loc, i, i2)
                     else `ModuleSubst (_loc, i, i2) : 'constr )))))]) : 
       Gramf.olevel )));
  (Gramf.extend_single (sigis : 'sigis Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"sigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigis )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigis )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(sg : 'sigis)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(sg : 'sigis)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(sg : 'sigis)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(sg : 'sigis)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Self],
            ("`Sem (_loc, sg, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'sigis)  ~__fan_0:(sg : 'sigi) 
                    (_loc : Locf.t)  -> (`Sem (_loc, sg, s) : 'sigis )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, sg, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(s : 'sigis)  ~__fan_1:_ 
                    ~__fan_0:(sg : 'sigi)  (_loc : Locf.t)  ->
                    (`Sem (_loc, sg, s) : 'sigis )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
            ("sg\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(sg : 'sigi)  (_loc : Locf.t)  ->
                    (sg : 'sigis )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("sg\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(sg : 'sigi)  (_loc : Locf.t)  ->
                    (sg : 'sigis )))))]) : Gramf.olevel ));
   Gramf.extend (mtyp : 'mtyp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Token
                ({
                   descr =
                     { tag = `Key; word = (A "functor"); tag_name = "Key" }
                 } : Tokenf.pattern );
             `Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self;
             `Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Token
               ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("`Functor (_loc, i, t, mt)\n",
                (Gramf.mk_action
                   (fun ~__fan_7:(mt : 'mtyp)  ~__fan_6:_  ~__fan_5:_ 
                      ~__fan_4:(t : 'mtyp)  ~__fan_3:_ 
                      ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                      (_loc : Locf.t)  ->
                      (`Functor (_loc, i, t, mt) : 'mtyp )))))]);
        ((Some "with"), None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (constr : 'constr Gramf.t ))],
             ("`With (_loc, mt, wc)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(wc : 'constr)  ~__fan_1:_ 
                     ~__fan_0:(mt : 'mtyp)  (_loc : Locf.t)  ->
                     (`With (_loc, mt, wc) : 'mtyp )))))]);
        ((Some "apply"), None,
          [([`Self; `Self],
             ("match (mt1, mt2) with\n| ((#ident as i1),(#ident as i2)) -> apply i1 i2\n| _ -> raise Streamf.NotConsumed\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(mt2 : 'mtyp)  ~__fan_0:(mt1 : 'mtyp) 
                     (_loc : Locf.t)  ->
                     (match (mt1, mt2) with
                      | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                      | _ -> raise Streamf.NotConsumed : 'mtyp )))))]);
        ((Some "."), None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("let acc0 mt1 mt2 =\n  match (mt1, mt2) with\n  | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n  | _ -> raise Streamf.NotConsumed in\nacc0 mt1 mt2\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(mt2 : 'mtyp)  ~__fan_1:_ 
                     ~__fan_0:(mt1 : 'mtyp)  (_loc : Locf.t)  ->
                     (let acc0 mt1 mt2 =
                        match (mt1, mt2) with
                        | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                        | _ -> raise Streamf.NotConsumed in
                      acc0 mt1 mt2 : 'mtyp )))))]);
        ((Some "sig"), None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "sig"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("match sg with | Some sg -> `Sig (_loc, sg) | None  -> `SigEnd _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     let sg = None in
                     (match sg with
                      | Some sg -> `Sig (_loc, sg)
                      | None  -> `SigEnd _loc : 'mtyp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "sig"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (sigis : 'sigis Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match sg with | Some sg -> `Sig (_loc, sg) | None  -> `SigEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(sg : 'sigis)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let sg = Some sg in
                    (match sg with
                     | Some sg -> `Sig (_loc, sg)
                     | None  -> `SigEnd _loc : 'mtyp )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"mtyp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"mtyp" s : 'mtyp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mtyp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mtyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mtyp" s : 'mtyp )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mtyp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mtyp : 'mtyp )))));
          ([`Nterm
              (Gramf.obj
                 (module_longident_with_app : 'module_longident_with_app
                                                Gramf.t ))],
            ("(i : ident  :>mtyp)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'module_longident_with_app) 
                    (_loc : Locf.t)  -> ((i : ident  :>mtyp) : 'mtyp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mt\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(mt : 'mtyp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (mt : 'mtyp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "of"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
            ("`ModuleTypeOf (_loc, me)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(me : 'mexp)  ~__fan_2:_  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ModuleTypeOf (_loc, me) : 'mtyp )))))])] : Gramf.olevel
                                                                   list ));
   Gramf.extend_single (module_declaration : 'module_declaration Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
             ("mt\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (mt : 'module_declaration )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Functor (_loc, i, t, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(mt : 'module_declaration)  ~__fan_4:_ 
                    ~__fan_3:(t : 'mtyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_uident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Functor (_loc, i, t, mt) : 'module_declaration )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (mtyp_quot : 'mtyp_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'mtyp)  (_loc : Locf.t)  ->
                     (x : 'mtyp_quot )))))]) : Gramf.olevel )));
  ();
  (Gramf.extend_single (sigi_quot : 'sigi_quot Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
             ("`DirectiveSimple (_loc, s)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(s : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Directive (_loc, s, dp)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(dp : 'exp)  ~__fan_1:(s : 'a_lident) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Directive (_loc, s, dp) : 'sigi_quot )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, sg1, sg2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(sg2 : 'sigi_quot)  ~__fan_1:_ 
                    ~__fan_0:(sg1 : 'sigi)  (_loc : Locf.t)  ->
                    (`Sem (_loc, sg1, sg2) : 'sigi_quot )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))],
            ("sg\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(sg : 'sigi)  (_loc : Locf.t)  ->
                    (sg : 'sigi_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (sigi : 'sigi Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"sigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigi )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.sigi\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.sigi : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "include"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`Include (_loc, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Include (_loc, mt) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm
             (Gramf.obj (module_declaration : 'module_declaration Gramf.t ))],
            ("`Module (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mt : 'module_declaration) 
                    ~__fan_1:(i : 'a_uident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Module (_loc, i, mt) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "rec"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (module_rec_declaration : 'module_rec_declaration Gramf.t ))],
            ("`RecModule (_loc, mb)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mb : 'module_rec_declaration)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`RecModule (_loc, mb) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
            ("`ModuleTypeEnd (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ModuleTypeEnd (_loc, i) : 'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'module_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = None in
                    (`Open
                       (_loc,
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc), (i : vid  :>ident)) : 
                      'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
            ("`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'module_longident) 
                    ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_  (_loc : Locf.t)
                     ->
                    let bang = Some bang in
                    (`Open
                       (_loc,
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc), (i : vid  :>ident)) : 
                      'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
            ("`Type (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'type_declaration)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Type (_loc, t) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`ModuleType (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(mt : 'mtyp)  ~__fan_3:_ 
                    ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ModuleType (_loc, i, mt) : 
                    'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "class"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
            ("`ClassType (_loc, ctd)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(ctd : 'cltyp_declaration)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ClassType (_loc, ctd) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "exception"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (constructor_declaration : 'constructor_declaration Gramf.t ))],
            ("`Exception (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'constructor_declaration)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Exception (_loc, t) : 'sigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "external"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
            ("`External (_loc, i, t, sl)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(sl : 'string_list)  ~__fan_4:_ 
                    ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`External (_loc, i, t, sl) : 'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Val (_loc, i, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Val (_loc, i, t) : 'sigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "class"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm
             (Gramf.obj (class_description : 'class_description Gramf.t ))],
            ("`Class (_loc, cd)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(cd : 'class_description)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Class (_loc, cd) : 'sigi )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (interf : 'interf Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Self],
             ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(rest : 'interf)  ~__fan_0:(si : 'sigi) 
                     (_loc : Locf.t)  ->
                     (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                     'interf )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(rest : 'interf)  ~__fan_1:_ 
                    ~__fan_0:(si : 'sigi)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'interf )))));
          ([`Token
              ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
              Tokenf.pattern )],
            ("([], None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  -> (([], None) : 'interf )))))]) : 
       Gramf.olevel )));
  (let grammar_entry_create x = Gramf.mk x in
   let name_space: 'name_space Gramf.t = grammar_entry_create "name_space"
   and fun_def_pat: 'fun_def_pat Gramf.t = grammar_entry_create "fun_def_pat" in
   Gramf.extend_single (exp_quot : 'exp_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ))],
             ("`Com (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'comma_exp)  ~__fan_1:_ 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     (`Com (_loc, e1, e2) : 'exp_quot )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ))],
            ("`Sem (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'sem_exp)  ~__fan_1:_ 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Sem (_loc, e1, e2) : 'exp_quot )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    (e : 'exp_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (cvalue_bind : 'cvalue_bind Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("e\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (e : 'cvalue_bind )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj (unquoted_typevars : 'unquoted_typevars Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let u: FAst.ctyp = `TyPol (_loc, t1, t2) in\n(`Constraint (_loc, e, u) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_6:(e : 'exp)  ~__fan_5:_  ~__fan_4:(t2 : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(t1 : 'unquoted_typevars) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (let u: FAst.ctyp = `TyPol (_loc, t1, t2) in
                     (`Constraint (_loc, e, u) : FAst.exp ) : 'cvalue_bind )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`Constraint (_loc, e, t) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_  ~__fan_1:(t : 'ctyp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Constraint (_loc, e, t) : FAst.exp ) : 'cvalue_bind )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":>"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("match t with\n| (`TyPol (_loc,_,_) : FAst.ctyp) ->\n    raise (Streamf.Error \"unexpected polytype here\")\n| _ -> (`Coercion (_loc, e, t, t2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(e : 'exp)  ~__fan_4:_  ~__fan_3:(t2 : 'ctyp) 
                    ~__fan_2:_  ~__fan_1:(t : 'ctyp)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (match t with
                     | (`TyPol (_loc,_,_) : FAst.ctyp) ->
                         raise (Streamf.Error "unexpected polytype here")
                     | _ -> (`Coercion (_loc, e, t, t2) : FAst.exp ) : 
                    'cvalue_bind )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ":>"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Subtype (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_  ~__fan_1:(t : 'ctyp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Subtype (_loc, e, t) : 'cvalue_bind )))))]) : Gramf.olevel ));
   Gramf.extend (fun_bind : 'fun_bind Gramf.t )
     (None,
       ([(None, (Some `RA),
           [([`Token
                ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
                Tokenf.pattern );
             `Token
               ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
                } : Tokenf.pattern );
             `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("`LocalTypeFun (_loc, i, e)\n",
                (Gramf.mk_action
                   (fun ~__fan_4:(e : 'fun_bind)  ~__fan_3:_ 
                      ~__fan_2:(i : 'a_lident)  ~__fan_1:_  ~__fan_0:_ 
                      (_loc : Locf.t)  ->
                      (`LocalTypeFun (_loc, i, e) : 'fun_bind )))));
           ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
             ("`Fun (_loc, (`Case (_loc, p, e)))\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'fun_bind)  ~__fan_0:(p : 'ipat) 
                     (_loc : Locf.t)  ->
                     (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_bind )))));
           ([`Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
             ("bi\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(bi : 'cvalue_bind)  (_loc : Locf.t)  ->
                     (bi : 'fun_bind )))))])] : Gramf.olevel list ));
   Gramf.extend_single (lang : 'lang Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
             ("let old = !Ast_quotation.default in\nmatch Ast_quotation.resolve_name ls with\n| Some x -> (Ast_quotation.default := (Some x); old)\n| None  ->\n    Locf.failf _loc \"DDSL `%s' can not be resolved\"\n      (Tokenf.string_of_name ls)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(ls : 'dot_lstrings)  (_loc : Locf.t)  ->
                     (let old = !Ast_quotation.default in
                      match Ast_quotation.resolve_name ls with
                      | Some x -> (Ast_quotation.default := (Some x); old)
                      | None  ->
                          Locf.failf _loc "DDSL `%s' can not be resolved"
                            (Tokenf.string_of_name ls) : 'lang )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (pos_exps : 'pos_exps Gramf.t )
     (None,
       ((None, None,
          [([`List1sep
               ((`Nterm (Gramf.obj (name_space : 'name_space Gramf.t ))),
                 (`Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )))],
             ("let old = !Ast_quotation.map in\nAst_quotation.map := (Mapf.String.add_list xys old); old\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(xys : 'name_space list)  (_loc : Locf.t)  ->
                     (let old = !Ast_quotation.map in
                      Ast_quotation.map := (Mapf.String.add_list xys old);
                      old : 'pos_exps )))))]) : Gramf.olevel ));
   Gramf.extend_single (name_space : 'name_space Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))],
             ("(x,\n  (match Ast_quotation.resolve_name y with\n   | None  ->\n       Locf.failf _loc \"DDSL `%s' can not be resolved\"\n         (Tokenf.string_of_name y)\n   | Some x -> x))\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(y : 'dot_lstrings)  ~__fan_1:_ 
                     ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let x = __fan_0.txt in
                     ((x,
                        (match Ast_quotation.resolve_name y with
                         | None  ->
                             Locf.failf _loc "DDSL `%s' can not be resolved"
                               (Tokenf.string_of_name y)
                         | Some x -> x)) : 'name_space )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("(x,\n  (match Ast_quotation.resolve_name ((`Sub []), x) with\n   | None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" x\n   | Some x -> x))\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    ((x,
                       (match Ast_quotation.resolve_name ((`Sub []), x) with
                        | None  ->
                            Locf.failf _loc "DDSL `%s' can not be resolved" x
                        | Some x -> x)) : 'name_space )))))]) : Gramf.olevel ));
   Gramf.extend_single (fun_def_pat : 'fun_def_pat Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("fun e  -> `LocalTypeFun (_loc, i, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:_  ~__fan_2:(i : 'a_lident)  ~__fan_1:_ 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat )))));
          ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t ))],
            ("fun e  -> `Fun (_loc, (`Case (_loc, p, e)))\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(p : 'ipat)  (_loc : Locf.t)  ->
                    (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 'fun_def_pat )))));
          ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "when"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e)))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(w : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'ipat) 
                    (_loc : Locf.t)  ->
                    (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                    'fun_def_pat )))))]) : Gramf.olevel ));
   Gramf.extend (fun_def : 'fun_def Gramf.t )
     (None,
       ([(None, (Some `RA),
           [([`Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
              ("f e\n",
                (Gramf.mk_action
                   (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                      ~__fan_0:(f : 'fun_def_pat)  (_loc : Locf.t)  ->
                      (f e : 'fun_def )))));
           ([`Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t )); `Self],
             ("f e\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'fun_def)  ~__fan_0:(f : 'fun_def_pat) 
                     (_loc : Locf.t)  -> (f e : 'fun_def )))))])] : Gramf.olevel
                                                                    list ));
   Gramf.extend (exp : 'exp Gramf.t )
     (None,
       ([((Some "top"), (Some `RA),
           [([`Token
                ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                 } : Tokenf.pattern );
             `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
             `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
             `Token
               ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("`LetIn (_loc, r, bi, x)\n",
                (Gramf.mk_action
                   (fun ~__fan_4:(x : 'exp)  ~__fan_3:_ 
                      ~__fan_2:(bi : 'bind)  ~__fan_1:(r : 'opt_rec) 
                      ~__fan_0:_  (_loc : Locf.t)  ->
                      (`LetIn (_loc, r, bi, x) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`LetModule (_loc, m, mb, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(mb : 'mbind0)  ~__fan_2:(m : 'a_uident) 
                     ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`LetModule (_loc, m, mb, e) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                     ~__fan_2:(i : 'module_longident)  ~__fan_1:_  ~__fan_0:_
                      (_loc : Locf.t)  ->
                     let bang = None in
                     (`LetOpen
                        (_loc,
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc), (i : vid  :>ident), e) : 
                       'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(i : 'module_longident) 
                     ~__fan_2:(bang : Tokenf.txt)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     let bang = Some bang in
                     (`LetOpen
                        (_loc,
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc), (i : vid  :>ident), e) : 
                       'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "try"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`LetTryInWith (_loc, r, bi, x, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_7:(a : 'case)  ~__fan_6:_  ~__fan_5:(x : 'exp) 
                     ~__fan_4:_  ~__fan_3:(bi : 'bind) 
                     ~__fan_2:(r : 'opt_rec)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "match"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`Match (_loc, e, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(a : 'case)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Match (_loc, e, a) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "try"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`Try (_loc, e, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(a : 'case)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Try (_loc, e, a) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "if"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "then"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "else"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Self],
             ("`IfThenElse (_loc, e1, e2, e3)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e3 : 'exp)  ~__fan_4:_  ~__fan_3:(e2 : 'exp)
                      ~__fan_2:_  ~__fan_1:(e1 : 'exp)  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "if"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "then"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Self],
             ("`IfThen (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(e2 : 'exp)  ~__fan_2:_  ~__fan_1:(e1 : 'exp)
                      ~__fan_0:_  (_loc : Locf.t)  ->
                     (`IfThen (_loc, e1, e2) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "do"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "done"); tag_name = "Key" }
               } : Tokenf.pattern )],
             ("`Seq (_loc, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:_  ~__fan_1:(seq : 'sequence)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Seq (_loc, seq) : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (lang : 'lang Gramf.t ));
            `Self],
             ("Ast_quotation.default := old; x\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(x : 'exp)  ~__fan_1:(old : 'lang) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (Ast_quotation.default := old; x : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (pos_exps : 'pos_exps Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("Ast_quotation.map := old; x\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(x : 'exp)  ~__fan_3:_ 
                     ~__fan_2:(old : 'pos_exps)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (Ast_quotation.map := old; x : 'exp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "for"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self;
            `Nterm (Gramf.obj (flag : 'flag Gramf.t ));
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "do"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "done"); tag_name = "Key" }
               } : Tokenf.pattern )],
             ("`For (_loc, i, e1, e2, df, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_8:_  ~__fan_7:(seq : 'sequence)  ~__fan_6:_ 
                     ~__fan_5:(e2 : 'exp)  ~__fan_4:(df : 'flag) 
                     ~__fan_3:(e1 : 'exp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "while"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A "do"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "done"); tag_name = "Key" }
               } : Tokenf.pattern )],
             ("`While (_loc, e, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:_  ~__fan_3:(seq : 'sequence)  ~__fan_2:_ 
                     ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`While (_loc, e, seq) : 'exp )))))]);
        ((Some ":="), (Some `NA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A ":="); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "<-"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("match Fan_ops.bigarray_set _loc e1 e2 with\n| Some e -> e\n| None  -> `Assign (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:_  ~__fan_0:(e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    (match Fan_ops.bigarray_set _loc e1 e2 with
                     | Some e -> e
                     | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
        ((Some "||"), (Some `RA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "or"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "||"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))))]);
        ((Some "&&"), (Some `RA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "&"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "&&"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))))]);
        ((Some "<"), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Inf; word = (Level 0); tag_name = "Inf" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.op) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "=="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "<"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A ">"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))))]);
        ((Some "^"), (Some `RA),
          [([`Self;
            `Token
              ({ descr = { tag = `Inf; word = (Level 1); tag_name = "Inf" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.op) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))))]);
        ((Some "::"), (Some `RA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "::"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Uid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Uid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))))]);
        ((Some "+"), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Inf; word = (Level 2); tag_name = "Inf" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.op) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "+"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "-."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))))]);
        ((Some "*"), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Inf; word = (Level 3); tag_name = "Inf" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.op) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))))]);
        ((Some "**"), (Some `RA),
          [([`Self;
            `Token
              ({ descr = { tag = `Inf; word = (Level 4); tag_name = "Inf" } } : 
              Tokenf.pattern );
            `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.op) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))))]);
        ((Some "obj"), (Some `RA),
          [([`Token
               ({ descr = { tag = `Key; word = (A "fun"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `List1sep
              ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                (`Token
                   ({
                      descr =
                        { tag = `Key; word = (A "|"); tag_name = "Key" }
                    } : Tokenf.pattern )))],
             ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(a : 'case0 list)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (let cases = bar_of_list a in `Fun (_loc, cases) : 
                     'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "function"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `List1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
               (`Token
                  ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" }
                   } : Tokenf.pattern )))],
            ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(a : 'case0 list)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "fun"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_def)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (e : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "function"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_def)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (e : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_
                     ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = None in
                    (match cst with
                     | Some cst -> `ObjPat (_loc, p, cst)
                     | None  -> `ObjPatEnd (_loc, p) : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(cst : 'class_structure) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let cst = Some cst in
                    (match cst with
                     | Some cst -> `ObjPat (_loc, p, cst)
                     | None  -> `ObjPatEnd (_loc, p) : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with\n| Some cst -> `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n| None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:_  ~__fan_4:(t : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let cst = None in
                    (match cst with
                     | Some cst ->
                         `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)
                     | None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                      'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with\n| Some cst -> `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n| None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
              (Gramf.mk_action
                 (fun ~__fan_7:_  ~__fan_6:(cst : 'class_structure) 
                    ~__fan_5:_  ~__fan_4:(t : 'ctyp)  ~__fan_3:_ 
                    ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let cst = Some cst in
                    (match cst with
                     | Some cst ->
                         `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)
                     | None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                      'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = None in
                    (match cst with
                     | Some cst -> `Obj (_loc, cst)
                     | None  -> `ObjEnd _loc : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(cst : 'class_structure) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = Some cst in
                    (match cst with
                     | Some cst -> `Obj (_loc, cst)
                     | None  -> `ObjEnd _loc : 'exp )))))]);
        ((Some "unary minus"), (Some `NA),
          [([`Token
               ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Self],
             ("Fan_ops.mkumin _loc x e\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                     (_loc : Locf.t)  ->
                     let x = __fan_0.txt in (Fan_ops.mkumin _loc x e : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "-."); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self],
            ("Fan_ops.mkumin _loc x e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let x = __fan_0.txt in (Fan_ops.mkumin _loc x e : 'exp )))))]);
        ((Some "apply"), (Some `LA),
          [([`Self; `Self],
             ("`App (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e2 : 'exp)  ~__fan_0:(e1 : 'exp) 
                     (_loc : Locf.t)  -> (`App (_loc, e1, e2) : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "assert"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Self],
            ("`Assert (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Assert (_loc, e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "new"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
            ("`New (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'class_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`New (_loc, i) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "lazy"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Self],
            ("`Lazy (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Lazy (_loc, e) : 'exp )))))]);
        ((Some "label"), (Some `NA),
          [([`Token
               ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`Label (_loc, i, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(e : 'exp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`Label (_loc, i, e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`LabelS (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`LabelS (_loc, i) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Label; word = Any; tag_name = "Label" } } : 
              Tokenf.pattern );
           `Self],
            ("(`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Label (_loc, (`Lid (_loc, i)), e) : FAst.exp ) : 
                      'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
               } : Tokenf.pattern );
           `Self],
            ("`OptLabl (_loc, (`Lid (_loc, i)), e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`OptLabl (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OptLabl (_loc, i, e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`OptLablS (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`OptLablS (_loc, i) : 'exp )))))]);
        ((Some "."), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self;
            `Token
              ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("`ArrayDot (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:_  ~__fan_3:(e2 : 'exp)  ~__fan_2:_ 
                     ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     (`ArrayDot (_loc, e1, e2) : 'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`StringDot (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(e2 : 'exp)  ~__fan_2:_ 
                    ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (`StringDot (_loc, e1, e2) : 'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("Fan_ops.bigarray_get _loc e1 e2\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(e2 : 'comma_exp)  ~__fan_2:_ 
                    ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (Fan_ops.bigarray_get _loc e1 e2 : 'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
            ("`Field (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Field (_loc, e1, e2) : 'exp )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Send (_loc, e, lab)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(lab : 'a_lident)  ~__fan_1:_ 
                    ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    (`Send (_loc, e, lab) : 'exp )))))]);
        ((Some "~-"), (Some `NA),
          [([`Token
               ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Self],
             ("`App (_loc, (`Lid (xloc, x)), e)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                     (_loc : Locf.t)  ->
                     let xloc = __fan_0.loc in
                     let x = __fan_0.txt in
                     (`App (_loc, (`Lid (xloc, x)), e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Pre; word = Any; tag_name = "Pre" } } : 
              Tokenf.pattern );
           `Self],
            ("`App (_loc, (`Lid (xloc, x)), e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_0.loc in
                    let x = __fan_0.txt in
                    (`App (_loc, (`Lid (xloc, x)), e) : 'exp )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )],
             ("Ast_quotation.expand x Dyn_tag.exp\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                     let x = __fan_0 in
                     (Ast_quotation.expand x Dyn_tag.exp : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "exp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "par"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "seq"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "chr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int32"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "str"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int64"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "flo"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "nativeint"); tag_name = "Ant"
                   }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "chr'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int64'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "nativeint'"); tag_name = "Ant"
                   }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "bool'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "int32'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "flo'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "str'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`chr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`int64"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`nativeint"); tag_name = "Ant"
                   }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`bool"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`int"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`int32"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`flo"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "`str"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
              Tokenf.pattern )],
            ("`Int (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Int32; word = Any; tag_name = "Int32" } } : 
              Tokenf.pattern )],
            ("`Int32 (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int32 (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Int64; word = Any; tag_name = "Int64" } } : 
              Tokenf.pattern )],
            ("`Int64 (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int64 (_loc, s) : 'exp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
               } : Tokenf.pattern )],
            ("`Nativeint (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Nativeint (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" } } : 
              Tokenf.pattern )],
            ("`Flo (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Flo (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" } } : 
              Tokenf.pattern )],
            ("`Chr (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Chr (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
              Tokenf.pattern )],
            ("`Str (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Str (_loc, s) : 'exp )))));
          ([`Try
              (`Nterm
                 (Gramf.obj
                    (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                     Gramf.t )));
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`LetOpen (_loc, (`Negative _loc), i, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(e : 'exp) 
                    ~__fan_0:(i : 'module_longident_dot_lparen) 
                    (_loc : Locf.t)  ->
                    (`LetOpen (_loc, (`Negative _loc), i, e) : 'exp )))));
          ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
            ("(i : vid  :>exp)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'vid)  (_loc : Locf.t)  ->
                    ((i : vid  :>exp) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
            ("`Vrn (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Vrn (_loc, s) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("(`Uid (_loc, \"[]\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (sem_exp_for_list : 'sem_exp_for_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(s : 'sem_exp_for_list) 
                    ~__fan_0:_  (_loc : Locf.t)  -> (s : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "[|"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "|]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ArrayEmpty _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ArrayEmpty _loc : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "[|"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "|]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Array (_loc, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(el : 'sem_exp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Array (_loc, el) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`RecordWith (_loc, el, (`Lid (xloc, x)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(el : 'label_exp_list)  ~__fan_2:_
                     ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (`RecordWith (_loc, el, (`Lid (xloc, x))) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Record (_loc, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(el : 'label_exp_list)  ~__fan_0:_
                     (_loc : Locf.t)  -> (`Record (_loc, el) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`RecordWith (_loc, el, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:(el : 'label_exp_list)  ~__fan_4:_
                     ~__fan_3:_  ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`RecordWith (_loc, el, e) : 
                    'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "{<"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ">}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`OvrInstEmpty _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OvrInstEmpty _loc : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "{<"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (field_exp_list : 'field_exp_list Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ">}"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`OvrInst (_loc, fel)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(fel : 'field_exp_list) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OvrInst (_loc, fel) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("(`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Constraint (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, e, t) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Par (_loc, (`Com (_loc, e, el)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(el : 'comma_exp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Seq (_loc, (`Sem (_loc, e, seq)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(seq : 'sequence)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Seq (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:_  ~__fan_1:(e : 'exp)  ~__fan_0:_
                     (_loc : Locf.t)  -> (`Seq (_loc, e) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":>"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Coercion (_loc, e, t, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:(t2 : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(t : 'ctyp)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Coercion (_loc, e, t, t2) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ":>"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Subtype (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Subtype (_loc, e, t) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(e : 'exp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (e : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "begin"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match seq with\n| Some seq -> `Seq (_loc, seq)\n| None  -> (`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    let seq = None in
                    (match seq with
                     | Some seq -> `Seq (_loc, seq)
                     | None  -> (`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "begin"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match seq with\n| Some seq -> `Seq (_loc, seq)\n| None  -> (`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(seq : 'sequence)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let seq = Some seq in
                    (match seq with
                     | Some seq -> `Seq (_loc, seq)
                     | None  -> (`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Package_exp (_loc, me)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(me : 'mexp)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Package_exp (_loc, me) : 'exp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Package_exp (_loc, (`Constraint (_loc, me, pt)))\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(pt : 'mtyp)  ~__fan_3:_ 
                    ~__fan_2:(me : 'mexp)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                    'exp )))))])] : Gramf.olevel list ));
   Gramf.extend_single (sem_exp_for_list : 'sem_exp_for_list Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), el) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(el : 'sem_exp_for_list)  ~__fan_1:_ 
                     ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                     ((`App (_loc, (`App (_loc, (`Uid (_loc, "::")), e)), el) : 
                     FAst.exp ) : 'sem_exp_for_list )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (`Uid (_loc, \"[]\"))) : \nFAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    ((`App
                        (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                          (`Uid (_loc, "[]"))) : FAst.exp ) : 'sem_exp_for_list )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("(`App (_loc, (`App (_loc, (`Uid (_loc, \"::\")), e)), (`Uid (_loc, \"[]\"))) : \nFAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    ((`App
                        (_loc, (`App (_loc, (`Uid (_loc, "::")), e)),
                          (`Uid (_loc, "[]"))) : FAst.exp ) : 'sem_exp_for_list )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (sequence : 'sequence Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
             ("k (`LetIn (_loc, rf, bi, e))\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(k : 'sequence')  ~__fan_4:(e : 'exp) 
                     ~__fan_3:_  ~__fan_2:(bi : 'bind) 
                     ~__fan_1:(rf : 'opt_rec)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "try"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (case : 'case Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetTryInWith (_loc, r, bi, x, a))\n",
              (Gramf.mk_action
                 (fun ~__fan_8:(k : 'sequence')  ~__fan_7:(a : 'case) 
                    ~__fan_6:_  ~__fan_5:(x : 'sequence)  ~__fan_4:_ 
                    ~__fan_3:(bi : 'bind)  ~__fan_2:(r : 'opt_rec) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetModule (_loc, m, mb, e))\n",
              (Gramf.mk_action
                 (fun ~__fan_6:(k : 'sequence')  ~__fan_5:(e : 'exp) 
                    ~__fan_4:_  ~__fan_3:(mb : 'mbind0) 
                    ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(e : 'sequence)  ~__fan_3:_ 
                    ~__fan_2:(i : 'module_longident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = None in
                    (`LetOpen
                       (_loc,
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc), (i : vid  :>ident), e) : 
                      'sequence )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(e : 'sequence)  ~__fan_4:_ 
                    ~__fan_3:(i : 'module_longident) 
                    ~__fan_2:(bang : Tokenf.txt)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (`LetOpen
                       (_loc,
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc), (i : vid  :>ident), e) : 
                      'sequence )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(k : 'sequence')  ~__fan_0:(e : 'exp) 
                    (_loc : Locf.t)  -> (k e : 'sequence )))))]) : Gramf.olevel ));
   Gramf.extend_single (sequence' : 'sequence' Gramf.t )
     (None,
       ((None, None,
          [([],
             ("fun e  -> e\n",
               (Gramf.mk_action
                  (fun (_loc : Locf.t)  -> (fun e  -> e : 'sequence' )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
              Tokenf.pattern )],
            ("fun e  -> e\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  ->
                    (fun e  -> e : 'sequence' )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ))],
            ("fun e  -> `Sem (_loc, e, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'sequence)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]) : 
       Gramf.olevel )));
  Gramf.extend_single (with_exp_lang : 'with_exp_lang Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (lang : 'lang Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("Ast_quotation.default := old; x\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(x : 'exp)  ~__fan_1:_  ~__fan_0:(old : 'lang)
                     (_loc : Locf.t)  ->
                    (Ast_quotation.default := old; x : 'with_exp_lang )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (with_stru_lang : 'with_stru_lang Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (lang : 'lang Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (stru : 'stru Gramf.t ))],
            ("Ast_quotation.default := old; x\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(x : 'stru)  ~__fan_1:_ 
                    ~__fan_0:(old : 'lang)  (_loc : Locf.t)  ->
                    (Ast_quotation.default := old; x : 'with_stru_lang )))))]) : 
      Gramf.olevel ));
  (Gramf.extend_single (bind_quot : 'bind_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (bind : 'bind Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'bind)  (_loc : Locf.t)  ->
                     (x : 'bind_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (bind : 'bind Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Ant; word = (Kind "bind"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"bind\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"bind" s : 'bind )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"bind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"bind" s : 'bind )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`Bind (_loc, (mk_ant ~c:\"pat\" s), e) : FAst.bind )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Bind (_loc, (mk_ant ~c:"pat" s), e) : FAst.bind ) : 
                      'bind )))));
          ([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`And (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(b2 : 'bind)  ~__fan_1:_ 
                    ~__fan_0:(b1 : 'bind)  (_loc : Locf.t)  ->
                    (`And (_loc, b1, b2) : 'bind )))));
          ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("`Bind (_loc, p, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_bind)  ~__fan_0:(p : 'pat) 
                    (_loc : Locf.t)  -> (`Bind (_loc, p, e) : 'bind )))))]) : 
       Gramf.olevel )));
  (Gramf.extend_single (rec_exp_quot : 'rec_exp_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'label_exp_list)  (_loc : Locf.t)  ->
                     (x : 'rec_exp_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (label_exp : 'label_exp Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Ant; word = (Kind "rec_exp"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"rec_exp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'label_exp )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"rec_exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'label_exp )))));
          ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ));
           `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("(`RecBind (_loc, i, e) : FAst.rec_exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_bind) 
                    ~__fan_0:(i : 'label_longident)  (_loc : Locf.t)  ->
                    ((`RecBind (_loc, i, e) : FAst.rec_exp ) : 'label_exp )))));
          ([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
            ("`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'label_longident)  (_loc : Locf.t)  ->
                    (`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                    'label_exp )))))]) : Gramf.olevel ));
   Gramf.extend_single (field_exp : 'field_exp Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"rec_exp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'field_exp )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "bi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"rec_exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'field_exp )))));
          ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`RecBind (_loc, (l :>vid), e)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(l : 'a_lident)  (_loc : Locf.t)  ->
                    (`RecBind (_loc, (l :>vid), e) : 'field_exp )))))]) : 
       Gramf.olevel )));
  (Gramf.extend_single (luident : 'luident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )],
             ("i\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (i : 'luident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("i\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (i : 'luident )))))]) : Gramf.olevel ));
   Gramf.extend_single (aident : 'aident Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
             ("(i :>ident)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                     ((i :>ident) : 'aident )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
            ("(i :>ident)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'a_uident)  (_loc : Locf.t)  ->
                    ((i :>ident) : 'aident )))))]) : Gramf.olevel ));
   Gramf.extend_single (astr : 'astr Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )],
             ("`C (_loc, i)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (`C (_loc, i) : 'astr )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`C (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`C (_loc, i) : 'astr )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'astr )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'astr )))))]) : Gramf.olevel ));
   Gramf.extend (ident_quot : 'ident_quot Gramf.t )
     (None,
       ([((Some "."), None,
           [([`Self;
             `Token
               ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
               Tokenf.pattern );
             `Self],
              ("(`Dot (_loc, i, j) : FAst.ident )\n",
                (Gramf.mk_action
                   (fun ~__fan_2:(j : 'ident_quot)  ~__fan_1:_ 
                      ~__fan_0:(i : 'ident_quot)  (_loc : Locf.t)  ->
                      ((`Dot (_loc, i, j) : FAst.ident ) : 'ident_quot )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("(`Lid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Lid (_loc, i) : FAst.ident ) : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(j : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    ((`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident ) : 
                      'ident_quot )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Apply (_loc, i, j)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(j : 'ident_quot) 
                    ~__fan_1:(i : 'ident_quot)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Apply (_loc, i, j) : 'ident_quot )))))])] : 
       Gramf.olevel list ));
   Gramf.extend_single (ident : 'ident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Lid (_loc, i) : 'ident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Uid (_loc, i) : 'ident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(j : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (vid : 'vid Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Lid (_loc, i) : 'vid )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Uid (_loc, i) : 'vid )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (`Uid (_loc, s)), j)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(j : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (uident : 'uident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
               Tokenf.pattern )],
             ("`Uid (_loc, s)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let s = __fan_0.txt in (`Uid (_loc, s) : 'uident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("dot (`Uid (_loc, s)) l\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (dot (`Uid (_loc, s)) l : 'uident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("dot (mk_ant ~c:\"uident\" s) i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (dot (mk_ant ~c:"uident" s) i : 'uident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("dot (mk_ant ~c:\"uident\" s) i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (dot (mk_ant ~c:"uident" s) i : 'uident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("dot (mk_ant ~c:\"uident\" s) i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (dot (mk_ant ~c:"uident" s) i : 'uident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (dot_lstrings : 'dot_lstrings Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
               Tokenf.pattern )],
             ("((`Sub []), i)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (((`Sub []), i) : 'dot_lstrings )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("match xs with\n| (`Sub xs,v) -> ((`Sub (i :: xs)), v)\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'dot_lstrings)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (match xs with
                     | (`Sub xs,v) -> ((`Sub (i :: xs)), v)
                     | _ -> raise (Streamf.Error "impossible dot_lstrings") : 
                      'dot_lstrings )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("match xs with\n| (`Sub xs,v) -> ((`Absolute (i :: xs)), v)\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(xs : 'dot_lstrings)  ~__fan_2:_ 
                    ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let i = __fan_1.txt in
                    (match xs with
                     | (`Sub xs,v) -> ((`Absolute (i :: xs)), v)
                     | _ -> raise (Streamf.Error "impossible dot_lstrings") : 
                      'dot_lstrings )))))]) : Gramf.olevel ));
   Gramf.extend_single
     (module_longident_dot_lparen : 'module_longident_dot_lparen Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_2:_  ~__fan_1:_ 
                     ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident_dot_lparen)  ~__fan_1:_
                     ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                      'module_longident_dot_lparen )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt)
                     (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident_dot_lparen)  ~__fan_1:_
                     ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                      'module_longident_dot_lparen )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident_dot_lparen)  ~__fan_1:_
                     ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                      'module_longident_dot_lparen )))))]) : Gramf.olevel ));
   Gramf.extend_single (module_longident : 'module_longident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (`Uid (_loc, i)), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Uid (_loc, i) : 'module_longident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'module_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'module_longident )))))]) : 
       Gramf.olevel ));
   Gramf.extend
     (module_longident_with_app : 'module_longident_with_app Gramf.t )
     (None,
       ([((Some "apply"), None,
           [([`Self; `Self],
              ("`Apply (_loc, i, j)\n",
                (Gramf.mk_action
                   (fun ~__fan_1:(j : 'module_longident_with_app) 
                      ~__fan_0:(i : 'module_longident_with_app) 
                      (_loc : Locf.t)  ->
                      (`Apply (_loc, i, j) : 'module_longident_with_app )))))]);
        ((Some "."), None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(j : 'module_longident_with_app)  ~__fan_1:_ 
                     ~__fan_0:(i : 'module_longident_with_app) 
                     (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'module_longident_with_app )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Uid (_loc, i) : 'module_longident_with_app )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(i : 'module_longident_with_app) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (i : 'module_longident_with_app )))))])] : Gramf.olevel
                                                                 list ));
   Gramf.extend (type_longident : 'type_longident Gramf.t )
     (None,
       ([((Some "apply"), None,
           [([`Self; `Self],
              ("`Apply (_loc, i, j)\n",
                (Gramf.mk_action
                   (fun ~__fan_1:(j : 'type_longident) 
                      ~__fan_0:(i : 'type_longident)  (_loc : Locf.t)  ->
                      (`Apply (_loc, i, j) : 'type_longident )))))]);
        ((Some "."), None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(j : 'type_longident)  ~__fan_1:_ 
                     ~__fan_0:(i : 'type_longident)  (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'type_longident )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("(`Lid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Lid (_loc, i) : FAst.ident ) : 'type_longident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'type_longident )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(i : 'type_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (i : 'type_longident )))))])] : 
       Gramf.olevel list ));
   Gramf.extend_single (label_longident : 'label_longident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Lid (_loc, i) : 'label_longident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (`Uid (iloc, i)), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let iloc = __fan_0.loc in
                    let i = __fan_0.txt in
                    (`Dot (_loc, (`Uid (iloc, i)), l) : 'label_longident )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'label_longident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'label_longident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (cltyp_longident : 'cltyp_longident Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'type_longident)  (_loc : Locf.t)  ->
                     (x : 'cltyp_longident )))))]) : Gramf.olevel ));
   Gramf.extend_single (val_longident : 'val_longident Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (ident : 'ident Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'ident)  (_loc : Locf.t)  ->
                     (x : 'val_longident )))))]) : Gramf.olevel ));
   Gramf.extend_single (class_longident : 'class_longident Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
             ("(x : vid  :>ident)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'label_longident)  (_loc : Locf.t)  ->
                     ((x : vid  :>ident) : 'class_longident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_override : 'opt_override Gramf.t )
     (None,
       ((None, None,
          [([],
             ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
               (Gramf.mk_action
                  (fun (_loc : Locf.t)  ->
                     let bang = None in
                     (match bang with
                      | Some _ -> `Positive _loc
                      | None  -> `Negative _loc : 'opt_override )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
              Tokenf.pattern )],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(bang : Tokenf.txt)  (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'opt_override )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "!"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_override )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "override"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_override )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (flag : 'flag Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "to"); tag_name = "Key" } } : 
               Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'flag )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "downto"); tag_name = "Key" }
               } : Tokenf.pattern )],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Negative _loc : 'flag )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind "to"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'flag )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'flag )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_private : 'opt_private Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "private"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_private )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "private"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_private )))));
          ([],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_private )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_mutable : 'opt_mutable Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "mutable"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_mutable )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "mutable"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_mutable )))));
          ([],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_mutable )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_virtual : 'opt_virtual Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "virtual"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_virtual )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "virtual"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_virtual )))));
          ([],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_virtual )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_dot_dot : 'opt_dot_dot Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A ".."); tag_name = "Key" } } : 
               Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_dot_dot )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ".."); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_dot_dot )))));
          ([],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_dot_dot )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_rec : 'opt_rec Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "rec"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_rec )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "rec"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_rec )))));
          ([],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_rec )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (a_lident : 'a_lident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"a_lident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"a_lident" s : 'a_lident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"a_lident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"a_lident" s : 'a_lident )))));
          ([`Token
              ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" } } : 
              Tokenf.pattern )],
            ("`Lid (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Lid (_loc, s) : 'a_lident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (a_uident : 'a_uident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"a_uident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"a_uident" s : 'a_uident )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"a_uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"a_uident" s : 'a_uident )))));
          ([`Token
              ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
              Tokenf.pattern )],
            ("`Uid (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Uid (_loc, s) : 'a_uident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (string_list : 'string_list Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"str_list\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"str_list" s : 'string_list )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Self],
            ("`App (_loc, (mk_ant ~c:\"\" s), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'string_list) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`App (_loc, (mk_ant ~c:"" s), xs) : 'string_list )))));
          ([`Token
              ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
              Tokenf.pattern )],
            ("`Str (_loc, x)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in (`Str (_loc, x) : 'string_list )))));
          ([`Token
              ({ descr = { tag = `Str; word = Any; tag_name = "Str" } } : 
              Tokenf.pattern );
           `Self],
            ("`App (_loc, (`Str (_loc, x)), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'string_list) 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in
                    (`App (_loc, (`Str (_loc, x)), xs) : 'string_list )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (rec_flag_quot : 'rec_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_rec)  (_loc : Locf.t)  ->
                     (x : 'rec_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (direction_flag_quot : 'direction_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (flag : 'flag Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'flag)  (_loc : Locf.t)  ->
                     (x : 'direction_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (mutable_flag_quot : 'mutable_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_mutable)  (_loc : Locf.t)  ->
                     (x : 'mutable_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (private_flag_quot : 'private_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_private)  (_loc : Locf.t)  ->
                     (x : 'private_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (virtual_flag_quot : 'virtual_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_virtual)  (_loc : Locf.t)  ->
                     (x : 'virtual_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (row_var_flag_quot : 'row_var_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_dot_dot)  (_loc : Locf.t)  ->
                     (x : 'row_var_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (override_flag_quot : 'override_flag_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'opt_override)  (_loc : Locf.t)  ->
                     (x : 'override_flag_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (pat_eoi : 'pat_eoi Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
            `Token
              ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
              Tokenf.pattern )],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:(x : 'pat)  (_loc : Locf.t)  ->
                     (x : 'pat_eoi )))))]) : Gramf.olevel ));
   Gramf.extend_single (exp_eoi : 'exp_eoi Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
              Tokenf.pattern )],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:(x : 'exp)  (_loc : Locf.t)  ->
                     (x : 'exp_eoi )))))]) : Gramf.olevel )));
  (Gramf.extend_single (implem : 'implem Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    {
                      tag = `DirQuotation;
                      word = Any;
                      tag_name = "DirQuotation"
                    }
                } : Tokenf.pattern )],
             ("Fdir.handle_quot x; ([], (Some _loc))\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                     let x = __fan_0 in
                     (Fdir.handle_quot x; ([], (Some _loc)) : 'implem )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(rest : 'implem)  ~__fan_1:_ 
                    ~__fan_0:(si : 'stru)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'implem )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Self],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(rest : 'implem)  ~__fan_0:(si : 'stru) 
                    (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'implem )))));
          ([`Token
              ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
              Tokenf.pattern )],
            ("([], None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  -> (([], None) : 'implem )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (top_phrase : 'top_phrase Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("Some (`Directive (_loc, n, dp))\n",
               (Gramf.mk_action
                  (fun ~__fan_3:_  ~__fan_2:(dp : 'exp) 
                     ~__fan_1:(n : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("Some (`DirectiveSimple (_loc, n))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(n : 'a_lident)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("Some st\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (Some st : 'top_phrase )))));
          ([`Token
              ({ descr = { tag = `EOI; word = Any; tag_name = "EOI" } } : 
              Tokenf.pattern )],
            ("None\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  -> (None : 'top_phrase )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (strus : 'strus Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "stri"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'strus)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'strus)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'strus)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'strus)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ))],
            ("st\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (st : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("st\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (st : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'strus)  ~__fan_0:(st : 'stru) 
                    (_loc : Locf.t)  -> (`Sem (_loc, st, xs) : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";;"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'strus)  ~__fan_1:_ 
                    ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (`Sem (_loc, st, xs) : 'strus )))))]) : Gramf.olevel ));
   Gramf.extend_single (stru_quot : 'stru_quot Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("`Directive (_loc, n, dp)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(dp : 'exp)  ~__fan_1:(n : 'a_lident) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Directive (_loc, n, dp) : 'stru_quot )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`DirectiveSimple (_loc, n)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(n : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`DirectiveSimple (_loc, n) : 'stru_quot )))));
          ([`Nterm (Gramf.obj (strus : 'strus Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(x : 'strus)  (_loc : Locf.t)  ->
                    (x : 'stru_quot )))))]) : Gramf.olevel ));
   Gramf.extend (stru : 'stru Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Token
                ({
                   descr =
                     { tag = `Key; word = (A "include"); tag_name = "Key" }
                 } : Tokenf.pattern );
             `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
              ("`Include (_loc, me)\n",
                (Gramf.mk_action
                   (fun ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t) 
                      -> (`Include (_loc, me) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "module"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ))],
             ("`Module (_loc, i, mb)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(mb : 'mbind0)  ~__fan_1:(i : 'a_uident) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Module (_loc, i, mb) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "module"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "rec"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (mbind : 'mbind Gramf.t ))],
             ("`RecModule (_loc, mb)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(mb : 'mbind)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`RecModule (_loc, mb) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
             ("`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(i : 'module_longident)  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     let bang = None in
                     (`Open
                        (_loc,
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc), (i : vid  :>ident)) : 
                       'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ))],
             ("`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(i : 'module_longident) 
                     ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     let bang = Some bang in
                     (`Open
                        (_loc,
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc), (i : vid  :>ident)) : 
                       'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
             ("`Type (_loc, t)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(t : 'type_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Type (_loc, t) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "module"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
             ("`ModuleType (_loc, i, mt)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(mt : 'mtyp)  ~__fan_3:_ 
                     ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`ModuleType (_loc, i, mt) : 
                     'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "class"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
             ("`ClassType (_loc, ctd)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(ctd : 'cltyp_declaration)  ~__fan_1:_ 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`ClassType (_loc, ctd) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "exception"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm
              (Gramf.obj
                 (constructor_declaration : 'constructor_declaration Gramf.t ))],
             ("`Exception (_loc, t)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(t : 'constructor_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Exception (_loc, t) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "external"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
             ("`External (_loc, i, t, sl)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(sl : 'string_list)  ~__fan_4:_ 
                     ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`External (_loc, i, t, sl) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "type"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (type_declaration : 'type_declaration Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
              Tokenf.pattern )],
             ("`TypeWith (_loc, t, ns)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:_  ~__fan_4:(ns : 'string_list)  ~__fan_3:_ 
                     ~__fan_2:_  ~__fan_1:(t : 'type_declaration)  ~__fan_0:_
                      (_loc : Locf.t)  -> (`TypeWith (_loc, t, ns) : 
                     'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru )) (`LetIn (_loc, r, bi, x))\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(x : 'exp)  ~__fan_3:_  ~__fan_2:(bi : 'bind)
                      ~__fan_1:(r : 'opt_rec)  ~__fan_0:_  (_loc : Locf.t) 
                     ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetIn (_loc, r, bi, x)) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({
                 descr =
                   { tag = `Key; word = (A "module"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru )) (`LetModule (_loc, m, mb, e))\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(mb : 'mbind0)  ~__fan_2:(m : 'a_uident) 
                     ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetModule (_loc, m, mb, e)) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru ))\n  (`LetOpen\n     (_loc,\n       (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n       (i : vid  :>ident), e))\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(e : 'exp)  ~__fan_3:_ 
                     ~__fan_2:(i : 'module_longident)  ~__fan_1:_  ~__fan_0:_
                      (_loc : Locf.t)  ->
                     let bang = None in
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetOpen
                           (_loc,
                             (match bang with
                              | Some _ -> `Positive _loc
                              | None  -> `Negative _loc), (i : vid  :>
                             ident), e)) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "open"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru ))\n  (`LetOpen\n     (_loc,\n       (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n       (i : vid  :>ident), e))\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(i : 'module_longident) 
                     ~__fan_2:(bang : Tokenf.txt)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     let bang = Some bang in
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetOpen
                           (_loc,
                             (match bang with
                              | Some _ -> `Positive _loc
                              | None  -> `Negative _loc), (i : vid  :>
                             ident), e)) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "try"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "with"); tag_name = "Key" }
               } : Tokenf.pattern );
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru ))\n  (`LetTryInWith (_loc, r, bi, x, a))\n",
               (Gramf.mk_action
                  (fun ~__fan_7:(a : 'case)  ~__fan_6:_  ~__fan_5:(x : 'exp) 
                     ~__fan_4:_  ~__fan_3:(bi : 'bind) 
                     ~__fan_2:(r : 'opt_rec)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetTryInWith (_loc, r, bi, x, a)) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ))],
             ("match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(bi : 'bind)  ~__fan_1:(r : 'opt_rec) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (match bi with
                      | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                      | _ -> `Value (_loc, r, bi) : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "class"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm
              (Gramf.obj (class_declaration : 'class_declaration Gramf.t ))],
             ("`Class (_loc, cd)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(cd : 'class_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Class (_loc, cd) : 'stru )))));
           ([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'stru )))));
           ([`Token
               ({
                  descr =
                    { tag = `Ant; word = (Kind "stri"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'stru )))));
           ([`Token
               ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
               Tokenf.pattern )],
             ("Ast_quotation.expand x Dyn_tag.stru\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                     let x = __fan_0 in
                     (Ast_quotation.expand x Dyn_tag.stru : 'stru )))));
           ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("`StExp (_loc, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                     (`StExp (_loc, e) : 'stru )))))])] : Gramf.olevel list )));
  (Gramf.extend_single (class_signature : 'class_signature Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clsigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "csg"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "csg"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Self],
            ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(csg : 'class_signature) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                      'class_signature )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "csg"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(csg : 'class_signature) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                      'class_signature )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(csg : 'class_signature)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                      'class_signature )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "csg"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("(`Sem (_loc, (mk_ant ~c:\"clsigi\" s), csg) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(csg : 'class_signature)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Sem (_loc, (mk_ant ~c:"clsigi" s), csg) : FAst.clsigi ) : 
                      'class_signature )))));
          ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))],
            ("csg\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(csg : 'clsigi)  (_loc : Locf.t)  ->
                    (csg : 'class_signature )))));
          ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("csg\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(csg : 'clsigi)  (_loc : Locf.t) 
                    -> (csg : 'class_signature )))));
          ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Self],
            ("`Sem (_loc, csg, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'class_signature) 
                    ~__fan_0:(csg : 'clsigi)  (_loc : Locf.t)  ->
                    (`Sem (_loc, csg, xs) : 'class_signature )))));
          ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, csg, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'class_signature)  ~__fan_1:_ 
                    ~__fan_0:(csg : 'clsigi)  (_loc : Locf.t)  ->
                    (`Sem (_loc, csg, xs) : 'class_signature )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (clsigi : 'clsigi Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clsigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clsigi" s : 'clsigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "csg"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clsigi" s : 'clsigi )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clsigi\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clsigi : 'clsigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "inherit"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
            ("`SigInherit (_loc, cs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(cs : 'cltyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`SigInherit (_loc, cs) : 'clsigi )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
           `Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(mv : 'opt_virtual) 
                    ~__fan_1:(mf : 'opt_mutable)  ~__fan_0:_  (_loc : Locf.t)
                     ->
                    ((`CgVal (_loc, l, mf, mv, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Key; word = (A "virtual"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`VirMeth (_loc, l, pf, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(pf : 'opt_private) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`VirMeth (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`Method (_loc, l, pf, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(t : 'ctyp)  ~__fan_3:_ 
                    ~__fan_2:(l : 'a_lident)  ~__fan_1:(pf : 'opt_private) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Method (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "constraint"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`Eq (_loc, t1, t2) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Eq (_loc, t1, t2) : FAst.clsigi ) : 'clsigi )))))]) : 
       Gramf.olevel )));
  (Gramf.extend_single (class_structure : 'class_structure Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clfield\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cst"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'class_structure) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'class_structure) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'class_structure)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'class_structure)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ))],
            ("st\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(st : 'clfield)  (_loc : Locf.t)  ->
                    (st : 'class_structure )))));
          ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("st\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(st : 'clfield)  (_loc : Locf.t) 
                    -> (st : 'class_structure )))));
          ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'class_structure) 
                    ~__fan_0:(st : 'clfield)  (_loc : Locf.t)  ->
                    (`Sem (_loc, st, xs) : 'class_structure )))));
          ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'class_structure)  ~__fan_1:_ 
                    ~__fan_0:(st : 'clfield)  (_loc : Locf.t)  ->
                    (`Sem (_loc, st, xs) : 'class_structure )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single
     (value_val_opt_override : 'value_val_opt_override Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     let bang = None in
                     (match bang with
                      | Some _ -> `Positive _loc
                      | None  -> `Negative _loc : 'value_val_opt_override )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'value_val_opt_override )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'value_val_opt_override )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "override"); tag_name = "Ant" }
              } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'value_val_opt_override )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({ descr = { tag = `Ant; word = (Kind "!"); tag_name = "Ant" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'value_val_opt_override )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (method_opt_override : 'method_opt_override Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "method"); tag_name = "Key" }
                } : Tokenf.pattern )],
             ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     let bang = None in
                     (match bang with
                      | Some _ -> `Positive _loc
                      | None  -> `Negative _loc : 'method_opt_override )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'method_opt_override )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'method_opt_override )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "override"); tag_name = "Ant" }
              } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'method_opt_override )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (clfield : 'clfield Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clfield\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clfield" s : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cst"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clfield" s : 'clfield )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clfield\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clfield : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "inherit"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("`Inherit (_loc, o, ce)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(ce : 'clexp)  ~__fan_1:(o : 'opt_override) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Inherit (_loc, o, ce) : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "inherit"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`InheritAs (_loc, o, ce, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(i : 'a_lident)  ~__fan_3:_ 
                    ~__fan_2:(ce : 'clexp)  ~__fan_1:(o : 'opt_override) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`InheritAs (_loc, o, ce, i) : 'clfield )))));
          ([`Nterm
              (Gramf.obj
                 (value_val_opt_override : 'value_val_opt_override Gramf.t ));
           `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))],
            ("(`CrVal (_loc, lab, o, mf, e) : FAst.clfield )\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'cvalue_bind)  ~__fan_2:(lab : 'a_lident)
                     ~__fan_1:(mf : 'opt_mutable) 
                    ~__fan_0:(o : 'value_val_opt_override)  (_loc : Locf.t) 
                    ->
                    ((`CrVal (_loc, lab, o, mf, e) : FAst.clfield ) : 
                    'clfield )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "val"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Key; word = (A "virtual"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`VirVal (_loc, l, mf, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(mf : 'opt_mutable) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`VirVal (_loc, l, mf, t) : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "method"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({
                descr =
                  { tag = `Key; word = (A "virtual"); tag_name = "Key" }
              } : Tokenf.pattern );
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`VirMeth (_loc, l, pf, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(pf : 'opt_private) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`VirMeth (_loc, l, pf, t) : 'clfield )))));
          ([`Nterm
              (Gramf.obj
                 (method_opt_override : 'method_opt_override Gramf.t ));
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("`CrMth (_loc, l, o, pf, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(e : 'fun_bind)  ~__fan_4:(t : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(l : 'a_lident) 
                    ~__fan_1:(pf : 'opt_private) 
                    ~__fan_0:(o : 'method_opt_override)  (_loc : Locf.t)  ->
                    (`CrMth (_loc, l, o, pf, e, t) : 'clfield )))));
          ([`Nterm
              (Gramf.obj
                 (method_opt_override : 'method_opt_override Gramf.t ));
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))],
            ("`CrMthS (_loc, l, o, pf, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'fun_bind)  ~__fan_2:(l : 'a_lident) 
                    ~__fan_1:(pf : 'opt_private) 
                    ~__fan_0:(o : 'method_opt_override)  (_loc : Locf.t)  ->
                    (`CrMthS (_loc, l, o, pf, e) : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "constraint"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Eq (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Eq (_loc, t1, t2) : 'clfield )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "initializer"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Initializer (_loc, se)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(se : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Initializer (_loc, se) : 'clfield )))))]) : Gramf.olevel )));
  (Gramf.extend_single (clexp_quot : 'clexp_quot Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(x : 'clexp)  (_loc : Locf.t)  ->
                     (x : 'clexp_quot )))))]) : Gramf.olevel ));
   Gramf.extend_single (class_declaration : 'class_declaration Gramf.t )
     (None,
       ((None, None,
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`And (_loc, c1, c2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(c2 : 'class_declaration)  ~__fan_1:_ 
                     ~__fan_0:(c1 : 'class_declaration)  (_loc : Locf.t)  ->
                     (`And (_loc, c1, c2) : 'class_declaration )))));
          ([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clexp" s : 'class_declaration )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cdcl"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clexp" s : 'class_declaration )))));
          ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (comma_type_parameter : 'comma_type_parameter Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
            ("`ClDecl (_loc, mv, (i :>ident), x, ce)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(ce : 'class_fun_bind)  ~__fan_4:_ 
                    ~__fan_3:(x : 'comma_type_parameter)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                    (_loc : Locf.t)  ->
                    (`ClDecl (_loc, mv, (i :>ident), x, ce) : 'class_declaration )))));
          ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))],
            ("`ClDeclS (_loc, mv, (i :>ident), ce)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(ce : 'class_fun_bind) 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                    (_loc : Locf.t)  ->
                    (`ClDeclS (_loc, mv, (i :>ident), ce) : 'class_declaration )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (class_fun_bind : 'class_fun_bind Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
               Tokenf.pattern );
            `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
             ("ce\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (ce : 'class_fun_bind )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("`Constraint (_loc, ce, ct)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(ce : 'clexp)  ~__fan_2:_ 
                    ~__fan_1:(ct : 'cltyp_plus)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Constraint (_loc, ce, ct) : 'class_fun_bind )))));
          ([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
            ("`CeFun (_loc, p, cfb)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(cfb : 'class_fun_bind)  ~__fan_0:(p : 'ipat) 
                    (_loc : Locf.t)  ->
                    (`CeFun (_loc, p, cfb) : 'class_fun_bind )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (class_fun_def : 'class_fun_def Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); `Self],
             ("`CeFun (_loc, p, ce)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(ce : 'class_fun_def)  ~__fan_0:(p : 'ipat) 
                     (_loc : Locf.t)  ->
                     (`CeFun (_loc, p, ce) : 'class_fun_def )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("ce\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (ce : 'class_fun_def )))))]) : Gramf.olevel ));
   Gramf.extend (clexp : 'clexp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Token
                ({ descr = { tag = `Key; word = (A "fun"); tag_name = "Key" }
                 } : Tokenf.pattern );
             `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
             `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
              ("`CeFun (_loc, p, ce)\n",
                (Gramf.mk_action
                   (fun ~__fan_2:(ce : 'class_fun_def)  ~__fan_1:(p : 'ipat) 
                      ~__fan_0:_  (_loc : Locf.t)  ->
                      (`CeFun (_loc, p, ce) : 'clexp )))));
           ([`Token
               ({
                  descr =
                    { tag = `Key; word = (A "function"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
            `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
             ("`CeFun (_loc, p, ce)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(ce : 'class_fun_def)  ~__fan_1:(p : 'ipat) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`CeFun (_loc, p, ce) : 'clexp )))));
           ([`Token
               ({ descr = { tag = `Key; word = (A "let"); tag_name = "Key" }
                } : Tokenf.pattern );
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Token
              ({ descr = { tag = `Key; word = (A "in"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Self],
             ("`LetIn (_loc, rf, bi, ce)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(ce : 'clexp)  ~__fan_3:_ 
                     ~__fan_2:(bi : 'bind)  ~__fan_1:(rf : 'opt_rec) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`LetIn (_loc, rf, bi, ce) : 'clexp )))))]);
        ((Some "apply"), (Some `NA),
          [([`Self; `Snterml ((Gramf.obj (exp : 'exp Gramf.t )), "label")],
             ("`CeApp (_loc, ce, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:(ce : 'clexp) 
                     (_loc : Locf.t)  -> (`CeApp (_loc, ce, e) : 'clexp )))))]);
        ((Some "simple"), None,
          [([`Token
               ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clexp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clexp" s : 'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Ant; word = (Kind "cexp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clexp" s : 'clexp )))));
          ([`Token
              ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
              Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clexp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clexp : 'clexp )))));
          ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ClApply (_loc, ci, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(t : 'comma_ctyp)  ~__fan_1:_ 
                    ~__fan_0:(ci : 'vid)  (_loc : Locf.t)  ->
                    (`ClApply (_loc, ci, t) : 'clexp )))));
          ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
            ("(ci :>clexp)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(ci : 'vid)  (_loc : Locf.t)  ->
                    ((ci :>clexp) : 'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ObjPat (_loc, p, cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(cst : 'class_structure) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_
                     ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ObjPatEnd (_loc, p) : 'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_7:_  ~__fan_6:(cst : 'class_structure) 
                    ~__fan_5:_  ~__fan_4:(t : 'ctyp)  ~__fan_3:_ 
                    ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                    'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:_  ~__fan_4:(t : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                    'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Obj (_loc, cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(cst : 'class_structure) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Obj (_loc, cst) : 'clexp )))));
          ([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "object"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ObjEnd _loc : 'clexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("`Constraint (_loc, ce, ct)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(ct : 'cltyp)  ~__fan_2:_ 
                    ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, ce, ct) : 'clexp )))));
          ([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Self;
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern )],
            ("ce\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(ce : 'clexp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (ce : 'clexp )))))])] : Gramf.olevel
                                                                  list )));
  Gramf.extend_single (class_description : 'class_description Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`And (_loc, cd1, cd2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(cd2 : 'class_description)  ~__fan_1:_ 
                    ~__fan_0:(cd1 : 'class_description)  (_loc : Locf.t)  ->
                    (`And (_loc, cd1, cd2) : 'class_description )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'class_description )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'class_description )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
           ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
             (Gramf.mk_action
                (fun ~__fan_6:(ct : 'cltyp_plus)  ~__fan_5:_  ~__fan_4:_ 
                   ~__fan_3:(x : 'comma_type_parameter)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                   (_loc : Locf.t)  ->
                   (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'class_description )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))],
           ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(ct : 'cltyp_plus)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                   (_loc : Locf.t)  ->
                   (`CtDeclS (_loc, mv, (i :>ident), ct) : 'class_description )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (cltyp_declaration : 'cltyp_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`And (_loc, cd1, cd2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(cd2 : 'cltyp_declaration)  ~__fan_1:_ 
                    ~__fan_0:(cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                    (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'cltyp_declaration )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'cltyp_declaration )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("`CtDecl (_loc, mv, (i :>ident), x, ct)\n",
             (Gramf.mk_action
                (fun ~__fan_6:(ct : 'cltyp)  ~__fan_5:_  ~__fan_4:_ 
                   ~__fan_3:(x : 'comma_type_parameter)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                   (_loc : Locf.t)  ->
                   (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'cltyp_declaration )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("`CtDeclS (_loc, mv, (i :>ident), ct)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(ct : 'cltyp)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:(mv : 'opt_virtual) 
                   (_loc : Locf.t)  ->
                   (`CtDeclS (_loc, mv, (i :>ident), ct) : 'cltyp_declaration )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (cltyp_quot : 'cltyp_quot Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
            ("x\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(x : 'cltyp)  (_loc : Locf.t)  ->
                    (x : 'cltyp_quot )))))]) : Gramf.olevel ));
  Gramf.extend_single (cltyp_plus : 'cltyp_plus Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Token
             ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`CtFun (_loc, t, ct)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(ct : 'cltyp_plus)  ~__fan_3:_  ~__fan_2:_ 
                    ~__fan_1:(t : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`CtFun (_loc, t, ct) : 'cltyp_plus )))));
         ([`Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
           ("ct\n",
             (Gramf.mk_action
                (fun ~__fan_0:(ct : 'cltyp)  (_loc : Locf.t)  ->
                   (ct : 'cltyp_plus )))))]) : Gramf.olevel ));
  Gramf.extend_single (cltyp : 'cltyp Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"cltyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"cltyp" s : 'cltyp )))));
         ([`Token
             ({
                descr =
                  { tag = `Ant; word = (Kind "ctyp"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"cltyp" s : 'cltyp )))));
         ([`Token
             ({ descr = { tag = `Quot; word = Any; tag_name = "Quot" } } : 
             Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.cltyp\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.cltyp : 'cltyp )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ClApply (_loc, i, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(t : 'comma_ctyp)  ~__fan_1:_ 
                   ~__fan_0:(i : 'vid)  (_loc : Locf.t)  ->
                   (`ClApply (_loc, i, t) : 'cltyp )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ))],
           ("(i :>cltyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'vid)  (_loc : Locf.t)  ->
                   ((i :>cltyp) : 'cltyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "object"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ObjTy (_loc, t, csg)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(csg : 'class_signature) 
                   ~__fan_3:_  ~__fan_2:(t : 'ctyp)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "object"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Obj (_loc, csg)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(csg : 'class_signature) 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Obj (_loc, csg) : 'cltyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "object"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ObjTyEnd (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(t : 'ctyp)  ~__fan_1:_  ~__fan_0:_
                    (_loc : Locf.t)  -> (`ObjTyEnd (_loc, t) : 'cltyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "object"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "end"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ObjEnd _loc : 'cltyp )))))]) : Gramf.olevel ))
let apply_ctyp () =
  Gramf.extend_single (ctyp_quot : 'ctyp_quot Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ))],
            ("`Sta (_loc, x, y)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(y : 'star_ctyp)  ~__fan_1:_ 
                    ~__fan_0:(x : 'ctyp)  (_loc : Locf.t)  ->
                    (`Sta (_loc, x, y) : 'ctyp_quot )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("x\n",
             (Gramf.mk_action
                (fun ~__fan_0:(x : 'ctyp)  (_loc : Locf.t)  ->
                   (x : 'ctyp_quot )))))]) : Gramf.olevel ));
  Gramf.extend_single (unquoted_typevars : 'unquoted_typevars Gramf.t )
    (None,
      ((None, None,
         [([`Self; `Self],
            ("`App (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t2 : 'unquoted_typevars) 
                    ~__fan_0:(t1 : 'unquoted_typevars)  (_loc : Locf.t)  ->
                    (`App (_loc, t1, t2) : 'unquoted_typevars )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'unquoted_typevars )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'unquoted_typevars )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i :>ctyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i :>ctyp) : 'unquoted_typevars )))))]) : Gramf.olevel ));
  Gramf.extend_single (type_parameter : 'type_parameter Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant s : 'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "+"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(i : 'a_lident)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`Quote
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc),
                        i) : 'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(i : 'a_lident)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`Quote
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc),
                        i) : 'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "+"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`QuoteAny
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc)) : 
                     'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "-"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`QuoteAny
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc)) : 
                     'type_parameter )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
             Tokenf.pattern )],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Any _loc : 'type_parameter )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (type_parameters : 'type_parameters Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
            ("tpl (i :>ctyp)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(i : 'type_longident)  ~__fan_2:_ 
                    ~__fan_1:(tpl : 'type_parameters)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (tpl (i :>ctyp) : 'type_longident_and_parameters )))));
         ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("`App (_loc, (i :>ctyp), (tpl :>ctyp))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'type_longident) 
                   ~__fan_0:(tpl : 'type_parameter)  (_loc : Locf.t)  ->
                   (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters )))));
         ([`Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("(i :>ctyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'type_longident)  (_loc : Locf.t)  ->
                   ((i :>ctyp) : 'type_longident_and_parameters )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant s ~c:\"ctyp\"\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant s ~c:"ctyp" : 'type_longident_and_parameters )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (type_parameters : 'type_parameters Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
           `Self],
            ("fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp)))\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t2 : 'type_parameters) 
                    ~__fan_0:(t1 : 'type_parameter)  (_loc : Locf.t)  ->
                    (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                    'type_parameters )))));
         ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
           ("fun acc  -> `App (_loc, acc, (t :>ctyp))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'type_parameter)  (_loc : Locf.t)  ->
                   (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters )))));
         ([],
           ("fun t  -> t\n",
             (Gramf.mk_action
                (fun (_loc : Locf.t)  -> (fun t  -> t : 'type_parameters )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (meth_list : 'meth_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("let (ml,v) = rest in ((`Sem (_loc, m, ml)), v)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(rest : 'meth_list)  ~__fan_1:_ 
                    ~__fan_0:(m : 'meth_decl)  (_loc : Locf.t)  ->
                    (let (ml,v) = rest in ((`Sem (_loc, m, ml)), v) : 
                    'meth_list )))));
         ([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
          `Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
           ("(m, v)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(v : 'opt_dot_dot)  ~__fan_0:(m : 'meth_decl) 
                   (_loc : Locf.t)  -> ((m, v) : 'meth_list )))));
         ([`Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
           ("(m, v)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(v : 'opt_dot_dot)  ~__fan_1:_ 
                   ~__fan_0:(m : 'meth_decl)  (_loc : Locf.t)  ->
                   ((m, v) : 'meth_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (meth_decl : 'meth_decl Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'meth_decl )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'meth_decl )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyCol (_loc, lab, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(lab : 'a_lident)  (_loc : Locf.t)  ->
                   (`TyCol (_loc, lab, t) : 'meth_decl )))))]) : Gramf.olevel ));
  Gramf.extend_single (opt_meth_list : 'opt_meth_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (meth_list : 'meth_list Gramf.t ))],
            ("let (ml,v) = rest in `TyObj (_loc, ml, v)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(rest : 'meth_list)  (_loc : Locf.t)  ->
                    (let (ml,v) = rest in `TyObj (_loc, ml, v) : 'opt_meth_list )))));
         ([`Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))],
           ("`TyObjEnd (_loc, v)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(v : 'opt_dot_dot)  (_loc : Locf.t)  ->
                   (`TyObjEnd (_loc, v) : 'opt_meth_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (row_field : 'row_field Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'row_field )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'row_field )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("`TyVrn (_loc, (mk_ant ~c:\"ctyp\" s))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`TyVrn (_loc, (mk_ant ~c:"ctyp" s)) : 'row_field )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "of"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyVrnOf (_loc, (mk_ant ~c:\"ctyp\" s), t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`TyVrnOf (_loc, (mk_ant ~c:"ctyp" s), t) : 'row_field )))));
         ([`Self;
          `Token
            ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Bar (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'row_field)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'row_field)  (_loc : Locf.t)  ->
                   (`Bar (_loc, t1, t2) : 'row_field )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
           ("`TyVrn (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'astr)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyVrn (_loc, i) : 'row_field )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (astr : 'astr Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "of"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyVrnOf (_loc, i, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_  ~__fan_1:(i : 'astr) 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyVrnOf (_loc, i, t) : 'row_field )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Ctyp (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'ctyp)  (_loc : Locf.t)  ->
                   (`Ctyp (_loc, t) : 'row_field )))))]) : Gramf.olevel ));
  Gramf.extend_single (name_tags : 'name_tags Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'name_tags )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'name_tags )))));
         ([`Self; `Self],
           ("`App (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t2 : 'name_tags)  ~__fan_0:(t1 : 'name_tags) 
                   (_loc : Locf.t)  -> (`App (_loc, t1, t2) : 'name_tags )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "`"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
           ("`TyVrn (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'astr)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyVrn (_loc, i) : 'name_tags )))))]) : Gramf.olevel ));
  Gramf.extend_single (type_declaration : 'type_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'type_declaration )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'type_declaration )))));
         ([`Self;
          `Token
            ({ descr = { tag = `Key; word = (A "and"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`And (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'type_declaration)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'type_declaration)  (_loc : Locf.t)  ->
                   (`And (_loc, t1, t2) : 'type_declaration )))));
         ([`Nterm
             (Gramf.obj
                (type_ident_and_parameters : 'type_ident_and_parameters
                                               Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (type_info : 'type_info Gramf.t ));
          `List0 (`Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
           ("let (n,tpl) = rest in\n`TyDcl\n  (_loc, n, tpl, tk,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
             (Gramf.mk_action
                (fun ~__fan_3:(cl : 'constrain list) 
                   ~__fan_2:(tk : 'type_info)  ~__fan_1:_ 
                   ~__fan_0:(rest : 'type_ident_and_parameters) 
                   (_loc : Locf.t)  ->
                   (let (n,tpl) = rest in
                    `TyDcl
                      (_loc, n, tpl, tk,
                        (match cl with
                         | [] -> `None _loc
                         | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))));
         ([`Nterm
             (Gramf.obj
                (type_ident_and_parameters : 'type_ident_and_parameters
                                               Gramf.t ));
          `List0 (`Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))],
           ("let (n,tpl) = rest in\n`TyAbstr\n  (_loc, n, tpl,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(cl : 'constrain list) 
                   ~__fan_0:(rest : 'type_ident_and_parameters) 
                   (_loc : Locf.t)  ->
                   (let (n,tpl) = rest in
                    `TyAbstr
                      (_loc, n, tpl,
                        (match cl with
                         | [] -> `None _loc
                         | _ -> `Some (_loc, (and_of_list cl)))) : 'type_declaration )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (type_info : 'type_info Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
            ("`TyRepr (_loc, (`Negative _loc), t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(t2 : 'type_repr)  (_loc : Locf.t)  ->
                    (`TyRepr (_loc, (`Negative _loc), t2) : 'type_info )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyMan (_loc, t1, (`Negative _loc), t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'type_repr)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`TyMan (_loc, t1, (`Negative _loc), t2) : 'type_info )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyEq (_loc, (`Negative _loc), t1)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`TyEq (_loc, (`Negative _loc), t1) : 'type_info )))));
         ([`Token
             ({
                descr =
                  { tag = `Key; word = (A "private"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyEq (_loc, (`Positive _loc), t1)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyEq (_loc, (`Positive _loc), t1) : 'type_info )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "private"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyMan (_loc, t1, (`Positive _loc), t2)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(t2 : 'type_repr)  ~__fan_2:_  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info )))));
         ([`Token
             ({
                descr =
                  { tag = `Key; word = (A "private"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyRepr (_loc, (`Positive _loc), t2)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t2 : 'type_repr)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (type_repr : 'type_repr Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm
             (Gramf.obj
                (constructor_declarations : 'constructor_declarations Gramf.t ))],
            ("`Sum (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'constructor_declarations)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Sum (_loc, t) : 'type_repr )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "{"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm
            (Gramf.obj
               (label_declaration_list : 'label_declaration_list Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "}"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Record (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'label_declaration_list) 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Record (_loc, t) : 'type_repr )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
              Tokenf.pattern );
           `List1sep
             ((`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))),
               (`Token
                  ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" }
                   } : Tokenf.pattern )));
           `Token
             ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("(i, (`Some (_loc, (com_of_list (tpl :>decl_params list)))))\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(i : 'a_lident)  ~__fan_2:_ 
                    ~__fan_1:(tpl : 'type_parameter list)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    ((i,
                       (`Some (_loc, (com_of_list (tpl :>decl_params list))))) : 
                    'type_ident_and_parameters )))));
         ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i, (`Some (_loc, (t :>decl_params))))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:(t : 'type_parameter)
                    (_loc : Locf.t)  ->
                   ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i, (`None _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i, (`None _loc)) : 'type_ident_and_parameters )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (constrain : 'constrain Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 descr =
                   { tag = `Key; word = (A "constraint"); tag_name = "Key" }
               } : Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Eq (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Eq (_loc, t1, t2) : 'constrain )))))]) : Gramf.olevel ));
  Gramf.extend_single (typevars : 'typevars Gramf.t )
    (None,
      ((None, None,
         [([`Self; `Self],
            ("`App (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t2 : 'typevars)  ~__fan_0:(t1 : 'typevars) 
                    (_loc : Locf.t)  -> (`App (_loc, t1, t2) : 'typevars )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'typevars )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'typevars )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]) : 
      Gramf.olevel ));
  Gramf.extend (ctyp : 'ctyp Gramf.t )
    (None,
      ([((Some "alias"), (Some `LA),
          [([`Self;
            `Token
              ({ descr = { tag = `Key; word = (A "as"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Token
              ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
              Tokenf.pattern );
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
             ("`Alias (_loc, t1, i)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(i : 'a_lident)  ~__fan_2:_  ~__fan_1:_ 
                     ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                     (`Alias (_loc, t1, i) : 'ctyp )))))]);
       ((Some "forall"), (Some `LA),
         [([`Token
              ({ descr = { tag = `Key; word = (A "!"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (typevars : 'typevars Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`TyPol (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'typevars)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
       ((Some "arrow"), (Some `RA),
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Arrow (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'ctyp)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                    (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
       ((Some "label"), (Some `NA),
         [([`Token
              ({ descr = { tag = `Key; word = (A "~"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Label (_loc, i, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Label (_loc, i, t) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Label; word = Any; tag_name = "Label" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Label (_loc, (`Lid (_loc, s)), t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )))));
         ([`Token
             ({
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Self],
           ("`OptLabl (_loc, (`Lid (_loc, s)), t)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t : 'ctyp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "?"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`OptLabl (_loc, i, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`OptLabl (_loc, i, t) : 'ctyp )))))]);
       ((Some "apply"), (Some `LA),
         [([`Self; `Self],
            ("`App (_loc, t2, t1)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t2 : 'ctyp)  ~__fan_0:(t1 : 'ctyp) 
                    (_loc : Locf.t)  -> (`App (_loc, t2, t1) : 'ctyp )))))]);
       ((Some "simple"), None,
         [([`Token
              ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
              Tokenf.pattern );
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Quote (_loc, (`Normal _loc), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
             Tokenf.pattern )],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "id"); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("(try\n   let id = ident_of_ctyp t in\n   fun ()  -> (`Dot (_loc, (mk_ant ~c:\"ident\" s), id) : ctyp )\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   ((try
                       let id = ident_of_ctyp t in
                       fun ()  ->
                         (`Dot (_loc, (mk_ant ~c:"ident" s), id) : ctyp )
                     with
                     | Invalid_argument s ->
                         (fun ()  -> raise (Streamf.Error s))) () : 'ctyp )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "."); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("(try let id = ident_of_ctyp t in fun ()  -> `Dot (_loc, (i :>ident), id)\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(i : 'a_uident)  (_loc : Locf.t)  ->
                   ((try
                       let id = ident_of_ctyp t in
                       fun ()  -> `Dot (_loc, (i :>ident), id)
                     with
                     | Invalid_argument s ->
                         (fun ()  -> raise (Streamf.Error s))) () : 'ctyp )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("(i :>ctyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                   ((i :>ctyp) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Par (_loc, (`Sta (_loc, t, tl)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(tl : 'star_ctyp)  ~__fan_2:_ 
                   ~__fan_1:(t : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'ctyp)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (t : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Self;
          `Token
            ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (com_ctyp : 'com_ctyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n",
             (Gramf.mk_action
                (fun ~__fan_5:(j : 'type_longident)  ~__fan_4:_ 
                   ~__fan_3:(tl : 'com_ctyp)  ~__fan_2:_ 
                   ~__fan_1:(t : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (appl_of_list ((j :>ctyp) :: t ::
                      (Ast_basic.list_of_com tl [])) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "["); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`PolyEq (_loc, rfl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`PolyEq (_loc, rfl) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "[>"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`PolySup (_loc, rfl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`PolySup (_loc, rfl) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "[<"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let ntl = None in
                   (match ntl with
                    | None  -> `PolyInf (_loc, rfl)
                    | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "[<"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ">"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (name_tags : 'name_tags Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "]"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(ntl : 'name_tags)  ~__fan_2:_ 
                   ~__fan_1:(rfl : 'row_field)  ~__fan_0:_  (_loc : Locf.t) 
                   ->
                   let ntl = Some ntl in
                   (match ntl with
                    | None  -> `PolyInf (_loc, rfl)
                    | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "#"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
           ("`ClassPath (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'class_longident)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ClassPath (_loc, i) : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "<"); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Nterm (Gramf.obj (opt_meth_list : 'opt_meth_list Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ">"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'opt_meth_list)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (t : 'ctyp )))));
         ([`Token
             ({ descr = { tag = `Key; word = (A "("); tag_name = "Key" } } : 
             Tokenf.pattern );
          `Token
            ({ descr = { tag = `Key; word = (A "module"); tag_name = "Key" }
             } : Tokenf.pattern );
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ")"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("`Package (_loc, p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'mtyp)  ~__fan_1:_  ~__fan_0:_
                    (_loc : Locf.t)  -> (`Package (_loc, p) : 'ctyp )))))])] : 
      Gramf.olevel list ));
  Gramf.extend_single (comma_ctyp : 'comma_ctyp Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Com (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'comma_ctyp)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'comma_ctyp)  (_loc : Locf.t)  ->
                    (`Com (_loc, t1, t2) : 'comma_ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
             Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp,\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp," s : 'comma_ctyp )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`Ctyp (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'ctyp)  (_loc : Locf.t)  ->
                   (`Ctyp (_loc, t) : 'comma_ctyp )))))]) : Gramf.olevel ));
  Gramf.extend_single (com_ctyp : 'com_ctyp Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'com_ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'com_ctyp )))));
         ([`Self;
          `Token
            ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Com (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'com_ctyp)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'com_ctyp)  (_loc : Locf.t)  ->
                   (`Com (_loc, t1, t2) : 'com_ctyp )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'ctyp)  (_loc : Locf.t)  ->
                   (t : 'com_ctyp )))))]) : Gramf.olevel ));
  Gramf.extend_single (star_ctyp : 'star_ctyp Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'star_ctyp )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'star_ctyp )))));
         ([`Self;
          `Token
            ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Sta (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'star_ctyp)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'star_ctyp)  (_loc : Locf.t)  ->
                   (`Sta (_loc, t1, t2) : 'star_ctyp )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'ctyp)  (_loc : Locf.t)  ->
                   (t : 'star_ctyp )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (constructor_declarations : 'constructor_declarations Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'constructor_declarations )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'constructor_declarations )))));
         ([`Self;
          `Token
            ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Self],
           ("`Bar (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'constructor_declarations)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'constructor_declarations)  (_loc : Locf.t)
                    -> (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "of"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm
            (Gramf.obj
               (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
           ("`Of (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'constructor_arg_list)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   (`Of (_loc, s, t) : 'constructor_declarations )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyCol (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   (`TyCol (_loc, s, t) : 'constructor_declarations )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("(s :>or_ctyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   ((s :>or_ctyp) : 'constructor_declarations )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single
    (constructor_declaration : 'constructor_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'constructor_declaration )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'constructor_declaration )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A "of"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm
            (Gramf.obj
               (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
           ("`Of (_loc, (s :>vid), t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'constructor_arg_list)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   (`Of (_loc, (s :>vid), t) : 'constructor_declaration )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
           ("(s :>of_ctyp)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   ((s :>of_ctyp) : 'constructor_declaration )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (constructor_arg_list : 'constructor_arg_list Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A "*"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sta (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'constructor_arg_list)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'constructor_arg_list)  (_loc : Locf.t) 
                    -> (`Sta (_loc, t1, t2) : 'constructor_arg_list )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'ctyp)  (_loc : Locf.t)  ->
                   (t : 'constructor_arg_list )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (label_declaration_list : 'label_declaration_list Gramf.t )
    (None,
      ((None, None,
         [([`Nterm
              (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
           `Token
             ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Sem (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'label_declaration_list)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'label_declaration)  (_loc : Locf.t)  ->
                    (`Sem (_loc, t1, t2) : 'label_declaration_list )))));
         ([`Nterm
             (Gramf.obj (label_declaration : 'label_declaration Gramf.t ))],
           ("t1\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t1 : 'label_declaration)  (_loc : Locf.t)  ->
                   (t1 : 'label_declaration_list )))));
         ([`Nterm
             (Gramf.obj (label_declaration : 'label_declaration Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
            Tokenf.pattern )],
           ("t1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(t1 : 'label_declaration) 
                   (_loc : Locf.t)  -> (t1 : 'label_declaration_list )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (label_declaration : 'label_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({ descr = { tag = `Ant; word = (Kind ""); tag_name = "Ant" } } : 
              Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'label_declaration )))));
         ([`Token
             ({ descr = { tag = `Ant; word = (Kind "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'label_declaration )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyCol (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_lident)  (_loc : Locf.t)  ->
                   (`TyCol (_loc, s, t) : 'label_declaration )))));
         ([`Token
             ({
                descr =
                  { tag = `Key; word = (A "mutable"); tag_name = "Key" }
              } : Tokenf.pattern );
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Token
            ({ descr = { tag = `Key; word = (A ":"); tag_name = "Key" } } : 
            Tokenf.pattern );
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyColMut (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(s : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyColMut (_loc, s, t) : 'label_declaration )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (comma_type_parameter : 'comma_type_parameter Gramf.t )
    (None,
      ((None, None,
         [([`Self;
           `Token
             ({ descr = { tag = `Key; word = (A ","); tag_name = "Key" } } : 
             Tokenf.pattern );
           `Self],
            ("`Com (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'comma_type_parameter)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'comma_type_parameter)  (_loc : Locf.t) 
                    -> (`Com (_loc, t1, t2) : 'comma_type_parameter )))));
         ([`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))],
           ("`Ctyp (_loc, (t :>ctyp))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(t : 'type_parameter)  (_loc : Locf.t)  ->
                   (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter )))))]) : 
      Gramf.olevel ))
let fill_parsers =
  let applied = ref false in
  fun ()  ->
    if not (!applied) then (applied := true; apply (); apply_ctyp ())
let () = Ast_parsers.register_parser ("revise", fill_parsers)
