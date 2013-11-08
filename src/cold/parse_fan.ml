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
           `Keyword "as";
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
         [([`Keyword "|";
           `List1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))), (`Keyword "|"))],
            ("bar_of_list l\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(l : 'case0 list)  ~__fan_0:_  (_loc : Locf.t)
                     -> (bar_of_list l : 'case )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword "->";
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
                 pred =
                   ((function
                     | `Ant ({ kind = "case";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "case"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'case0 )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant s : 'case0 )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Keyword "when";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, (mk_ant s), w, e)\n",
             (Gramf.mk_action
                (fun ~__fan_4:(e : 'exp)  ~__fan_3:_  ~__fan_2:(w : 'exp) 
                   ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                   (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`CaseWhen (_loc, (mk_ant s), w, e) : 'case0 )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`Case (_loc, (mk_ant s), e)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (`Case (_loc, (mk_ant s), e) : 'case0 )))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Keyword "when";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword "->";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
           ("`CaseWhen (_loc, p, w, e)\n",
             (Gramf.mk_action
                (fun ~__fan_4:(e : 'exp)  ~__fan_3:_  ~__fan_2:(w : 'exp) 
                   ~__fan_1:_  ~__fan_0:(p : 'pat_as_pat_opt) 
                   (_loc : Locf.t)  -> (`CaseWhen (_loc, p, w, e) : 'case0 )))));
         ([`Nterm (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
          `Keyword "->";
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
                (`Keyword "|"))],
            ("bar_of_list x\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(x : 'case0 list)  (_loc : Locf.t)  ->
                    (bar_of_list x : 'case_quot )))))]) : Gramf.olevel ))
let make_semi atom nt =
  Gramf.extend_single (nt : 'nt Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (atom : 'atom Gramf.t )); `Keyword ";"; `Self],
            ("`Sem (_loc, b1, b2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(b2 : 'nt)  ~__fan_1:_  ~__fan_0:(b1 : 'atom) 
                    (_loc : Locf.t)  -> (`Sem (_loc, b1, b2) : 'nt )))));
         ([`Nterm (Gramf.obj (atom : 'atom Gramf.t ))],
           ("b1\n",
             (Gramf.mk_action
                (fun ~__fan_0:(b1 : 'atom)  (_loc : Locf.t)  -> (b1 : 'nt )))));
         ([`Nterm (Gramf.obj (atom : 'atom Gramf.t )); `Keyword ";"],
           ("b1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(b1 : 'atom)  (_loc : Locf.t)  ->
                   (b1 : 'nt )))))]) : Gramf.olevel ))
let make_comma atom nt =
  Gramf.extend_single (nt : 'nt Gramf.t )
    (None,
      ((None, None,
         [([`Self; `Keyword ","; `Self],
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
           `Keyword ",";
           `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ))],
            ("`Com (_loc, x, y)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(y : 'comma_pat)  ~__fan_1:_ 
                    ~__fan_0:(x : 'pat)  (_loc : Locf.t)  ->
                    (`Com (_loc, x, y) : 'pat_quot )))));
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ";";
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
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : pat ) : 'pat_constr )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat_constr )))))]) : 
      Gramf.olevel ));
  Gramf.extend (pat : 'pat Gramf.t )
    (None,
      ([((Some "|"), (Some `LA),
          [([`Self; `Keyword "|"; `Self],
             ("`Bar (_loc, p1, p2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(p2 : 'pat)  ~__fan_1:_  ~__fan_0:(p1 : 'pat)
                      (_loc : Locf.t)  -> (`Bar (_loc, p1, p2) : 'pat )))))]);
       ((Some ".."), (Some `NA),
         [([`Self; `Keyword ".."; `Self],
            ("`PaRng (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'pat)  ~__fan_1:_  ~__fan_0:(p1 : 'pat) 
                    (_loc : Locf.t)  -> (`PaRng (_loc, p1, p2) : 'pat )))))]);
       ((Some "::"), (Some `RA),
         [([`Self; `Keyword "::"; `Self],
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
         ([`Keyword "lazy"; `Self],
           ("`Lazy (_loc, p)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Lazy (_loc, p) : 'pat )))))]);
       ((Some "simple"), None,
         [([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int32";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int32"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int64";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int64"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "flo";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "flo"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "chr";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "chr"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "nativeint";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr =
                  { tag = `Ant; word = (A "nativeint"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "str";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "str"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int32'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int32'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "int64'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "int64'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "nativeint'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr =
                  { tag = `Ant; word = (A "nativeint'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "flo'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "flo'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "chr'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "chr'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "str'";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "str'"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`int";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`int"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`int32";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`int32"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`int64";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`int64"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`nativeint";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr =
                  { tag = `Ant; word = (A "`nativeint"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`flo";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`flo"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`chr";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`chr"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "`str";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "`str"); tag_name = "Ant" }
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
             ({
                pred = ((function | `Int _ -> true | _ -> false));
                descr = { tag = `Int; word = Any; tag_name = "Int" }
              } : Tokenf.pattern )],
           ("`Int (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Int32 _ -> true | _ -> false));
                descr = { tag = `Int32; word = Any; tag_name = "Int32" }
              } : Tokenf.pattern )],
           ("`Int32 (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int32 (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Int64 _ -> true | _ -> false));
                descr = { tag = `Int64; word = Any; tag_name = "Int64" }
              } : Tokenf.pattern )],
           ("`Int64 (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Int64 (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Nativeint _ -> true | _ -> false));
                descr =
                  { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
              } : Tokenf.pattern )],
           ("`Nativeint (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Nativeint (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Flo _ -> true | _ -> false));
                descr = { tag = `Flo; word = Any; tag_name = "Flo" }
              } : Tokenf.pattern )],
           ("`Flo (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Flo (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Chr _ -> true | _ -> false));
                descr = { tag = `Chr; word = Any; tag_name = "Chr" }
              } : Tokenf.pattern )],
           ("`Chr (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Chr (_loc, s) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Str _ -> true | _ -> false));
                descr = { tag = `Str; word = Any; tag_name = "Str" }
              } : Tokenf.pattern )],
           ("`Str (_loc, s)\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in (`Str (_loc, s) : 'pat )))));
         ([`Keyword "-";
          `Token
            ({
               pred = ((function | `Int _ -> true | _ -> false));
               descr = { tag = `Int; word = Any; tag_name = "Int" }
             } : Tokenf.pattern )],
           ("`Int (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Keyword "-";
          `Token
            ({
               pred = ((function | `Int32 _ -> true | _ -> false));
               descr = { tag = `Int32; word = Any; tag_name = "Int32" }
             } : Tokenf.pattern )],
           ("`Int32 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int32 (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Keyword "-";
          `Token
            ({
               pred = ((function | `Int64 _ -> true | _ -> false));
               descr = { tag = `Int64; word = Any; tag_name = "Int64" }
             } : Tokenf.pattern )],
           ("`Int64 (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Int64 (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Keyword "-";
          `Token
            ({
               pred = ((function | `Nativeint _ -> true | _ -> false));
               descr =
                 { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
             } : Tokenf.pattern )],
           ("`Nativeint (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Nativeint (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Keyword "-";
          `Token
            ({
               pred = ((function | `Flo _ -> true | _ -> false));
               descr = { tag = `Flo; word = Any; tag_name = "Flo" }
             } : Tokenf.pattern )],
           ("`Flo (_loc, (Stringf.neg s))\n",
             (Gramf.mk_action
                (fun ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_1.txt in
                   (`Flo (_loc, (Stringf.neg s)) : 'pat )))));
         ([`Keyword "["; `Keyword "]"],
           ("`Uid (_loc, \"[]\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "[]") : 'pat )))));
         ([`Keyword "[";
          `Nterm (Gramf.obj (sem_pat_for_list : 'sem_pat_for_list Gramf.t ));
          `Keyword "]"],
           ("s\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(s : 'sem_pat_for_list)  ~__fan_0:_
                    (_loc : Locf.t)  -> (s : 'pat )))));
         ([`Keyword "[|"; `Keyword "|]"],
           ("`ArrayEmpty _loc\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ArrayEmpty _loc : 'pat )))));
         ([`Keyword "[|";
          `Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ));
          `Keyword "|]"],
           ("`Array (_loc, pl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(pl : 'sem_pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`Array (_loc, pl) : 'pat )))));
         ([`Keyword "{";
          `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
          `Keyword "}"],
           ("`Record (_loc, pl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(pl : 'label_pat_list)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`Record (_loc, pl) : 'pat )))));
         ([`Keyword "("; `Keyword ")"],
           ("`Uid (_loc, \"()\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "()") : 'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ")"],
           ("`ModuleUnpack (_loc, m)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(m : 'a_uident)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ModuleUnpack (_loc, m) : 'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(pt : 'mtyp)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                   'pat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Token
            ({
               pred =
                 ((function
                   | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
                   | _ -> false));
               descr = { tag = `Ant; word = (A "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.ant)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_4 in
                   (`ModuleConstraint (_loc, m, (mk_ant s)) : 'pat )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(p : 'pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (p : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("`Constraint (_loc, p, t)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Constraint (_loc, p, t) : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword "as";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ")"],
           ("`Alias (_loc, p, s)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(s : 'a_lident)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Alias (_loc, p, s) : 'pat )))));
         ([`Keyword "(";
          `Self;
          `Keyword ",";
          `Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ));
          `Keyword ")"],
           ("`Par (_loc, (`Com (_loc, p, pl)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(pl : 'comma_pat)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Par (_loc, (`Com (_loc, p, pl))) : 'pat )))));
         ([`Keyword "#";
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("`ClassPath (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'type_longident)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ClassPath (_loc, i) : 'pat )))));
         ([`Keyword "~";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("`Label (_loc, i, p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(p : 'pat)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Label (_loc, i, p) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Label _ -> true | _ -> false));
                descr = { tag = `Label; word = Any; tag_name = "Label" }
              } : Tokenf.pattern );
          `Self],
           ("`Label (_loc, (`Lid (_loc, i)), p)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   (`Label (_loc, (`Lid (_loc, i)), p) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Quot _ -> true | _ -> false));
                descr = { tag = `Quot; word = Any; tag_name = "Quot" }
              } : Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.pat : 'pat )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'pat )))));
         ([`Keyword "_"],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'pat )))));
         ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`LabelS (_loc, i) : 'pat )))));
         ([`Token
             ({
                pred = ((function | `Optlabel _ -> true | _ -> false));
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
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
                pred = ((function | `Optlabel _ -> true | _ -> false));
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_ 
                   ~__fan_2:_  ~__fan_1:(i : 'a_lident)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(e : 'exp)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Token
            ({
               pred =
                 ((function
                   | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
                   | _ -> false));
               descr = { tag = `Ant; word = (A "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.ant)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let s = __fan_6 in
                   (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'pat )))));
         ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`OptLablS (_loc, i) : 'pat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword ")"],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'pat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
         [([`Keyword "{";
           `Nterm (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
           `Keyword "}"],
            ("(`Record (_loc, pl) : FAst.pat )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(pl : 'label_pat_list)  ~__fan_0:_
                     (_loc : Locf.t)  ->
                    ((`Record (_loc, pl) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "pat"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"pat\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"pat" s : 'ipat )))));
         ([`Keyword "("; `Keyword ")"],
           ("`Uid (_loc, \"()\")\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Uid (_loc, "()") : 'ipat )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ")"],
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
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
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
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
          `Token
            ({
               pred =
                 ((function
                   | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
                   | _ -> false));
               descr = { tag = `Ant; word = (A "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("`ModuleConstraint (_loc, m, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(__fan_4 : Tokenf.ant)  ~__fan_3:_ 
                   ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let s = __fan_4 in
                   (`ModuleConstraint (_loc, m, (mk_ant s)) : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ")"],
           ("p\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(p : 'pat)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (p : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("(`Constraint (_loc, p, t) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Constraint (_loc, p, t) : FAst.pat ) : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword "as";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ")"],
           ("(`Alias (_loc, p, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(s : 'a_lident)  ~__fan_2:_ 
                   ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Alias (_loc, p, s) : FAst.pat ) : 'ipat )))));
         ([`Keyword "(";
          `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
          `Keyword ",";
          `Nterm (Gramf.obj (comma_ipat : 'comma_ipat Gramf.t ));
          `Keyword ")"],
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
             ({
                pred = ((function | `Label _ -> true | _ -> false));
                descr = { tag = `Label; word = Any; tag_name = "Label" }
              } : Tokenf.pattern );
          `Self],
           ("(`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'ipat)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let i = __fan_0.txt in
                   ((`Label (_loc, (`Lid (_loc, i)), p) : FAst.pat ) : 
                     'ipat )))));
         ([`Keyword "~";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Self],
           ("(`Label (_loc, i, p) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_3:(p : 'ipat)  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Label (_loc, i, p) : FAst.pat ) : 'ipat )))));
         ([`Token
             ({
                pred = ((function | `Quot _ -> true | _ -> false));
                descr = { tag = `Quot; word = Any; tag_name = "Quot" }
              } : Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.pat\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.pat : 'ipat )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
           ("(`Vrn (_loc, s) : FAst.pat )\n",
             (Gramf.mk_action
                (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Vrn (_loc, s) : FAst.pat ) : 'ipat )))));
         ([`Keyword "_"],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'ipat )))));
         ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`LabelS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`LabelS (_loc, i) : 'ipat )))));
         ([`Token
             ({
                pred = ((function | `Optlabel _ -> true | _ -> false));
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
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
                pred = ((function | `Optlabel _ -> true | _ -> false));
                descr =
                  { tag = `Optlabel; word = Any; tag_name = "Optlabel" }
              } : Tokenf.pattern );
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword ")"],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_ 
                   ~__fan_2:_  ~__fan_1:(i : 'a_lident)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
           ("match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(e : 'exp)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = Some e in
                   (match e with
                    | None  -> `OptLabl (_loc, i, p)
                    | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat )))));
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Keyword "(";
          `Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
          `Keyword "=";
          `Token
            ({
               pred =
                 ((function
                   | `Ant ({ kind = "opt";_} : Tokenf.ant) -> true
                   | _ -> false));
               descr = { tag = `Ant; word = (A "opt"); tag_name = "Ant" }
             } : Tokenf.pattern );
          `Keyword ")"],
           ("`OptLablExpr (_loc, i, p, (mk_ant s))\n",
             (Gramf.mk_action
                (fun ~__fan_7:_  ~__fan_6:(__fan_6 : Tokenf.ant)  ~__fan_5:_ 
                   ~__fan_4:(p : 'pat_tcon)  ~__fan_3:_  ~__fan_2:_ 
                   ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                   let s = __fan_6 in
                   (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'ipat )))));
         ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`OptLablS (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`OptLablS (_loc, i) : 'ipat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword ")"],
           ("match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'ipat_tcon)  ~__fan_1:_ 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   let e = None in
                   (match e with
                    | Some e -> `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                    | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                     'ipat )))));
         ([`Keyword "?";
          `Keyword "(";
          `Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
          `Keyword "=";
          `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
          `Keyword ")"],
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
         [([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"; `Self],
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
         ([`Nterm (Gramf.obj (pat : 'pat Gramf.t )); `Keyword ";"],
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
           `Keyword ":";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
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
          `Keyword ":";
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
           `Keyword ";";
           `Self],
            ("`Sem (_loc, p1, p2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(p2 : 'label_pat_list)  ~__fan_1:_ 
                    ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t)  ->
                    (`Sem (_loc, p1, p2) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Keyword ";";
          `Keyword "_"],
           ("`Sem (_loc, p1, (`Any _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(p1 : 'label_pat) 
                   (_loc : Locf.t)  ->
                   (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list )))));
         ([`Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
          `Keyword ";";
          `Keyword "_";
          `Keyword ";"],
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
          `Keyword ";"],
           ("p1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(p1 : 'label_pat)  (_loc : Locf.t) 
                   -> (p1 : 'label_pat_list )))))]) : Gramf.olevel ));
  Gramf.extend_single (label_pat : 'label_pat Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"pat\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"pat" s : 'label_pat )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "pat";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "pat"); tag_name = "Ant" }
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
          `Keyword "=";
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
           [([`Keyword "(";
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Keyword ":";
             `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
             `Keyword ")";
             `Self],
              ("`Functor (_loc, m, mt, mb)\n",
                (Gramf.mk_action
                   (fun ~__fan_5:(mb : 'mbind0)  ~__fan_4:_ 
                      ~__fan_3:(mt : 'mtyp)  ~__fan_2:_ 
                      ~__fan_1:(m : 'a_uident)  ~__fan_0:_  (_loc : Locf.t) 
                      -> (`Functor (_loc, m, mt, mb) : 'mbind0 )))));
           ([`Keyword ":";
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
            `Keyword "=";
            `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
             ("`Constraint (_loc, me, mt)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(me : 'mexp)  ~__fan_2:_ 
                     ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Constraint (_loc, me, mt) : 'mbind0 )))));
           ([`Keyword "="; `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
             ("me\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (me : 'mbind0 )))))])] : Gramf.olevel list ));
   Gramf.extend (mexp : 'mexp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Keyword "functor";
             `Keyword "(";
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Keyword ":";
             `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
             `Keyword ")";
             `Keyword "->";
             `Self],
              ("`Functor (_loc, i, t, me)\n",
                (Gramf.mk_action
                   (fun ~__fan_7:(me : 'mexp)  ~__fan_6:_  ~__fan_5:_ 
                      ~__fan_4:(t : 'mtyp)  ~__fan_3:_ 
                      ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                      (_loc : Locf.t)  ->
                      (`Functor (_loc, i, t, me) : 'mexp )))));
           ([`Keyword "struct"; `Keyword "end"],
             ("match st with | Some st -> `Struct (_loc, st) | None  -> `StructEnd _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     let st = None in
                     (match st with
                      | Some st -> `Struct (_loc, st)
                      | None  -> `StructEnd _loc : 'mexp )))));
           ([`Keyword "struct";
            `Nterm (Gramf.obj (strus : 'strus Gramf.t ));
            `Keyword "end"],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"mexp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"mexp" s : 'mexp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mexp";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "mexp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mexp" s : 'mexp )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
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
          ([`Keyword "(";
           `Self;
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword ")"],
            ("`Constraint (_loc, me, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(mt : 'mtyp)  ~__fan_2:_ 
                    ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, me, mt) : 'mexp )))));
          ([`Keyword "("; `Self; `Keyword ")"],
            ("me\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(me : 'mexp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (me : 'mexp )))));
          ([`Keyword "(";
           `Keyword "val";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword ")"],
            ("`PackageModule (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:_
                     (_loc : Locf.t)  -> (`PackageModule (_loc, e) : 
                    'mexp )))));
          ([`Keyword "(";
           `Keyword "val";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword ")"],
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
          [([`Self; `Keyword "and"; `Self],
             ("`And (_loc, b1, b2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(b2 : 'mbind_quot)  ~__fan_1:_ 
                     ~__fan_0:(b1 : 'mbind_quot)  (_loc : Locf.t)  ->
                     (`And (_loc, b1, b2) : 'mbind_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind_quot )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`Constraint (_loc, m, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mt : 'mtyp)  ~__fan_1:_ 
                    ~__fan_0:(m : 'a_uident)  (_loc : Locf.t)  ->
                    (`Constraint (_loc, m, mt) : 'mbind_quot )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword "=";
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
          [([`Self; `Keyword "and"; `Self],
             ("`And (_loc, b1, b2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(b2 : 'mbind)  ~__fan_1:_ 
                     ~__fan_0:(b1 : 'mbind)  (_loc : Locf.t)  ->
                     (`And (_loc, b1, b2) : 'mbind )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mbind" s : 'mbind )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mbind\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mbind : 'mbind )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword "=";
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
          [([`Self; `Keyword "and"; `Self],
             ("`And (_loc, m1, m2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(m2 : 'module_rec_declaration)  ~__fan_1:_ 
                     ~__fan_0:(m1 : 'module_rec_declaration)  (_loc : Locf.t)
                      -> (`And (_loc, m1, m2) : 'module_rec_declaration )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"mbind" s : 'module_rec_declaration )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mbind";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "mbind"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mbind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"mbind" s : 'module_rec_declaration )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.mbind\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.mbind : 'module_rec_declaration )))));
          ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword ":";
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
          [([`Self; `Keyword "and"; `Self],
             ("`And (_loc, wc1, wc2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(wc2 : 'constr)  ~__fan_1:_ 
                     ~__fan_0:(wc1 : 'constr)  (_loc : Locf.t)  ->
                     (`And (_loc, wc1, wc2) : 'constr )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"constr\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"constr" s : 'constr )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "constr";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "constr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"constr\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"constr" s : 'constr )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.constr\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.constr : 'constr )))));
          ([`Keyword "type";
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Keyword "=";
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
          ([`Keyword "type";
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Keyword "=";
           `Keyword "private";
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
          ([`Keyword "type";
           `Nterm
             (Gramf.obj
                (type_longident_and_parameters : 'type_longident_and_parameters
                                                   Gramf.t ));
           `Keyword ":=";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`TypeSubst (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'type_longident_and_parameters) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`TypeSubst (_loc, t1, t2) : 'constr )))));
          ([`Keyword "module";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "=";
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
          ([`Keyword "module";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword ":=";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"sigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigis )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigis )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(sg : 'sigis)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(sg : 'sigis)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;";
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"sigi\" s), sg)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(sg : 'sigis)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"sigi" s), sg) : 'sigis )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;";
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
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"; `Self],
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
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"],
            ("sg\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(sg : 'sigi)  (_loc : Locf.t)  ->
                    (sg : 'sigis )))))]) : Gramf.olevel ));
   Gramf.extend (mtyp : 'mtyp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Keyword "functor";
             `Keyword "(";
             `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
             `Keyword ":";
             `Self;
             `Keyword ")";
             `Keyword "->";
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
            `Keyword "with";
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
          [([`Self; `Keyword "."; `Self],
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
          [([`Keyword "sig"; `Keyword "end"],
             ("match sg with | Some sg -> `Sig (_loc, sg) | None  -> `SigEnd _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     let sg = None in
                     (match sg with
                      | Some sg -> `Sig (_loc, sg)
                      | None  -> `SigEnd _loc : 'mtyp )))));
          ([`Keyword "sig";
           `Nterm (Gramf.obj (sigis : 'sigis Gramf.t ));
           `Keyword "end"],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"mtyp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"mtyp" s : 'mtyp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mtyp";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "mtyp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"mtyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"mtyp" s : 'mtyp )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
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
          ([`Keyword "("; `Self; `Keyword ")"],
            ("mt\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(mt : 'mtyp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (mt : 'mtyp )))));
          ([`Keyword "module";
           `Keyword "type";
           `Keyword "of";
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
          [([`Keyword ":"; `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
             ("mt\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (mt : 'module_declaration )))));
          ([`Keyword "(";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword ")";
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
          [([`Keyword "#";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
             ("`DirectiveSimple (_loc, s)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(s : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`DirectiveSimple (_loc, s) : 'sigi_quot )))));
          ([`Keyword "#";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Directive (_loc, s, dp)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(dp : 'exp)  ~__fan_1:(s : 'a_lident) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Directive (_loc, s, dp) : 'sigi_quot )))));
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";"; `Self],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"sigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigi )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "sigi";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "sigi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"sigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"sigi" s : 'sigi )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.sigi\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.sigi : 'sigi )))));
          ([`Keyword "include"; `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`Include (_loc, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(mt : 'mtyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Include (_loc, mt) : 'sigi )))));
          ([`Keyword "module";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm
             (Gramf.obj (module_declaration : 'module_declaration Gramf.t ))],
            ("`Module (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mt : 'module_declaration) 
                    ~__fan_1:(i : 'a_uident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Module (_loc, i, mt) : 'sigi )))));
          ([`Keyword "module";
           `Keyword "rec";
           `Nterm
             (Gramf.obj
                (module_rec_declaration : 'module_rec_declaration Gramf.t ))],
            ("`RecModule (_loc, mb)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(mb : 'module_rec_declaration)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`RecModule (_loc, mb) : 'sigi )))));
          ([`Keyword "module";
           `Keyword "type";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))],
            ("`ModuleTypeEnd (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ModuleTypeEnd (_loc, i) : 'sigi )))));
          ([`Keyword "open";
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
          ([`Keyword "open";
           `Keyword "!";
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
          ([`Keyword "type";
           `Nterm (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
            ("`Type (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'type_declaration)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Type (_loc, t) : 'sigi )))));
          ([`Keyword "module";
           `Keyword "type";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
            ("`ModuleType (_loc, i, mt)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(mt : 'mtyp)  ~__fan_3:_ 
                    ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ModuleType (_loc, i, mt) : 
                    'sigi )))));
          ([`Keyword "class";
           `Keyword "type";
           `Nterm
             (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
            ("`ClassType (_loc, ctd)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(ctd : 'cltyp_declaration)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ClassType (_loc, ctd) : 'sigi )))));
          ([`Keyword "exception";
           `Nterm
             (Gramf.obj
                (constructor_declaration : 'constructor_declaration Gramf.t ))],
            ("`Exception (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'constructor_declaration)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Exception (_loc, t) : 'sigi )))));
          ([`Keyword "external";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
            ("`External (_loc, i, t, sl)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(sl : 'string_list)  ~__fan_4:_ 
                    ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`External (_loc, i, t, sl) : 'sigi )))));
          ([`Keyword "val";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Val (_loc, i, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Val (_loc, i, t) : 'sigi )))));
          ([`Keyword "class";
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
          ([`Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); `Keyword ";;"; `Self],
            ("let (sil,stopped) = rest in ((si :: sil), stopped)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(rest : 'interf)  ~__fan_1:_ 
                    ~__fan_0:(si : 'sigi)  (_loc : Locf.t)  ->
                    (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                    'interf )))));
          ([`Token
              ({
                 pred =
                   ((function | (`EOI _ : Tokenf.t) -> true | _ -> false));
                 descr = { tag = `EOI; word = Empty; tag_name = "EOI" }
               } : Tokenf.pattern )],
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
            `Keyword ",";
            `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ))],
             ("`Com (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'comma_exp)  ~__fan_1:_ 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     (`Com (_loc, e1, e2) : 'exp_quot )))));
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Keyword ";";
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
          [([`Keyword "="; `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("e\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (e : 'cvalue_bind )))));
          ([`Keyword ":";
           `Keyword "type";
           `Nterm
             (Gramf.obj (unquoted_typevars : 'unquoted_typevars Gramf.t ));
           `Keyword ".";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("let u: FAst.ctyp = `TyPol (_loc, t1, t2) in\n(`Constraint (_loc, e, u) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_6:(e : 'exp)  ~__fan_5:_  ~__fan_4:(t2 : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(t1 : 'unquoted_typevars) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (let u: FAst.ctyp = `TyPol (_loc, t1, t2) in
                     (`Constraint (_loc, e, u) : FAst.exp ) : 'cvalue_bind )))));
          ([`Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`Constraint (_loc, e, t) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_  ~__fan_1:(t : 'ctyp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Constraint (_loc, e, t) : FAst.exp ) : 'cvalue_bind )))));
          ([`Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ":>";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
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
          ([`Keyword ":>";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("`Subtype (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_  ~__fan_1:(t : 'ctyp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Subtype (_loc, e, t) : 'cvalue_bind )))))]) : Gramf.olevel ));
   Gramf.extend (fun_bind : 'fun_bind Gramf.t )
     (None,
       ([(None, (Some `RA),
           [([`Keyword "(";
             `Keyword "type";
             `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
             `Keyword ")";
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
                 (`Keyword ";"))],
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
               ({
                  pred = ((function | `Lid _ -> true | _ -> false));
                  descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                } : Tokenf.pattern );
            `Keyword ":";
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
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
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
          [([`Keyword "(";
            `Keyword "type";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Keyword ")"],
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
           `Keyword "when";
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
             `Keyword "->";
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
           [([`Keyword "let";
             `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
             `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
             `Keyword "in";
             `Self],
              ("`LetIn (_loc, r, bi, x)\n",
                (Gramf.mk_action
                   (fun ~__fan_4:(x : 'exp)  ~__fan_3:_ 
                      ~__fan_2:(bi : 'bind)  ~__fan_1:(r : 'opt_rec) 
                      ~__fan_0:_  (_loc : Locf.t)  ->
                      (`LetIn (_loc, r, bi, x) : 'exp )))));
           ([`Keyword "let";
            `Keyword "module";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
            `Keyword "in";
            `Self],
             ("`LetModule (_loc, m, mb, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(mb : 'mbind0)  ~__fan_2:(m : 'a_uident) 
                     ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`LetModule (_loc, m, mb, e) : 'exp )))));
           ([`Keyword "let";
            `Keyword "open";
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Keyword "in";
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
           ([`Keyword "let";
            `Keyword "open";
            `Keyword "!";
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Keyword "in";
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
           ([`Keyword "let";
            `Keyword "try";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
            `Self;
            `Keyword "with";
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`LetTryInWith (_loc, r, bi, x, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_7:(a : 'case)  ~__fan_6:_  ~__fan_5:(x : 'exp) 
                     ~__fan_4:_  ~__fan_3:(bi : 'bind) 
                     ~__fan_2:(r : 'opt_rec)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (`LetTryInWith (_loc, r, bi, x, a) : 'exp )))));
           ([`Keyword "match";
            `Self;
            `Keyword "with";
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`Match (_loc, e, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(a : 'case)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Match (_loc, e, a) : 'exp )))));
           ([`Keyword "try";
            `Self;
            `Keyword "with";
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("`Try (_loc, e, a)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(a : 'case)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Try (_loc, e, a) : 'exp )))));
           ([`Keyword "if";
            `Self;
            `Keyword "then";
            `Self;
            `Keyword "else";
            `Self],
             ("`IfThenElse (_loc, e1, e2, e3)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e3 : 'exp)  ~__fan_4:_  ~__fan_3:(e2 : 'exp)
                      ~__fan_2:_  ~__fan_1:(e1 : 'exp)  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (`IfThenElse (_loc, e1, e2, e3) : 'exp )))));
           ([`Keyword "if"; `Self; `Keyword "then"; `Self],
             ("`IfThen (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(e2 : 'exp)  ~__fan_2:_  ~__fan_1:(e1 : 'exp)
                      ~__fan_0:_  (_loc : Locf.t)  ->
                     (`IfThen (_loc, e1, e2) : 'exp )))));
           ([`Keyword "do";
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Keyword "done"],
             ("`Seq (_loc, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:_  ~__fan_1:(seq : 'sequence)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Seq (_loc, seq) : 'exp )))));
           ([`Keyword "with";
            `Nterm (Gramf.obj (lang : 'lang Gramf.t ));
            `Self],
             ("Ast_quotation.default := old; x\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(x : 'exp)  ~__fan_1:(old : 'lang) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (Ast_quotation.default := old; x : 'exp )))));
           ([`Keyword "with";
            `Keyword "{";
            `Nterm (Gramf.obj (pos_exps : 'pos_exps Gramf.t ));
            `Keyword "}";
            `Self],
             ("Ast_quotation.map := old; x\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(x : 'exp)  ~__fan_3:_ 
                     ~__fan_2:(old : 'pos_exps)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (Ast_quotation.map := old; x : 'exp )))));
           ([`Keyword "for";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Keyword "=";
            `Self;
            `Nterm (Gramf.obj (flag : 'flag Gramf.t ));
            `Self;
            `Keyword "do";
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Keyword "done"],
             ("`For (_loc, i, e1, e2, df, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_8:_  ~__fan_7:(seq : 'sequence)  ~__fan_6:_ 
                     ~__fan_5:(e2 : 'exp)  ~__fan_4:(df : 'flag) 
                     ~__fan_3:(e1 : 'exp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`For (_loc, i, e1, e2, df, seq) : 'exp )))));
           ([`Keyword "while";
            `Self;
            `Keyword "do";
            `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
            `Keyword "done"],
             ("`While (_loc, e, seq)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:_  ~__fan_3:(seq : 'sequence)  ~__fan_2:_ 
                     ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                     (`While (_loc, e, seq) : 'exp )))))]);
        ((Some ":="), (Some `NA),
          [([`Self; `Keyword ":="; `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self; `Keyword "<-"; `Self],
            ("match Fan_ops.bigarray_set _loc e1 e2 with\n| Some e -> e\n| None  -> `Assign (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:_  ~__fan_0:(e1 : 'exp) 
                    (_loc : Locf.t)  ->
                    (match Fan_ops.bigarray_set _loc e1 e2 with
                     | Some e -> e
                     | None  -> `Assign (_loc, e1, e2) : 'exp )))))]);
        ((Some "||"), (Some `RA),
          [([`Self; `Keyword "or"; `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self; `Keyword "||"; `Self],
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
          [([`Self; `Keyword "&"; `Self],
             ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                     ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     let xloc = __fan_1.loc in
                     let op = __fan_1.txt in
                     (let op: FAst.exp = `Lid (xloc, op) in
                      (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                       'exp )))));
          ([`Self; `Keyword "&&"; `Self],
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
              ({
                 pred =
                   ((function
                     | `Inf ({ level = 0;_} : Tokenf.op) -> true
                     | _ -> false));
                 descr = { tag = `Inf; word = (Level 0); tag_name = "Inf" }
               } : Tokenf.pattern );
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
          ([`Self; `Keyword "=="; `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self; `Keyword "="; `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self; `Keyword "<"; `Self],
            ("let op: FAst.exp = `Lid (xloc, x) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, x) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self; `Keyword ">"; `Self],
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
              ({
                 pred =
                   ((function
                     | `Inf ({ level = 1;_} : Tokenf.op) -> true
                     | _ -> false));
                 descr = { tag = `Inf; word = (Level 1); tag_name = "Inf" }
               } : Tokenf.pattern );
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
          [([`Self; `Keyword "::"; `Self],
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
              ({
                 pred =
                   ((function
                     | `Inf ({ level = 2;_} : Tokenf.op) -> true
                     | _ -> false));
                 descr = { tag = `Inf; word = (Level 2); tag_name = "Inf" }
               } : Tokenf.pattern );
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
          ([`Self; `Keyword "+"; `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self; `Keyword "-"; `Self],
            ("let op: FAst.exp = `Lid (xloc, op) in\n(`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'exp)  ~__fan_1:(__fan_1 : Tokenf.txt) 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let op = __fan_1.txt in
                    (let op: FAst.exp = `Lid (xloc, op) in
                     (`App (_loc, (`App (_loc, op, e1)), e2) : FAst.exp ) : 
                      'exp )))));
          ([`Self; `Keyword "-."; `Self],
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
              ({
                 pred =
                   ((function
                     | `Inf ({ level = 3;_} : Tokenf.op) -> true
                     | _ -> false));
                 descr = { tag = `Inf; word = (Level 3); tag_name = "Inf" }
               } : Tokenf.pattern );
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
              ({
                 pred =
                   ((function
                     | `Inf ({ level = 4;_} : Tokenf.op) -> true
                     | _ -> false));
                 descr = { tag = `Inf; word = (Level 4); tag_name = "Inf" }
               } : Tokenf.pattern );
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
          [([`Keyword "fun";
            `Keyword "|";
            `List1sep
              ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                (`Keyword "|"))],
             ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(a : 'case0 list)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     (let cases = bar_of_list a in `Fun (_loc, cases) : 
                     'exp )))));
          ([`Keyword "function";
           `Keyword "|";
           `List1sep
             ((`Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))), (`Keyword "|"))],
            ("let cases = bar_of_list a in `Fun (_loc, cases)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(a : 'case0 list)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (let cases = bar_of_list a in `Fun (_loc, cases) : 
                    'exp )))));
          ([`Keyword "fun"; `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_def)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (e : 'exp )))));
          ([`Keyword "function";
           `Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'fun_def)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (e : 'exp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ")";
           `Keyword "end"],
            ("match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_
                     ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = None in
                    (match cst with
                     | Some cst -> `ObjPat (_loc, p, cst)
                     | None  -> `ObjPatEnd (_loc, p) : 'exp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ")";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
            ("match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(cst : 'class_structure) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let cst = Some cst in
                    (match cst with
                     | Some cst -> `ObjPat (_loc, p, cst)
                     | None  -> `ObjPatEnd (_loc, p) : 'exp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")";
           `Keyword "end"],
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
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
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
          ([`Keyword "object"; `Keyword "end"],
            ("match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = None in
                    (match cst with
                     | Some cst -> `Obj (_loc, cst)
                     | None  -> `ObjEnd _loc : 'exp )))));
          ([`Keyword "object";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
            ("match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(cst : 'class_structure) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    let cst = Some cst in
                    (match cst with
                     | Some cst -> `Obj (_loc, cst)
                     | None  -> `ObjEnd _loc : 'exp )))))]);
        ((Some "unary minus"), (Some `NA),
          [([`Keyword "-"; `Self],
             ("Fan_ops.mkumin _loc x e\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                     (_loc : Locf.t)  ->
                     let x = __fan_0.txt in (Fan_ops.mkumin _loc x e : 'exp )))));
          ([`Keyword "-."; `Self],
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
          ([`Keyword "assert"; `Self],
            ("`Assert (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Assert (_loc, e) : 'exp )))));
          ([`Keyword "new";
           `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
            ("`New (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'class_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`New (_loc, i) : 'exp )))));
          ([`Keyword "lazy"; `Self],
            ("`Lazy (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Lazy (_loc, e) : 'exp )))))]);
        ((Some "label"), (Some `NA),
          [([`Keyword "~";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Keyword ":";
            `Self],
             ("`Label (_loc, i, e)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(e : 'exp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`Label (_loc, i, e) : 'exp )))));
          ([`Keyword "~"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`LabelS (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`LabelS (_loc, i) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Label _ -> true | _ -> false));
                 descr = { tag = `Label; word = Any; tag_name = "Label" }
               } : Tokenf.pattern );
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
                 pred = ((function | `Optlabel _ -> true | _ -> false));
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
          ([`Keyword "?";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Self],
            ("`OptLabl (_loc, i, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(e : 'exp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OptLabl (_loc, i, e) : 'exp )))));
          ([`Keyword "?"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`OptLablS (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`OptLablS (_loc, i) : 'exp )))))]);
        ((Some "."), (Some `LA),
          [([`Self; `Keyword "."; `Keyword "("; `Self; `Keyword ")"],
             ("`ArrayDot (_loc, e1, e2)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:_  ~__fan_3:(e2 : 'exp)  ~__fan_2:_ 
                     ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                     (`ArrayDot (_loc, e1, e2) : 'exp )))));
          ([`Self; `Keyword "."; `Keyword "["; `Self; `Keyword "]"],
            ("`StringDot (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(e2 : 'exp)  ~__fan_2:_ 
                    ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (`StringDot (_loc, e1, e2) : 'exp )))));
          ([`Self;
           `Keyword ".";
           `Keyword "{";
           `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
           `Keyword "}"],
            ("Fan_ops.bigarray_get _loc e1 e2\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(e2 : 'comma_exp)  ~__fan_2:_ 
                    ~__fan_1:_  ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (Fan_ops.bigarray_get _loc e1 e2 : 'exp )))));
          ([`Self;
           `Keyword ".";
           `Nterm (Gramf.obj (label_longident : 'label_longident Gramf.t ))],
            ("`Field (_loc, e1, e2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e2 : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(e1 : 'exp)  (_loc : Locf.t)  ->
                    (`Field (_loc, e1, e2) : 'exp )))));
          ([`Self;
           `Keyword "#";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Send (_loc, e, lab)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(lab : 'a_lident)  ~__fan_1:_ 
                    ~__fan_0:(e : 'exp)  (_loc : Locf.t)  ->
                    (`Send (_loc, e, lab) : 'exp )))))]);
        ((Some "~-"), (Some `NA),
          [([`Keyword "!"; `Self],
             ("`App (_loc, (`Lid (xloc, x)), e)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(e : 'exp)  ~__fan_0:(__fan_0 : Tokenf.txt) 
                     (_loc : Locf.t)  ->
                     let xloc = __fan_0.loc in
                     let x = __fan_0.txt in
                     (`App (_loc, (`Lid (xloc, x)), e) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Pre _ -> true | _ -> false));
                 descr = { tag = `Pre; word = Any; tag_name = "Pre" }
               } : Tokenf.pattern );
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
               ({
                  pred = ((function | `Quot _ -> true | _ -> false));
                  descr = { tag = `Quot; word = Any; tag_name = "Quot" }
                } : Tokenf.pattern )],
             ("Ast_quotation.expand x Dyn_tag.exp\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                     let x = __fan_0 in
                     (Ast_quotation.expand x Dyn_tag.exp : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "exp";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "exp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "par"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "seq";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "seq"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "chr";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "chr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "int"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int32";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "int32"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "str";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "str"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int64";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "int64"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "flo";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "flo"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "nativeint";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "nativeint"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "chr'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "chr'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int64'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "int64'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "nativeint'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "nativeint'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "bool'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "bool'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "int'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "int32'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "int32'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "flo'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "flo'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "str'";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "str'"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`chr";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "`chr"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`int64";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "`int64"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`nativeint";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "`nativeint"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`bool";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "`bool"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`int";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "`int"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`int32";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "`int32"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`flo";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "`flo"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "`str";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "`str"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"exp" s : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Int _ -> true | _ -> false));
                 descr = { tag = `Int; word = Any; tag_name = "Int" }
               } : Tokenf.pattern )],
            ("`Int (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Int32 _ -> true | _ -> false));
                 descr = { tag = `Int32; word = Any; tag_name = "Int32" }
               } : Tokenf.pattern )],
            ("`Int32 (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int32 (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Int64 _ -> true | _ -> false));
                 descr = { tag = `Int64; word = Any; tag_name = "Int64" }
               } : Tokenf.pattern )],
            ("`Int64 (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Int64 (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Nativeint _ -> true | _ -> false));
                 descr =
                   { tag = `Nativeint; word = Any; tag_name = "Nativeint" }
               } : Tokenf.pattern )],
            ("`Nativeint (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Nativeint (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Flo _ -> true | _ -> false));
                 descr = { tag = `Flo; word = Any; tag_name = "Flo" }
               } : Tokenf.pattern )],
            ("`Flo (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Flo (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Chr _ -> true | _ -> false));
                 descr = { tag = `Chr; word = Any; tag_name = "Chr" }
               } : Tokenf.pattern )],
            ("`Chr (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Chr (_loc, s) : 'exp )))));
          ([`Token
              ({
                 pred = ((function | `Str _ -> true | _ -> false));
                 descr = { tag = `Str; word = Any; tag_name = "Str" }
               } : Tokenf.pattern )],
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
           `Keyword ")"],
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
          ([`Keyword "`"; `Nterm (Gramf.obj (luident : 'luident Gramf.t ))],
            ("`Vrn (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(s : 'luident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Vrn (_loc, s) : 'exp )))));
          ([`Keyword "["; `Keyword "]"],
            ("(`Uid (_loc, \"[]\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Uid (_loc, "[]") : FAst.exp ) : 'exp )))));
          ([`Keyword "[";
           `Nterm (Gramf.obj (sem_exp_for_list : 'sem_exp_for_list Gramf.t ));
           `Keyword "]"],
            ("s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(s : 'sem_exp_for_list) 
                    ~__fan_0:_  (_loc : Locf.t)  -> (s : 'exp )))));
          ([`Keyword "[|"; `Keyword "|]"],
            ("`ArrayEmpty _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ArrayEmpty _loc : 'exp )))));
          ([`Keyword "[|";
           `Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ));
           `Keyword "|]"],
            ("`Array (_loc, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(el : 'sem_exp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Array (_loc, el) : 'exp )))));
          ([`Keyword "{";
           `Token
             ({
                pred = ((function | `Lid _ -> true | _ -> false));
                descr = { tag = `Lid; word = Any; tag_name = "Lid" }
              } : Tokenf.pattern );
           `Keyword "with";
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Keyword "}"],
            ("`RecordWith (_loc, el, (`Lid (xloc, x)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(el : 'label_exp_list)  ~__fan_2:_
                     ~__fan_1:(__fan_1 : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let xloc = __fan_1.loc in
                    let x = __fan_1.txt in
                    (`RecordWith (_loc, el, (`Lid (xloc, x))) : 'exp )))));
          ([`Keyword "{";
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Keyword "}"],
            ("`Record (_loc, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(el : 'label_exp_list)  ~__fan_0:_
                     (_loc : Locf.t)  -> (`Record (_loc, el) : 'exp )))));
          ([`Keyword "{";
           `Keyword "(";
           `Self;
           `Keyword ")";
           `Keyword "with";
           `Nterm (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
           `Keyword "}"],
            ("`RecordWith (_loc, el, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:(el : 'label_exp_list)  ~__fan_4:_
                     ~__fan_3:_  ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`RecordWith (_loc, el, e) : 
                    'exp )))));
          ([`Keyword "{<"; `Keyword ">}"],
            ("`OvrInstEmpty _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OvrInstEmpty _loc : 'exp )))));
          ([`Keyword "{<";
           `Nterm (Gramf.obj (field_exp_list : 'field_exp_list Gramf.t ));
           `Keyword ">}"],
            ("`OvrInst (_loc, fel)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(fel : 'field_exp_list) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`OvrInst (_loc, fel) : 'exp )))));
          ([`Keyword "("; `Keyword ")"],
            ("(`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")"],
            ("`Constraint (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, e, t) : 'exp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ",";
           `Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
           `Keyword ")"],
            ("`Par (_loc, (`Com (_loc, e, el)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(el : 'comma_exp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Par (_loc, (`Com (_loc, e, el))) : 'exp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ";";
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Keyword ")"],
            ("`Seq (_loc, (`Sem (_loc, e, seq)))\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(seq : 'sequence)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp )))));
          ([`Keyword "("; `Self; `Keyword ";"; `Keyword ")"],
            ("`Seq (_loc, e)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:_  ~__fan_1:(e : 'exp)  ~__fan_0:_
                     (_loc : Locf.t)  -> (`Seq (_loc, e) : 'exp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ":>";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")"],
            ("`Coercion (_loc, e, t, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:(t2 : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(t : 'ctyp)  ~__fan_2:_  ~__fan_1:(e : 'exp) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Coercion (_loc, e, t, t2) : 'exp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ":>";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")"],
            ("`Subtype (_loc, e, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(e : 'exp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Subtype (_loc, e, t) : 'exp )))));
          ([`Keyword "("; `Self; `Keyword ")"],
            ("e\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(e : 'exp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (e : 'exp )))));
          ([`Keyword "begin"; `Keyword "end"],
            ("match seq with\n| Some seq -> `Seq (_loc, seq)\n| None  -> (`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    let seq = None in
                    (match seq with
                     | Some seq -> `Seq (_loc, seq)
                     | None  -> (`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Keyword "begin";
           `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
           `Keyword "end"],
            ("match seq with\n| Some seq -> `Seq (_loc, seq)\n| None  -> (`Uid (_loc, \"()\") : FAst.exp )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(seq : 'sequence)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let seq = Some seq in
                    (match seq with
                     | Some seq -> `Seq (_loc, seq)
                     | None  -> (`Uid (_loc, "()") : FAst.exp ) : 'exp )))));
          ([`Keyword "(";
           `Keyword "module";
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
           `Keyword ")"],
            ("`Package_exp (_loc, me)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:_  ~__fan_2:(me : 'mexp)  ~__fan_1:_ 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Package_exp (_loc, me) : 'exp )))));
          ([`Keyword "(";
           `Keyword "module";
           `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
           `Keyword ")"],
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
          [([`Nterm (Gramf.obj (exp : 'exp Gramf.t )); `Keyword ";"; `Self],
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
          ([`Nterm (Gramf.obj (exp : 'exp Gramf.t )); `Keyword ";"],
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
          [([`Keyword "let";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
             ("k (`LetIn (_loc, rf, bi, e))\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(k : 'sequence')  ~__fan_4:(e : 'exp) 
                     ~__fan_3:_  ~__fan_2:(bi : 'bind) 
                     ~__fan_1:(rf : 'opt_rec)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (k (`LetIn (_loc, rf, bi, e)) : 'sequence )))));
          ([`Keyword "let";
           `Keyword "try";
           `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
           `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
           `Keyword "in";
           `Self;
           `Keyword "with";
           `Nterm (Gramf.obj (case : 'case Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetTryInWith (_loc, r, bi, x, a))\n",
              (Gramf.mk_action
                 (fun ~__fan_8:(k : 'sequence')  ~__fan_7:(a : 'case) 
                    ~__fan_6:_  ~__fan_5:(x : 'sequence)  ~__fan_4:_ 
                    ~__fan_3:(bi : 'bind)  ~__fan_2:(r : 'opt_rec) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence )))));
          ([`Keyword "let";
           `Keyword "module";
           `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
           `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
           `Keyword "in";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
           `Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))],
            ("k (`LetModule (_loc, m, mb, e))\n",
              (Gramf.mk_action
                 (fun ~__fan_6:(k : 'sequence')  ~__fan_5:(e : 'exp) 
                    ~__fan_4:_  ~__fan_3:(mb : 'mbind0) 
                    ~__fan_2:(m : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (k (`LetModule (_loc, m, mb, e)) : 'sequence )))));
          ([`Keyword "let";
           `Keyword "open";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
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
          ([`Keyword "let";
           `Keyword "open";
           `Keyword "!";
           `Nterm (Gramf.obj (module_longident : 'module_longident Gramf.t ));
           `Keyword "in";
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
          ([`Keyword ";"],
            ("fun e  -> e\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  ->
                    (fun e  -> e : 'sequence' )))));
          ([`Keyword ";"; `Nterm (Gramf.obj (sequence : 'sequence Gramf.t ))],
            ("fun e  -> `Sem (_loc, e, el)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(el : 'sequence)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (fun e  -> `Sem (_loc, e, el) : 'sequence' )))))]) : 
       Gramf.olevel )));
  Gramf.extend_single (with_exp_lang : 'with_exp_lang Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (lang : 'lang Gramf.t ));
           `Keyword ":";
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
           `Keyword ":";
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
                  pred =
                    ((function
                      | `Ant ({ kind = "bind";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A "bind"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"bind\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"bind" s : 'bind )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"bind\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"bind" s : 'bind )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword "=";
           `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
            ("(`Bind (_loc, (mk_ant ~c:\"pat\" s), e) : FAst.bind )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Bind (_loc, (mk_ant ~c:"pat" s), e) : FAst.bind ) : 
                      'bind )))));
          ([`Self; `Keyword "and"; `Self],
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
                  pred =
                    ((function
                      | `Ant ({ kind = "rec_exp";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr =
                    { tag = `Ant; word = (A "rec_exp"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"rec_exp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'label_exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"rec_exp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'field_exp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "bi";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "bi"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"rec_exp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"rec_exp" s : 'field_exp )))));
          ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword "=";
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
               ({
                  pred = ((function | `Lid _ -> true | _ -> false));
                  descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                } : Tokenf.pattern )],
             ("i\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (i : 'luident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
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
               ({
                  pred = ((function | `Lid _ -> true | _ -> false));
                  descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                } : Tokenf.pattern )],
             ("`C (_loc, i)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (`C (_loc, i) : 'astr )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`C (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`C (_loc, i) : 'astr )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'astr )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'astr )))))]) : Gramf.olevel ));
   Gramf.extend (ident_quot : 'ident_quot Gramf.t )
     (None,
       ([((Some "."), None,
           [([`Self; `Keyword "."; `Self],
              ("(`Dot (_loc, i, j) : FAst.ident )\n",
                (Gramf.mk_action
                   (fun ~__fan_2:(j : 'ident_quot)  ~__fan_1:_ 
                      ~__fan_0:(i : 'ident_quot)  (_loc : Locf.t)  ->
                      ((`Dot (_loc, i, j) : FAst.ident ) : 'ident_quot )))))]);
        ((Some "simple"), None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident_quot )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("(`Lid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Lid (_loc, i) : FAst.ident ) : 'ident_quot )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'ident_quot )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("(`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(j : 'ident_quot)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    ((`Dot (_loc, (`Uid (_loc, s)), j) : FAst.ident ) : 
                      'ident_quot )))));
          ([`Keyword "("; `Self; `Self; `Keyword ")"],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'ident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'ident )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Lid (_loc, i) : 'ident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Uid (_loc, i) : 'ident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ident" s : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'vid)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), i) : 'vid )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Lid (_loc, i) : 'vid )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in (`Uid (_loc, i) : 'vid )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
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
               ({
                  pred = ((function | `Uid _ -> true | _ -> false));
                  descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                } : Tokenf.pattern )],
             ("`Uid (_loc, s)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let s = __fan_0.txt in (`Uid (_loc, s) : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"uident" s : 'uident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("dot (`Uid (_loc, s)) l\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in
                    (dot (`Uid (_loc, s)) l : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("dot (mk_ant ~c:\"uident\" s) i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (dot (mk_ant ~c:"uident" s) i : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("dot (mk_ant ~c:\"uident\" s) i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(i : 'uident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (dot (mk_ant ~c:"uident" s) i : 'uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
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
               ({
                  pred = ((function | `Lid _ -> true | _ -> false));
                  descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                } : Tokenf.pattern )],
             ("((`Sub []), i)\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                     let i = __fan_0.txt in (((`Sub []), i) : 'dot_lstrings )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
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
          ([`Keyword ".";
           `Token
             ({
                pred = ((function | `Uid _ -> true | _ -> false));
                descr = { tag = `Uid; word = Any; tag_name = "Uid" }
              } : Tokenf.pattern );
           `Keyword ".";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern );
            `Keyword ".";
            `Keyword "("],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_2:_  ~__fan_1:_ 
                     ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Keyword "("],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Keyword "("],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("(`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident_dot_lparen)  ~__fan_1:_
                     ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Dot (_loc, (`Uid (_loc, i)), l) : FAst.ident ) : 
                      'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Keyword "("],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt)
                     (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("(`Dot (_loc, (mk_ant ~c:\"ident\" s), l) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident_dot_lparen)  ~__fan_1:_
                     ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    ((`Dot (_loc, (mk_ant ~c:"ident" s), l) : FAst.ident ) : 
                      'module_longident_dot_lparen )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (_loc, i)), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Uid (_loc, i) : 'module_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'module_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'module_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
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
          [([`Self; `Keyword "."; `Self],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(j : 'module_longident_with_app)  ~__fan_1:_ 
                     ~__fan_0:(i : 'module_longident_with_app) 
                     (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'module_longident_with_app )))))]);
        ((Some "simple"), None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'module_longident_with_app )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`Uid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Uid (_loc, i) : 'module_longident_with_app )))));
          ([`Keyword "("; `Self; `Keyword ")"],
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
          [([`Self; `Keyword "."; `Self],
             ("(`Dot (_loc, i, j) : FAst.ident )\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(j : 'type_longident)  ~__fan_1:_ 
                     ~__fan_0:(i : 'type_longident)  (_loc : Locf.t)  ->
                     ((`Dot (_loc, i, j) : FAst.ident ) : 'type_longident )))))]);
        ((Some "simple"), None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'type_longident )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("(`Lid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Lid (_loc, i) : FAst.ident ) : 'type_longident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("(`Uid (_loc, i) : FAst.ident )\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    ((`Uid (_loc, i) : FAst.ident ) : 'type_longident )))));
          ([`Keyword "("; `Self; `Keyword ")"],
            ("i\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(i : 'type_longident)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (i : 'type_longident )))))])] : 
       Gramf.olevel list ));
   Gramf.extend_single (label_longident : 'label_longident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"ident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ident" s : 'label_longident )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("`Lid (_loc, i)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let i = __fan_0.txt in
                    (`Lid (_loc, i) : 'label_longident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (`Uid (iloc, i)), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let iloc = __fan_0.loc in
                    let i = __fan_0.txt in
                    (`Dot (_loc, (`Uid (iloc, i)), l) : 'label_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
           `Self],
            ("`Dot (_loc, (mk_ant ~c:\"ident\" s), l)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(l : 'label_longident)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Dot (_loc, (mk_ant ~c:"ident" s), l) : 'label_longident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ".";
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
          ([`Keyword "!"],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(bang : Tokenf.txt)  (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'opt_override )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "!";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "!"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_override )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "override"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'opt_override )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (flag : 'flag Gramf.t )
     (None,
       ((None, None,
          [([`Keyword "to"],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'flag )))));
          ([`Keyword "downto"],
            ("`Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Negative _loc : 'flag )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "to";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "to"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'flag )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"flag" s : 'flag )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (opt_private : 'opt_private Gramf.t )
     (None,
       ((None, None,
          [([`Keyword "private"],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_private )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "private";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "private"); tag_name = "Ant" }
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
          [([`Keyword "mutable"],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_mutable )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "mutable";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "mutable"); tag_name = "Ant" }
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
          [([`Keyword "virtual"],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_virtual )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "virtual";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr =
                   { tag = `Ant; word = (A "virtual"); tag_name = "Ant" }
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
          [([`Keyword ".."],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_dot_dot )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "..";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ".."); tag_name = "Ant" }
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
          [([`Keyword "rec"],
             ("`Positive _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Positive _loc : 'opt_rec )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "rec";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "rec"); tag_name = "Ant" }
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"a_lident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"a_lident" s : 'a_lident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "lid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "lid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"a_lident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"a_lident" s : 'a_lident )))));
          ([`Token
              ({
                 pred = ((function | `Lid _ -> true | _ -> false));
                 descr = { tag = `Lid; word = Any; tag_name = "Lid" }
               } : Tokenf.pattern )],
            ("`Lid (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Lid (_loc, s) : 'a_lident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (a_uident : 'a_uident Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"a_uident\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"a_uident" s : 'a_uident )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "uid";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "uid"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"a_uident\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"a_uident" s : 'a_uident )))));
          ([`Token
              ({
                 pred = ((function | `Uid _ -> true | _ -> false));
                 descr = { tag = `Uid; word = Any; tag_name = "Uid" }
               } : Tokenf.pattern )],
            ("`Uid (_loc, s)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let s = __fan_0.txt in (`Uid (_loc, s) : 'a_uident )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (string_list : 'string_list Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"str_list\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"str_list" s : 'string_list )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`App (_loc, (mk_ant ~c:\"\" s), xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'string_list) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`App (_loc, (mk_ant ~c:"" s), xs) : 'string_list )))));
          ([`Token
              ({
                 pred = ((function | `Str _ -> true | _ -> false));
                 descr = { tag = `Str; word = Any; tag_name = "Str" }
               } : Tokenf.pattern )],
            ("`Str (_loc, x)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                    let x = __fan_0.txt in (`Str (_loc, x) : 'string_list )))));
          ([`Token
              ({
                 pred = ((function | `Str _ -> true | _ -> false));
                 descr = { tag = `Str; word = Any; tag_name = "Str" }
               } : Tokenf.pattern );
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
              ({
                 pred =
                   ((function | (`EOI _ : Tokenf.t) -> true | _ -> false));
                 descr = { tag = `EOI; word = Empty; tag_name = "EOI" }
               } : Tokenf.pattern )],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:(x : 'pat)  (_loc : Locf.t)  ->
                     (x : 'pat_eoi )))))]) : Gramf.olevel ));
   Gramf.extend_single (exp_eoi : 'exp_eoi Gramf.t )
     (None,
       ((None, None,
          [([`Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Token
              ({
                 pred =
                   ((function | (`EOI _ : Tokenf.t) -> true | _ -> false));
                 descr = { tag = `EOI; word = Empty; tag_name = "EOI" }
               } : Tokenf.pattern )],
             ("x\n",
               (Gramf.mk_action
                  (fun ~__fan_1:_  ~__fan_0:(x : 'exp)  (_loc : Locf.t)  ->
                     (x : 'exp_eoi )))))]) : Gramf.olevel )));
  (Gramf.extend_single (implem : 'implem Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  pred = ((function | `DirQuotation _ -> true | _ -> false));
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
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"; `Self],
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
              ({
                 pred =
                   ((function | (`EOI _ : Tokenf.t) -> true | _ -> false));
                 descr = { tag = `EOI; word = Empty; tag_name = "EOI" }
               } : Tokenf.pattern )],
            ("([], None)\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  -> (([], None) : 'implem )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (top_phrase : 'top_phrase Gramf.t )
     (None,
       ((None, None,
          [([`Keyword "#";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Keyword ";;"],
             ("Some (`Directive (_loc, n, dp))\n",
               (Gramf.mk_action
                  (fun ~__fan_3:_  ~__fan_2:(dp : 'exp) 
                     ~__fan_1:(n : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (Some (`Directive (_loc, n, dp)) : 'top_phrase )))));
          ([`Keyword "#";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ";;"],
            ("Some (`DirectiveSimple (_loc, n))\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(n : 'a_lident)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (Some (`DirectiveSimple (_loc, n)) : 'top_phrase )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"],
            ("Some st\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (Some st : 'top_phrase )))));
          ([`Token
              ({
                 pred =
                   ((function | (`EOI _ : Tokenf.t) -> true | _ -> false));
                 descr = { tag = `EOI; word = Empty; tag_name = "EOI" }
               } : Tokenf.pattern )],
            ("None\n",
              (Gramf.mk_action
                 (fun ~__fan_0:_  (_loc : Locf.t)  -> (None : 'top_phrase )))))]) : 
       Gramf.olevel ));
   Gramf.extend_single (strus : 'strus Gramf.t )
     (None,
       ((None, None,
          [([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "stri"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;"],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;"],
            ("mk_ant ~c:\"stru\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"stru" s : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'strus)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'strus)  ~__fan_0:(__fan_0 : Tokenf.ant)
                     (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;";
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"stru\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'strus)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"stru" s), st) : 'strus )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "stri"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";;";
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
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"],
            ("st\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (st : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(xs : 'strus)  ~__fan_0:(st : 'stru) 
                    (_loc : Locf.t)  -> (`Sem (_loc, st, xs) : 'strus )))));
          ([`Nterm (Gramf.obj (stru : 'stru Gramf.t )); `Keyword ";;"; `Self],
            ("`Sem (_loc, st, xs)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(xs : 'strus)  ~__fan_1:_ 
                    ~__fan_0:(st : 'stru)  (_loc : Locf.t)  ->
                    (`Sem (_loc, st, xs) : 'strus )))))]) : Gramf.olevel ));
   Gramf.extend_single (stru_quot : 'stru_quot Gramf.t )
     (None,
       ((None, None,
          [([`Keyword "#";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("`Directive (_loc, n, dp)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(dp : 'exp)  ~__fan_1:(n : 'a_lident) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Directive (_loc, n, dp) : 'stru_quot )))));
          ([`Keyword "#"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
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
           [([`Keyword "include"; `Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))],
              ("`Include (_loc, me)\n",
                (Gramf.mk_action
                   (fun ~__fan_1:(me : 'mexp)  ~__fan_0:_  (_loc : Locf.t) 
                      -> (`Include (_loc, me) : 'stru )))));
           ([`Keyword "module";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ))],
             ("`Module (_loc, i, mb)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(mb : 'mbind0)  ~__fan_1:(i : 'a_uident) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`Module (_loc, i, mb) : 'stru )))));
           ([`Keyword "module";
            `Keyword "rec";
            `Nterm (Gramf.obj (mbind : 'mbind Gramf.t ))],
             ("`RecModule (_loc, mb)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(mb : 'mbind)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`RecModule (_loc, mb) : 'stru )))));
           ([`Keyword "open";
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
           ([`Keyword "open";
            `Keyword "!";
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
           ([`Keyword "type";
            `Nterm
              (Gramf.obj (type_declaration : 'type_declaration Gramf.t ))],
             ("`Type (_loc, t)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(t : 'type_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Type (_loc, t) : 'stru )))));
           ([`Keyword "module";
            `Keyword "type";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Keyword "=";
            `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))],
             ("`ModuleType (_loc, i, mt)\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(mt : 'mtyp)  ~__fan_3:_ 
                     ~__fan_2:(i : 'a_uident)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`ModuleType (_loc, i, mt) : 
                     'stru )))));
           ([`Keyword "class";
            `Keyword "type";
            `Nterm
              (Gramf.obj (cltyp_declaration : 'cltyp_declaration Gramf.t ))],
             ("`ClassType (_loc, ctd)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(ctd : 'cltyp_declaration)  ~__fan_1:_ 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`ClassType (_loc, ctd) : 'stru )))));
           ([`Keyword "exception";
            `Nterm
              (Gramf.obj
                 (constructor_declaration : 'constructor_declaration Gramf.t ))],
             ("`Exception (_loc, t)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(t : 'constructor_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Exception (_loc, t) : 'stru )))));
           ([`Keyword "external";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
            `Keyword ":";
            `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
            `Keyword "=";
            `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))],
             ("`External (_loc, i, t, sl)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(sl : 'string_list)  ~__fan_4:_ 
                     ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                     ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (`External (_loc, i, t, sl) : 'stru )))));
           ([`Keyword "type";
            `Nterm
              (Gramf.obj (type_declaration : 'type_declaration Gramf.t ));
            `Keyword "with";
            `Keyword "(";
            `Nterm (Gramf.obj (string_list : 'string_list Gramf.t ));
            `Keyword ")"],
             ("`TypeWith (_loc, t, ns)\n",
               (Gramf.mk_action
                  (fun ~__fan_5:_  ~__fan_4:(ns : 'string_list)  ~__fan_3:_ 
                     ~__fan_2:_  ~__fan_1:(t : 'type_declaration)  ~__fan_0:_
                      (_loc : Locf.t)  -> (`TypeWith (_loc, t, ns) : 
                     'stru )))));
           ([`Keyword "let";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru )) (`LetIn (_loc, r, bi, x))\n",
               (Gramf.mk_action
                  (fun ~__fan_4:(x : 'exp)  ~__fan_3:_  ~__fan_2:(bi : 'bind)
                      ~__fan_1:(r : 'opt_rec)  ~__fan_0:_  (_loc : Locf.t) 
                     ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetIn (_loc, r, bi, x)) : 'stru )))));
           ([`Keyword "let";
            `Keyword "module";
            `Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
            `Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
            `Keyword "in";
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru )) (`LetModule (_loc, m, mb, e))\n",
               (Gramf.mk_action
                  (fun ~__fan_5:(e : 'exp)  ~__fan_4:_ 
                     ~__fan_3:(mb : 'mbind0)  ~__fan_2:(m : 'a_uident) 
                     ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetModule (_loc, m, mb, e)) : 'stru )))));
           ([`Keyword "let";
            `Keyword "open";
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Keyword "in";
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
           ([`Keyword "let";
            `Keyword "open";
            `Keyword "!";
            `Nterm
              (Gramf.obj (module_longident : 'module_longident Gramf.t ));
            `Keyword "in";
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
           ([`Keyword "let";
            `Keyword "try";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
            `Nterm (Gramf.obj (exp : 'exp Gramf.t ));
            `Keyword "with";
            `Nterm (Gramf.obj (case : 'case Gramf.t ))],
             ("(fun x  -> (`StExp (_loc, x) : FAst.stru ))\n  (`LetTryInWith (_loc, r, bi, x, a))\n",
               (Gramf.mk_action
                  (fun ~__fan_7:(a : 'case)  ~__fan_6:_  ~__fan_5:(x : 'exp) 
                     ~__fan_4:_  ~__fan_3:(bi : 'bind) 
                     ~__fan_2:(r : 'opt_rec)  ~__fan_1:_  ~__fan_0:_ 
                     (_loc : Locf.t)  ->
                     ((fun x  -> (`StExp (_loc, x) : FAst.stru ))
                        (`LetTryInWith (_loc, r, bi, x, a)) : 'stru )))));
           ([`Keyword "let";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ))],
             ("match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(bi : 'bind)  ~__fan_1:(r : 'opt_rec) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (match bi with
                      | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                      | _ -> `Value (_loc, r, bi) : 'stru )))));
           ([`Keyword "class";
            `Nterm
              (Gramf.obj (class_declaration : 'class_declaration Gramf.t ))],
             ("`Class (_loc, cd)\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(cd : 'class_declaration)  ~__fan_0:_ 
                     (_loc : Locf.t)  -> (`Class (_loc, cd) : 'stru )))));
           ([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'stru )))));
           ([`Token
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "stri";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A "stri"); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"stru\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"stru" s : 'stru )))));
           ([`Token
               ({
                  pred = ((function | `Quot _ -> true | _ -> false));
                  descr = { tag = `Quot; word = Any; tag_name = "Quot" }
                } : Tokenf.pattern )],
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clsigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "csg"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";"],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "csg"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";"],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clsigi" s : 'class_signature )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "csg"); tag_name = "Ant" }
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";";
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
                 pred =
                   ((function
                     | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "csg"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";";
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
          ([`Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); `Keyword ";"],
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
           `Keyword ";";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clsigi\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clsigi" s : 'clsigi )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "csg";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "csg"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clsigi\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clsigi" s : 'clsigi )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clsigi\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clsigi : 'clsigi )))));
          ([`Keyword "inherit"; `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))],
            ("`SigInherit (_loc, cs)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(cs : 'cltyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`SigInherit (_loc, cs) : 'clsigi )))));
          ([`Keyword "val";
           `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
           `Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`CgVal (_loc, l, mf, mv, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(mv : 'opt_virtual) 
                    ~__fan_1:(mf : 'opt_mutable)  ~__fan_0:_  (_loc : Locf.t)
                     ->
                    ((`CgVal (_loc, l, mf, mv, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Keyword "method";
           `Keyword "virtual";
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`VirMeth (_loc, l, pf, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(pf : 'opt_private) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`VirMeth (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Keyword "method";
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("(`Method (_loc, l, pf, t) : FAst.clsigi )\n",
              (Gramf.mk_action
                 (fun ~__fan_4:(t : 'ctyp)  ~__fan_3:_ 
                    ~__fan_2:(l : 'a_lident)  ~__fan_1:(pf : 'opt_private) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    ((`Method (_loc, l, pf, t) : FAst.clsigi ) : 'clsigi )))));
          ([`Keyword "constraint";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clfield\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in
                     (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cst"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";"],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";"],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.ant) 
                    (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clfield" s : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'class_structure) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(st : 'class_structure) 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";";
           `Self],
            ("`Sem (_loc, (mk_ant ~c:\"clfield\" s), st)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(st : 'class_structure)  ~__fan_1:_ 
                    ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (`Sem (_loc, (mk_ant ~c:"clfield" s), st) : 'class_structure )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cst"); tag_name = "Ant" }
               } : Tokenf.pattern );
           `Keyword ";";
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
          ([`Nterm (Gramf.obj (clfield : 'clfield Gramf.t )); `Keyword ";"],
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
           `Keyword ";";
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
          [([`Keyword "val"],
             ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     let bang = None in
                     (match bang with
                      | Some _ -> `Positive _loc
                      | None  -> `Negative _loc : 'value_val_opt_override )))));
          ([`Keyword "val"; `Keyword "!"],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'value_val_opt_override )))));
          ([`Keyword "val";
           `Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'value_val_opt_override )))));
          ([`Keyword "val";
           `Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr =
                  { tag = `Ant; word = (A "override"); tag_name = "Ant" }
              } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'value_val_opt_override )))));
          ([`Keyword "val";
           `Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "!";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "!"); tag_name = "Ant" }
              } : Tokenf.pattern )],
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
          [([`Keyword "method"],
             ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
               (Gramf.mk_action
                  (fun ~__fan_0:_  (_loc : Locf.t)  ->
                     let bang = None in
                     (match bang with
                      | Some _ -> `Positive _loc
                      | None  -> `Negative _loc : 'method_opt_override )))));
          ([`Keyword "method"; `Keyword "!"],
            ("match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(bang : Tokenf.txt)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let bang = Some bang in
                    (match bang with
                     | Some _ -> `Positive _loc
                     | None  -> `Negative _loc : 'method_opt_override )))));
          ([`Keyword "method";
           `Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
            ("mk_ant ~c:\"flag\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(__fan_1 : Tokenf.ant)  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    let s = __fan_1 in
                    (mk_ant ~c:"flag" s : 'method_opt_override )))));
          ([`Keyword "method";
           `Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "override";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr =
                  { tag = `Ant; word = (A "override"); tag_name = "Ant" }
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clfield\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clfield" s : 'clfield )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cst";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cst"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clfield\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clfield" s : 'clfield )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clfield\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clfield : 'clfield )))));
          ([`Keyword "inherit";
           `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("`Inherit (_loc, o, ce)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(ce : 'clexp)  ~__fan_1:(o : 'opt_override) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Inherit (_loc, o, ce) : 'clfield )))));
          ([`Keyword "inherit";
           `Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
           `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ));
           `Keyword "as";
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
          ([`Keyword "val";
           `Keyword "virtual";
           `Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`VirVal (_loc, l, mf, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:(t : 'ctyp)  ~__fan_4:_ 
                    ~__fan_3:(l : 'a_lident)  ~__fan_2:(mf : 'opt_mutable) 
                    ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`VirVal (_loc, l, mf, t) : 'clfield )))));
          ([`Keyword "method";
           `Keyword "virtual";
           `Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
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
           `Keyword ":";
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
          ([`Keyword "constraint";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
            ("`Eq (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Eq (_loc, t1, t2) : 'clfield )))));
          ([`Keyword "initializer"; `Nterm (Gramf.obj (exp : 'exp Gramf.t ))],
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
          [([`Self; `Keyword "and"; `Self],
             ("`And (_loc, c1, c2)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(c2 : 'class_declaration)  ~__fan_1:_ 
                     ~__fan_0:(c1 : 'class_declaration)  (_loc : Locf.t)  ->
                     (`And (_loc, c1, c2) : 'class_declaration )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clexp" s : 'class_declaration )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cdcl";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cdcl"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"clexp" s : 'class_declaration )))));
          ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword "[";
           `Nterm
             (Gramf.obj
                (comma_type_parameter : 'comma_type_parameter Gramf.t ));
           `Keyword "]";
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
          [([`Keyword "="; `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
             ("ce\n",
               (Gramf.mk_action
                  (fun ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t) 
                     -> (ce : 'class_fun_bind )))));
          ([`Keyword ":";
           `Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ));
           `Keyword "=";
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
          ([`Keyword "->"; `Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))],
            ("ce\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (ce : 'class_fun_def )))))]) : Gramf.olevel ));
   Gramf.extend (clexp : 'clexp Gramf.t )
     (None,
       ([((Some "top"), None,
           [([`Keyword "fun";
             `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
             `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
              ("`CeFun (_loc, p, ce)\n",
                (Gramf.mk_action
                   (fun ~__fan_2:(ce : 'class_fun_def)  ~__fan_1:(p : 'ipat) 
                      ~__fan_0:_  (_loc : Locf.t)  ->
                      (`CeFun (_loc, p, ce) : 'clexp )))));
           ([`Keyword "function";
            `Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
            `Nterm (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))],
             ("`CeFun (_loc, p, ce)\n",
               (Gramf.mk_action
                  (fun ~__fan_2:(ce : 'class_fun_def)  ~__fan_1:(p : 'ipat) 
                     ~__fan_0:_  (_loc : Locf.t)  ->
                     (`CeFun (_loc, p, ce) : 'clexp )))));
           ([`Keyword "let";
            `Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
            `Nterm (Gramf.obj (bind : 'bind Gramf.t ));
            `Keyword "in";
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
               ({
                  pred =
                    ((function
                      | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                      | _ -> false));
                  descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
                } : Tokenf.pattern )],
             ("mk_ant ~c:\"clexp\" s\n",
               (Gramf.mk_action
                  (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                     let s = __fan_0 in (mk_ant ~c:"clexp" s : 'clexp )))));
          ([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "cexp";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A "cexp"); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"clexp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"clexp" s : 'clexp )))));
          ([`Token
              ({
                 pred = ((function | `Quot _ -> true | _ -> false));
                 descr = { tag = `Quot; word = Any; tag_name = "Quot" }
               } : Tokenf.pattern )],
            ("Ast_quotation.expand x Dyn_tag.clexp\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                    let x = __fan_0 in
                    (Ast_quotation.expand x Dyn_tag.clexp : 'clexp )))));
          ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
           `Keyword "[";
           `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
           `Keyword "]"],
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
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ")";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
            ("`ObjPat (_loc, p, cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_5:_  ~__fan_4:(cst : 'class_structure) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`ObjPat (_loc, p, cst) : 'clexp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ")";
           `Keyword "end"],
            ("`ObjPatEnd (_loc, p)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_
                     ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ObjPatEnd (_loc, p) : 'clexp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
            ("`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_7:_  ~__fan_6:(cst : 'class_structure) 
                    ~__fan_5:_  ~__fan_4:(t : 'ctyp)  ~__fan_3:_ 
                    ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                    'clexp )))));
          ([`Keyword "object";
           `Keyword "(";
           `Nterm (Gramf.obj (pat : 'pat Gramf.t ));
           `Keyword ":";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword ")";
           `Keyword "end"],
            ("`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n",
              (Gramf.mk_action
                 (fun ~__fan_6:_  ~__fan_5:_  ~__fan_4:(t : 'ctyp) 
                    ~__fan_3:_  ~__fan_2:(p : 'pat)  ~__fan_1:_  ~__fan_0:_ 
                    (_loc : Locf.t)  ->
                    (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                    'clexp )))));
          ([`Keyword "object";
           `Nterm (Gramf.obj (class_structure : 'class_structure Gramf.t ));
           `Keyword "end"],
            ("`Obj (_loc, cst)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(cst : 'class_structure) 
                    ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Obj (_loc, cst) : 'clexp )))));
          ([`Keyword "object"; `Keyword "end"],
            ("`ObjEnd _loc\n",
              (Gramf.mk_action
                 (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`ObjEnd _loc : 'clexp )))));
          ([`Keyword "(";
           `Self;
           `Keyword ":";
           `Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ));
           `Keyword ")"],
            ("`Constraint (_loc, ce, ct)\n",
              (Gramf.mk_action
                 (fun ~__fan_4:_  ~__fan_3:(ct : 'cltyp)  ~__fan_2:_ 
                    ~__fan_1:(ce : 'clexp)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Constraint (_loc, ce, ct) : 'clexp )))));
          ([`Keyword "("; `Self; `Keyword ")"],
            ("ce\n",
              (Gramf.mk_action
                 (fun ~__fan_2:_  ~__fan_1:(ce : 'clexp)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (ce : 'clexp )))))])] : Gramf.olevel
                                                                  list )));
  Gramf.extend_single (class_description : 'class_description Gramf.t )
    (None,
      ((None, None,
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, cd1, cd2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(cd2 : 'class_description)  ~__fan_1:_ 
                    ~__fan_0:(cd1 : 'class_description)  (_loc : Locf.t)  ->
                    (`And (_loc, cd1, cd2) : 'class_description )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'class_description )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'class_description )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword "[";
          `Nterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Keyword "]";
          `Keyword ":";
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
          `Keyword ":";
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
         [([`Self; `Keyword "and"; `Self],
            ("`And (_loc, cd1, cd2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(cd2 : 'cltyp_declaration)  ~__fan_1:_ 
                    ~__fan_0:(cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                    (`And (_loc, cd1, cd2) : 'cltyp_declaration )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'cltyp_declaration )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"cltyp" s : 'cltyp_declaration )))));
         ([`Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword "[";
          `Nterm
            (Gramf.obj
               (comma_type_parameter : 'comma_type_parameter Gramf.t ));
          `Keyword "]";
          `Keyword "=";
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
          `Keyword "=";
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
         [([`Keyword "[";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "]";
           `Keyword "->";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"cltyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"cltyp" s : 'cltyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "ctyp";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "ctyp"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"cltyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"cltyp" s : 'cltyp )))));
         ([`Token
             ({
                pred = ((function | `Quot _ -> true | _ -> false));
                descr = { tag = `Quot; word = Any; tag_name = "Quot" }
              } : Tokenf.pattern )],
           ("Ast_quotation.expand x Dyn_tag.cltyp\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                   let x = __fan_0 in
                   (Ast_quotation.expand x Dyn_tag.cltyp : 'cltyp )))));
         ([`Nterm (Gramf.obj (vid : 'vid Gramf.t ));
          `Keyword "[";
          `Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
          `Keyword "]"],
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
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
          `Keyword "end"],
           ("`ObjTy (_loc, t, csg)\n",
             (Gramf.mk_action
                (fun ~__fan_5:_  ~__fan_4:(csg : 'class_signature) 
                   ~__fan_3:_  ~__fan_2:(t : 'ctyp)  ~__fan_1:_  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 'cltyp )))));
         ([`Keyword "object";
          `Nterm (Gramf.obj (class_signature : 'class_signature Gramf.t ));
          `Keyword "end"],
           ("`Obj (_loc, csg)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(csg : 'class_signature) 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Obj (_loc, csg) : 'cltyp )))));
         ([`Keyword "object";
          `Keyword "(";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword ")"],
           ("`ObjTyEnd (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(t : 'ctyp)  ~__fan_1:_  ~__fan_0:_
                    (_loc : Locf.t)  -> (`ObjTyEnd (_loc, t) : 'cltyp )))));
         ([`Keyword "object"; `Keyword "end"],
           ("`ObjEnd _loc\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`ObjEnd _loc : 'cltyp )))))]) : Gramf.olevel ))
let apply_ctyp () =
  Gramf.extend_single (ctyp_quot : 'ctyp_quot Gramf.t )
    (None,
      ((None, None,
         [([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "*";
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
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'unquoted_typevars )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant s : 'type_parameter )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant s : 'type_parameter )))));
         ([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`Quote (_loc, (`Normal _loc), i) : 'type_parameter )))));
         ([`Keyword "+";
          `Keyword "'";
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
         ([`Keyword "-";
          `Keyword "'";
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
         ([`Keyword "+"; `Keyword "_"],
           ("`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`QuoteAny
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc)) : 
                     'type_parameter )))));
         ([`Keyword "-"; `Keyword "_"],
           ("`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(__fan_0 : Tokenf.txt) 
                   (_loc : Locf.t)  ->
                   let p = __fan_0.txt in
                   (`QuoteAny
                      (_loc,
                        (if p = "+" then `Positive _loc else `Negative _loc)) : 
                     'type_parameter )))));
         ([`Keyword "_"],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Any _loc : 'type_parameter )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (type_longident_and_parameters : 'type_longident_and_parameters Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "(";
           `Nterm (Gramf.obj (type_parameters : 'type_parameters Gramf.t ));
           `Keyword ")";
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
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
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
           `Keyword ";";
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
          `Keyword ";";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'meth_decl )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'meth_decl )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'row_field )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'row_field )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("`TyVrn (_loc, (mk_ant ~c:\"ctyp\" s))\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`TyVrn (_loc, (mk_ant ~c:"ctyp" s)) : 'row_field )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "vrn";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "vrn"); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Keyword "of";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyVrnOf (_loc, (mk_ant ~c:\"ctyp\" s), t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (`TyVrnOf (_loc, (mk_ant ~c:"ctyp" s), t) : 'row_field )))));
         ([`Self; `Keyword "|"; `Self],
           ("`Bar (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'row_field)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'row_field)  (_loc : Locf.t)  ->
                   (`Bar (_loc, t1, t2) : 'row_field )))));
         ([`Keyword "`"; `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
           ("`TyVrn (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'astr)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyVrn (_loc, i) : 'row_field )))));
         ([`Keyword "`";
          `Nterm (Gramf.obj (astr : 'astr Gramf.t ));
          `Keyword "of";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'name_tags )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
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
         ([`Keyword "`"; `Nterm (Gramf.obj (astr : 'astr Gramf.t ))],
           ("`TyVrn (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'astr)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyVrn (_loc, i) : 'name_tags )))))]) : Gramf.olevel ));
  Gramf.extend_single (type_declaration : 'type_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'type_declaration )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'type_declaration )))));
         ([`Self; `Keyword "and"; `Self],
           ("`And (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'type_declaration)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'type_declaration)  (_loc : Locf.t)  ->
                   (`And (_loc, t1, t2) : 'type_declaration )))));
         ([`Nterm
             (Gramf.obj
                (type_ident_and_parameters : 'type_ident_and_parameters
                                               Gramf.t ));
          `Keyword "=";
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
          `Keyword "=";
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
         ([`Keyword "private"; `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyEq (_loc, (`Positive _loc), t1)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t1 : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`TyEq (_loc, (`Positive _loc), t1) : 'type_info )))));
         ([`Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
          `Keyword "=";
          `Keyword "private";
          `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyMan (_loc, t1, (`Positive _loc), t2)\n",
             (Gramf.mk_action
                (fun ~__fan_3:(t2 : 'type_repr)  ~__fan_2:_  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                   (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info )))));
         ([`Keyword "private";
          `Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))],
           ("`TyRepr (_loc, (`Positive _loc), t2)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(t2 : 'type_repr)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (type_repr : 'type_repr Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "|";
           `Nterm
             (Gramf.obj
                (constructor_declarations : 'constructor_declarations Gramf.t ))],
            ("`Sum (_loc, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(t : 'constructor_declarations)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (`Sum (_loc, t) : 'type_repr )))));
         ([`Keyword "{";
          `Nterm
            (Gramf.obj
               (label_declaration_list : 'label_declaration_list Gramf.t ));
          `Keyword "}"],
           ("`Record (_loc, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'label_declaration_list) 
                   ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Record (_loc, t) : 'type_repr )))))]) : Gramf.olevel ));
  Gramf.extend_single
    (type_ident_and_parameters : 'type_ident_and_parameters Gramf.t )
    (None,
      ((None, None,
         [([`Keyword "(";
           `List1sep
             ((`Nterm (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))),
               (`Keyword ","));
           `Keyword ")";
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
         [([`Keyword "constraint";
           `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
           `Keyword "=";
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
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'typevars )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'typevars )))));
         ([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
           ("`Quote (_loc, (`Normal _loc), i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                   -> (`Quote (_loc, (`Normal _loc), i) : 'typevars )))))]) : 
      Gramf.olevel ));
  Gramf.extend (ctyp : 'ctyp Gramf.t )
    (None,
      ([((Some "alias"), (Some `LA),
          [([`Self;
            `Keyword "as";
            `Keyword "'";
            `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
             ("`Alias (_loc, t1, i)\n",
               (Gramf.mk_action
                  (fun ~__fan_3:(i : 'a_lident)  ~__fan_2:_  ~__fan_1:_ 
                     ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                     (`Alias (_loc, t1, i) : 'ctyp )))))]);
       ((Some "forall"), (Some `LA),
         [([`Keyword "!";
           `Nterm (Gramf.obj (typevars : 'typevars Gramf.t ));
           `Keyword ".";
           `Self],
            ("`TyPol (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t2 : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(t1 : 'typevars)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`TyPol (_loc, t1, t2) : 'ctyp )))))]);
       ((Some "arrow"), (Some `RA),
         [([`Self; `Keyword "->"; `Self],
            ("`Arrow (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'ctyp)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'ctyp)  (_loc : Locf.t)  ->
                    (`Arrow (_loc, t1, t2) : 'ctyp )))))]);
       ((Some "label"), (Some `NA),
         [([`Keyword "~";
           `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
           `Keyword ":";
           `Self],
            ("`Label (_loc, i, t)\n",
              (Gramf.mk_action
                 (fun ~__fan_3:(t : 'ctyp)  ~__fan_2:_ 
                    ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t)  ->
                    (`Label (_loc, i, t) : 'ctyp )))));
         ([`Token
             ({
                pred = ((function | `Label _ -> true | _ -> false));
                descr = { tag = `Label; word = Any; tag_name = "Label" }
              } : Tokenf.pattern );
          `Keyword ":";
          `Self],
           ("`Label (_loc, (`Lid (_loc, s)), t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let s = __fan_0.txt in
                   (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp )))));
         ([`Token
             ({
                pred = ((function | `Optlabel _ -> true | _ -> false));
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
         ([`Keyword "?";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
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
         [([`Keyword "'"; `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))],
            ("`Quote (_loc, (`Normal _loc), i)\n",
              (Gramf.mk_action
                 (fun ~__fan_1:(i : 'a_lident)  ~__fan_0:_  (_loc : Locf.t) 
                    -> (`Quote (_loc, (`Normal _loc), i) : 'ctyp )))));
         ([`Keyword "_"],
           ("`Any _loc\n",
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  -> (`Any _loc : 'ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "par";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "par"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "id";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "id"); tag_name = "Ant" }
              } : Tokenf.pattern );
          `Keyword ".";
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
          `Keyword ".";
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
         ([`Keyword "(";
          `Self;
          `Keyword "*";
          `Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ));
          `Keyword ")"],
           ("`Par (_loc, (`Sta (_loc, t, tl)))\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(tl : 'star_ctyp)  ~__fan_2:_ 
                   ~__fan_1:(t : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp )))));
         ([`Keyword "("; `Self; `Keyword ")"],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'ctyp)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (t : 'ctyp )))));
         ([`Keyword "(";
          `Self;
          `Keyword ",";
          `Nterm (Gramf.obj (com_ctyp : 'com_ctyp Gramf.t ));
          `Keyword ")";
          `Nterm (Gramf.obj (type_longident : 'type_longident Gramf.t ))],
           ("appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n",
             (Gramf.mk_action
                (fun ~__fan_5:(j : 'type_longident)  ~__fan_4:_ 
                   ~__fan_3:(tl : 'com_ctyp)  ~__fan_2:_ 
                   ~__fan_1:(t : 'ctyp)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (appl_of_list ((j :>ctyp) :: t ::
                      (Ast_basic.list_of_com tl [])) : 'ctyp )))));
         ([`Keyword "[";
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Keyword "]"],
           ("`PolyEq (_loc, rfl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`PolyEq (_loc, rfl) : 'ctyp )))));
         ([`Keyword "[>";
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Keyword "]"],
           ("`PolySup (_loc, rfl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`PolySup (_loc, rfl) : 'ctyp )))));
         ([`Keyword "[<";
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Keyword "]"],
           ("match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(rfl : 'row_field)  ~__fan_0:_ 
                   (_loc : Locf.t)  ->
                   let ntl = None in
                   (match ntl with
                    | None  -> `PolyInf (_loc, rfl)
                    | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 'ctyp )))));
         ([`Keyword "[<";
          `Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
          `Keyword ">";
          `Nterm (Gramf.obj (name_tags : 'name_tags Gramf.t ));
          `Keyword "]"],
           ("match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n",
             (Gramf.mk_action
                (fun ~__fan_4:_  ~__fan_3:(ntl : 'name_tags)  ~__fan_2:_ 
                   ~__fan_1:(rfl : 'row_field)  ~__fan_0:_  (_loc : Locf.t) 
                   ->
                   let ntl = Some ntl in
                   (match ntl with
                    | None  -> `PolyInf (_loc, rfl)
                    | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 'ctyp )))));
         ([`Keyword "#";
          `Nterm (Gramf.obj (class_longident : 'class_longident Gramf.t ))],
           ("`ClassPath (_loc, i)\n",
             (Gramf.mk_action
                (fun ~__fan_1:(i : 'class_longident)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (`ClassPath (_loc, i) : 'ctyp )))));
         ([`Keyword "<";
          `Nterm (Gramf.obj (opt_meth_list : 'opt_meth_list Gramf.t ));
          `Keyword ">"],
           ("t\n",
             (Gramf.mk_action
                (fun ~__fan_2:_  ~__fan_1:(t : 'opt_meth_list)  ~__fan_0:_ 
                   (_loc : Locf.t)  -> (t : 'ctyp )))));
         ([`Keyword "(";
          `Keyword "module";
          `Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
          `Keyword ")"],
           ("`Package (_loc, p)\n",
             (Gramf.mk_action
                (fun ~__fan_3:_  ~__fan_2:(p : 'mtyp)  ~__fan_1:_  ~__fan_0:_
                    (_loc : Locf.t)  -> (`Package (_loc, p) : 'ctyp )))))])] : 
      Gramf.olevel list ));
  Gramf.extend_single (comma_ctyp : 'comma_ctyp Gramf.t )
    (None,
      ((None, None,
         [([`Self; `Keyword ","; `Self],
            ("`Com (_loc, t1, t2)\n",
              (Gramf.mk_action
                 (fun ~__fan_2:(t2 : 'comma_ctyp)  ~__fan_1:_ 
                    ~__fan_0:(t1 : 'comma_ctyp)  (_loc : Locf.t)  ->
                    (`Com (_loc, t1, t2) : 'comma_ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
              } : Tokenf.pattern )],
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'com_ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'com_ctyp )))));
         ([`Self; `Keyword ","; `Self],
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'star_ctyp )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in (mk_ant ~c:"ctyp" s : 'star_ctyp )))));
         ([`Self; `Keyword "*"; `Self],
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'constructor_declarations )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'constructor_declarations )))));
         ([`Self; `Keyword "|"; `Self],
           ("`Bar (_loc, t1, t2)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t2 : 'constructor_declarations)  ~__fan_1:_ 
                   ~__fan_0:(t1 : 'constructor_declarations)  (_loc : Locf.t)
                    -> (`Bar (_loc, t1, t2) : 'constructor_declarations )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword "of";
          `Nterm
            (Gramf.obj
               (constructor_arg_list : 'constructor_arg_list Gramf.t ))],
           ("`Of (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'constructor_arg_list)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_uident)  (_loc : Locf.t)  ->
                   (`Of (_loc, s, t) : 'constructor_declarations )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword ":";
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
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'constructor_declaration )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'constructor_declaration )))));
         ([`Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
          `Keyword "of";
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
         [([`Self; `Keyword "*"; `Self],
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
           `Keyword ";";
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
          `Keyword ";"],
           ("t1\n",
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(t1 : 'label_declaration) 
                   (_loc : Locf.t)  -> (t1 : 'label_declaration_list )))))]) : 
      Gramf.olevel ));
  Gramf.extend_single (label_declaration : 'label_declaration Gramf.t )
    (None,
      ((None, None,
         [([`Token
              ({
                 pred =
                   ((function
                     | `Ant ({ kind = "";_} : Tokenf.ant) -> true
                     | _ -> false));
                 descr = { tag = `Ant; word = (A ""); tag_name = "Ant" }
               } : Tokenf.pattern )],
            ("mk_ant ~c:\"ctyp\" s\n",
              (Gramf.mk_action
                 (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                    let s = __fan_0 in
                    (mk_ant ~c:"ctyp" s : 'label_declaration )))));
         ([`Token
             ({
                pred =
                  ((function
                    | `Ant ({ kind = "typ";_} : Tokenf.ant) -> true
                    | _ -> false));
                descr = { tag = `Ant; word = (A "typ"); tag_name = "Ant" }
              } : Tokenf.pattern )],
           ("mk_ant ~c:\"ctyp\" s\n",
             (Gramf.mk_action
                (fun ~__fan_0:(__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                   let s = __fan_0 in
                   (mk_ant ~c:"ctyp" s : 'label_declaration )))));
         ([`Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
          `Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))],
           ("`TyCol (_loc, s, t)\n",
             (Gramf.mk_action
                (fun ~__fan_2:(t : 'ctyp)  ~__fan_1:_ 
                   ~__fan_0:(s : 'a_lident)  (_loc : Locf.t)  ->
                   (`TyCol (_loc, s, t) : 'label_declaration )))));
         ([`Keyword "mutable";
          `Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
          `Keyword ":";
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
         [([`Self; `Keyword ","; `Self],
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
