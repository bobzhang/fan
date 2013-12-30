let mk_ant = Tokenf.mk_ant
let ident_of_ctyp = Fan_ops.ident_of_ctyp
let (<+>) = Ast_gen.( <+> ) 
let apply = Ast_gen.apply
let dot = Ast_gen.dot
let bar_of_list = Ast_gen.bar_of_list
let and_of_list = Ast_gen.and_of_list
let com_of_list = Ast_gen.com_of_list
let appl_of_list = Ast_gen.appl_of_list
open Astf
open! Syntaxf
let pos_exps = Gramf.mk "pos_exps"
let make_infix ?(left= true)  exp f i =
  Gramf.extend_single
    ({
       entry = (exp : 'exp Gramf.t );
       olevel =
         ({
            label = (Some (f i));
            lassoc = left;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Inf; word = (Level i); tag_name = "Inf" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "let op = (`Lid (xloc, op) :>Astf.exp) in\n(`App\n   (_loc, (`App (_loc, (op :>Astf.exp), (e1 :>Astf.exp))), (e2 :>Astf.exp)) :>\n  Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e2 : 'exp)  (__fan_1 : Tokenf.op)  (e1 : 'exp) 
                         (_loc : Locf.t)  ->
                         let xloc = __fan_1.loc in
                         let op = __fan_1.txt in
                         (let op = (`Lid (xloc, op) :>Astf.exp) in
                          (`App
                             (_loc,
                               (`App (_loc, (op :>Astf.exp), (e1 :>Astf.exp))),
                               (e2 :>Astf.exp)) :>Astf.exp) : 'exp ) : 
                      'exp -> Tokenf.op -> 'exp -> Locf.t -> 'exp ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_key ?(left= true)  ?action  exp i op =
  match action with
  | None  ->
      Gramf.extend_single
        ({
           entry = (exp : 'exp Gramf.t );
           olevel =
             ({
                label = (Some i);
                lassoc = left;
                productions =
                  [{
                     symbols =
                       [Self;
                       Token
                         ({
                            descr =
                              { tag = `Key; word = (A op); tag_name = "Key" }
                          } : Tokenf.pattern );
                       Self];
                     annot =
                       "let op = (`Lid (xloc, op) :>Astf.exp) in\n(`App\n   (_loc, (`App (_loc, (op :>Astf.exp), (e1 :>Astf.exp))), (e2 :>Astf.exp)) :>\n  Astf.exp)\n";
                     fn =
                       (Gramf.mk_action
                          (fun (e2 : 'exp)  (__fan_1 : Tokenf.txt) 
                             (e1 : 'exp)  (_loc : Locf.t)  ->
                             let xloc = __fan_1.loc in
                             (let op = (`Lid (xloc, op) :>Astf.exp) in
                              (`App
                                 (_loc,
                                   (`App
                                      (_loc, (op :>Astf.exp),
                                        (e1 :>Astf.exp))), (e2 :>Astf.exp)) :>
                                Astf.exp) : 'exp ) : 'exp ->
                                                       Tokenf.txt ->
                                                         'exp ->
                                                           Locf.t -> 'exp ))
                   }]
              } : Gramf.olevel )
         } : _ Gramf.single_extend_statement )
  | Some action ->
      Gramf.extend_single
        ({
           entry = (exp : 'exp Gramf.t );
           olevel =
             ({
                label = (Some i);
                lassoc = left;
                productions =
                  [{
                     symbols =
                       [Self;
                       Token
                         ({
                            descr =
                              { tag = `Key; word = (A op); tag_name = "Key" }
                          } : Tokenf.pattern );
                       Self];
                     annot = "";
                     fn =
                       (Gramf.mk_action
                          (action : 'exp ->
                                      Tokenf.txt -> 'exp -> Locf.t -> 'exp ))
                   }]
              } : Gramf.olevel )
         } : _ Gramf.single_extend_statement )
let _ =
  let transform i =
    List.assoc i [(0, 50); (1, 60); (2, 80); (3, 90); (4, 100)] in
  make_key exp 20 ~left:true
    ~action:(fun e2  _  e1  _loc  ->
               match Fan_ops.bigarray_set _loc e1 e2 with
               | Some e -> e
               | None  ->
                   (`Assign (_loc, (e1 :>Astf.exp), (e2 :>Astf.exp)) :>
                   Astf.exp)) "<-";
  List.iter (make_key exp 20 ~left:true) [":="];
  List.iter (make_key exp 30 ~left:false) ["or"; "||"];
  List.iter (make_key exp 40 ~left:false) ["&"; "&&"];
  List.iter (make_key exp 50 ~left:true) ["=="; "="; "<"; ">"];
  List.iter (make_key exp 80 ~left:true) ["+"; "-"; "-."];
  make_infix exp transform 0;
  make_infix ~left:false exp transform 1;
  make_infix exp transform 2;
  make_infix exp transform 3;
  make_infix exp transform 4
let make_case exp pat =
  let pat_as_pat_opt: 'pat_as_pat_opt Gramf.t = Gramf.mk "pat_as_pat_opt" in
  Gramf.extend_single
    ({
       entry = (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "as"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`Alias (_loc, p1, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (s : 'a_lident)  _  (p1 : 'pat)  (_loc : Locf.t) 
                         -> (`Alias (_loc, p1, s) : 'pat_as_pat_opt ) : 
                      'a_lident ->
                        Tokenf.txt -> 'pat -> Locf.t -> 'pat_as_pat_opt ))
               };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (_loc : Locf.t)  ->
                        (p : 'pat_as_pat_opt ) : 'pat ->
                                                   Locf.t -> 'pat_as_pat_opt ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (case : 'case Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "|"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   List1sep
                     ((Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                       (Token
                          ({
                             descr =
                               { tag = `Key; word = (A "|"); tag_name = "Key"
                               }
                           } : Tokenf.pattern )))];
                 annot = "bar_of_list l\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'case0 list)  _  (_loc : Locf.t)  ->
                         (bar_of_list l : 'case ) : 'case0 list ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'case ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "->"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                annot = "`Case (_loc, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'exp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                        (`Case (_loc, p, e) : 'case ) : 'exp ->
                                                          Tokenf.txt ->
                                                            'pat ->
                                                              Locf.t -> 'case ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (case0 : 'case0 Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "case");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'case0 ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'case0 ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in (mk_ant s : 'case0 ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'case0 ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "when"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "->"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                annot = "`CaseWhen (_loc, (mk_ant s), w, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'exp)  _  (w : 'exp)  _ 
                        (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (`CaseWhen (_loc, (mk_ant s), w, e) : 'case0 ) : 
                     'exp ->
                       Tokenf.txt ->
                         'exp -> Tokenf.txt -> Tokenf.ant -> Locf.t -> 'case0 ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "->"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                annot = "`Case (_loc, (mk_ant s), e)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'exp)  _  (__fan_0 : Tokenf.ant) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (`Case (_loc, (mk_ant s), e) : 'case0 ) : 'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'case0 ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "when"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "->"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                annot = "`CaseWhen (_loc, p, w, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'exp)  _  (w : 'exp)  _  (p : 'pat_as_pat_opt)
                         (_loc : Locf.t)  ->
                        (`CaseWhen (_loc, p, w, e) : 'case0 ) : 'exp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'pat_as_pat_opt
                                                                    ->
                                                                    Locf.t ->
                                                                    'case0 ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (pat_as_pat_opt : 'pat_as_pat_opt Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "->"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                annot = "`Case (_loc, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'exp)  _  (p : 'pat_as_pat_opt) 
                        (_loc : Locf.t)  -> (`Case (_loc, p, e) : 'case0 ) : 
                     'exp ->
                       Tokenf.txt -> 'pat_as_pat_opt -> Locf.t -> 'case0 ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (case_quot : 'case_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [List1sep
                      ((Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                        (Token
                           ({
                              descr =
                                {
                                  tag = `Key;
                                  word = (A "|");
                                  tag_name = "Key"
                                }
                            } : Tokenf.pattern )))];
                 annot = "bar_of_list x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'case0 list)  (_loc : Locf.t)  ->
                         (bar_of_list x : 'case_quot ) : 'case0 list ->
                                                           Locf.t ->
                                                             'case_quot ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_semi atom nt =
  Gramf.extend_single
    ({
       entry = (nt : 'nt Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (atom : 'atom Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, b1, b2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (b2 : 'nt)  _  (b1 : 'atom)  (_loc : Locf.t)  ->
                         (`Sem (_loc, b1, b2) : 'nt ) : 'nt ->
                                                          Tokenf.txt ->
                                                            'atom ->
                                                              Locf.t -> 'nt ))
               };
              {
                symbols = [Nterm (Gramf.obj (atom : 'atom Gramf.t ))];
                annot = "b1\n";
                fn =
                  (Gramf.mk_action
                     (fun (b1 : 'atom)  (_loc : Locf.t)  -> (b1 : 'nt ) : 
                     'atom -> Locf.t -> 'nt ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (atom : 'atom Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "b1\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (b1 : 'atom)  (_loc : Locf.t)  -> (b1 : 'nt ) : 
                     Tokenf.txt -> 'atom -> Locf.t -> 'nt ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_comma atom nt =
  Gramf.extend_single
    ({
       entry = (nt : 'nt Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ","); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Com (_loc, p1, p2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'nt)  _  (p1 : 'nt)  (_loc : Locf.t)  ->
                         (`Com (_loc, p1, p2) : 'nt ) : 'nt ->
                                                          Tokenf.txt ->
                                                            'nt ->
                                                              Locf.t -> 'nt ))
               };
              {
                symbols = [Nterm (Gramf.obj (atom : 'atom Gramf.t ))];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'atom)  (_loc : Locf.t)  -> (p : 'nt ) : 
                     'atom -> Locf.t -> 'nt ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_ant ?(c= "")  ?(i= 10)  nt x =
  Gramf.extend_single
    ({
       entry = (nt : 'nt Gramf.t );
       olevel =
         ({
            label = (Some i);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind x); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant ~c s : 'nt ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'nt ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_ants ?c  ?i  nt xs = List.iter (make_ant ?c ?i nt) xs
let make_quot tag ?(i= 10)  nt =
  Gramf.extend_single
    ({
       entry = (nt : 'nt Gramf.t );
       olevel =
         ({
            label = (Some i);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Quot; word = Any; tag_name = "Quot" }
                       } : Tokenf.pattern )];
                 annot = "Ast_quotation.expand x tag\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                         let x = __fan_0 in
                         (Ast_quotation.expand x tag : 'nt ) : Tokenf.quot ->
                                                                 Locf.t ->
                                                                   'nt ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let make_pat exp =
  let pat_constr: 'pat_constr Gramf.t = Gramf.mk "pat_constr"
  and ep_constr: 'ep_constr Gramf.t = Gramf.mk "ep_constr" in
  Gramf.extend_single
    ({
       entry = (pat_quot : 'pat_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ","); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ))];
                 annot = "`Com (_loc, x, y)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (y : 'comma_pat)  _  (x : 'pat)  (_loc : Locf.t) 
                         -> (`Com (_loc, x, y) : 'pat_quot ) : 'comma_pat ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'pat ->
                                                                    Locf.t ->
                                                                    'pat_quot ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ))];
                annot = "`Sem (_loc, x, y)\n";
                fn =
                  (Gramf.mk_action
                     (fun (y : 'sem_pat)  _  (x : 'pat)  (_loc : Locf.t)  ->
                        (`Sem (_loc, x, y) : 'pat_quot ) : 'sem_pat ->
                                                             Tokenf.txt ->
                                                               'pat ->
                                                                 Locf.t ->
                                                                   'pat_quot ))
              };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot = "x\n";
                fn =
                  (Gramf.mk_action
                     (fun (x : 'pat)  (_loc : Locf.t)  -> (x : 'pat_quot ) : 
                     'pat -> Locf.t -> 'pat_quot ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat_constr : 'pat_constr Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (module_longident : 'module_longident Gramf.t ))];
                 annot = "(i : vid  :>pat)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                         ((i : vid  :>pat) : 'pat_constr ) : 'module_longident
                                                               ->
                                                               Locf.t ->
                                                                 'pat_constr ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (luident : 'luident Gramf.t ))];
                annot = "(`Vrn (_loc, s) : pat )\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                        ((`Vrn (_loc, s) : pat ) : 'pat_constr ) : 'luident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'pat_constr ) : Tokenf.ant -> Locf.t -> 'pat_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "pat"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'pat_constr ) : Tokenf.ant -> Locf.t -> 'pat_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'pat_constr ) : Tokenf.ant -> Locf.t -> 'pat_constr ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ep_constr : 'ep_constr Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (module_longident : 'module_longident Gramf.t ))];
                 annot = "(i : vid  :>ep)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                         ((i : vid  :>ep) : 'ep_constr ) : 'module_longident
                                                             ->
                                                             Locf.t ->
                                                               'ep_constr ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (luident : 'luident Gramf.t ))];
                annot = "(`Vrn (_loc, s) : ep )\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                        ((`Vrn (_loc, s) : ep ) : 'ep_constr ) : 'luident ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s : 
                          'ep_constr ) : Tokenf.ant -> Locf.t -> 'ep_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "ep"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s : 
                          'ep_constr ) : Tokenf.ant -> Locf.t -> 'ep_constr ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ep) s : 
                          'ep_constr ) : Tokenf.ant -> Locf.t -> 'ep_constr ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ep : 'ep Gramf.t );
       olevel =
         ({
            label = (Some 30);
            lassoc = false;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "::"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p1, p2)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'ep)  _  (p1 : 'ep)  (_loc : Locf.t)  ->
                         (`App
                            (_loc, (`Uid (_loc, "::")),
                              (`Par (_loc, (`Com (_loc, p1, p2))))) : 
                         'ep ) : 'ep -> Tokenf.txt -> 'ep -> Locf.t -> 'ep ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ep : 'ep Gramf.t );
       olevel =
         ({
            label = (Some 40);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (ep_constr : 'ep_constr Gramf.t ));
                   Self];
                 annot =
                   "(`App (_loc, (p1 :>Astf.ep), (p2 :>Astf.ep)) :>Astf.ep)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'ep)  (p1 : 'ep_constr)  (_loc : Locf.t)  ->
                         ((`App (_loc, (p1 :>Astf.ep), (p2 :>Astf.ep)) :>
                         Astf.ep) : 'ep ) : 'ep ->
                                              'ep_constr -> Locf.t -> 'ep ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (ep_constr : 'ep_constr Gramf.t ))];
                annot = "p1\n";
                fn =
                  (Gramf.mk_action
                     (fun (p1 : 'ep_constr)  (_loc : Locf.t)  -> (p1 : 'ep ) : 
                     'ep_constr -> Locf.t -> 'ep ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (sem_ep_for_list : 'sem_ep_for_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (ep : 'ep Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p, pl)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (pl : 'sem_ep_for_list)  _  (p : 'ep) 
                         (_loc : Locf.t)  ->
                         (`App
                            (_loc, (`Uid (_loc, "::")),
                              (`Par (_loc, (`Com (_loc, p, pl))))) : 
                         'sem_ep_for_list ) : 'sem_ep_for_list ->
                                                Tokenf.txt ->
                                                  'ep ->
                                                    Locf.t ->
                                                      'sem_ep_for_list ))
               };
              {
                symbols = [Nterm (Gramf.obj (ep : 'ep Gramf.t ))];
                annot =
                  "`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'ep)  (_loc : Locf.t)  ->
                        (`App
                           (_loc, (`Uid (_loc, "::")),
                             (`Par
                                (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                        'sem_ep_for_list ) : 'ep ->
                                               Locf.t -> 'sem_ep_for_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (ep : 'ep Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'ep)  (_loc : Locf.t)  ->
                        (`App
                           (_loc, (`Uid (_loc, "::")),
                             (`Par
                                (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                        'sem_ep_for_list ) : Tokenf.txt ->
                                               'ep ->
                                                 Locf.t -> 'sem_ep_for_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ep : 'ep Gramf.t );
       olevel =
         ({
            label = (Some 50);
            lassoc = true;
            productions =
              [{
                 symbols = [Nterm (Gramf.obj (vid : 'vid Gramf.t ))];
                 annot = "(i : vid  :>ep)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  (_loc : Locf.t)  ->
                         ((i : vid  :>ep) : 'ep ) : 'vid -> Locf.t -> 'ep ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Int; word = Any; tag_name = "Int" }
                      } : Tokenf.pattern )];
                annot = "`Int (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Int32; word = Any; tag_name = "Int32" }
                      } : Tokenf.pattern )];
                annot = "`Int32 (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int32 (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Int64; word = Any; tag_name = "Int64" }
                      } : Tokenf.pattern )];
                annot = "`Int64 (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int64 (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Nativeint;
                            word = Any;
                            tag_name = "Nativeint"
                          }
                      } : Tokenf.pattern )];
                annot = "`Nativeint (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Nativeint (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" }
                      } : Tokenf.pattern )];
                annot = "`Flo (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Flo (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" }
                      } : Tokenf.pattern )];
                annot = "`Chr (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Chr (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                      } : Tokenf.pattern )];
                annot = "`Str (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Str (_loc, s) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "true"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Bool (_loc, true)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Bool (_loc, true) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "false"); tag_name = "Key"
                          }
                      } : Tokenf.pattern )];
                annot = "`Bool (_loc, false)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Bool (_loc, false) : 'ep ) : 
                     Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
                    Tokenf.pattern )];
                annot = "`Int (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int (_loc, (Stringf.neg s)) : 'ep ) : Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Int32; word = Any; tag_name = "Int32" }
                     } : Tokenf.pattern )];
                annot = "`Int32 (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int32 (_loc, (Stringf.neg s)) : 'ep ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Int64; word = Any; tag_name = "Int64" }
                     } : Tokenf.pattern )];
                annot = "`Int64 (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int64 (_loc, (Stringf.neg s)) : 'ep ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         {
                           tag = `Nativeint;
                           word = Any;
                           tag_name = "Nativeint"
                         }
                     } : Tokenf.pattern )];
                annot = "`Nativeint (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Nativeint (_loc, (Stringf.neg s)) : 'ep ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" } } : 
                    Tokenf.pattern )];
                annot = "`Flo (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Flo (_loc, (Stringf.neg s)) : 'ep ) : Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Uid (_loc, \"[]\")\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  ->
                        (`Uid (_loc, "[]") : 'ep ) : Tokenf.txt ->
                                                       Tokenf.txt ->
                                                         Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (sem_ep_for_list : 'sem_ep_for_list Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "s\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (s : 'sem_ep_for_list)  _  (_loc : Locf.t)  ->
                        (s : 'ep ) : Tokenf.txt ->
                                       'sem_ep_for_list ->
                                         Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ArrayEmpty _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  -> (`ArrayEmpty _loc : 'ep ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (sem_ep : 'sem_ep Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Array (_loc, pl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'sem_ep)  _  (_loc : Locf.t)  ->
                        (`Array (_loc, pl) : 'ep ) : Tokenf.txt ->
                                                       'sem_ep ->
                                                         Tokenf.txt ->
                                                           Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "{"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (label_ep_list : 'label_ep_list Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "}"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Record (_loc, pl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'label_ep_list)  _  (_loc : Locf.t)  ->
                        (`Record (_loc, pl) : 'ep ) : Tokenf.txt ->
                                                        'label_ep_list ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Unit _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  -> (`Unit _loc : 'ep ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'ep)  _  (_loc : Locf.t)  -> (p : 'ep ) : 
                     Tokenf.txt -> 'ep -> Tokenf.txt -> Locf.t -> 'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Constraint (_loc, p, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'ctyp)  _  (p : 'ep)  _  (_loc : Locf.t) 
                        -> (`Constraint (_loc, p, t) : 'ep ) : Tokenf.txt ->
                                                                 'ctyp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'ep ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ep ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ","); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (comma_ep : 'comma_ep Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Par (_loc, (`Com (_loc, p, pl)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'comma_ep)  _  (p : 'ep)  _ 
                        (_loc : Locf.t)  ->
                        (`Par (_loc, (`Com (_loc, p, pl))) : 'ep ) : 
                     Tokenf.txt ->
                       'comma_ep ->
                         Tokenf.txt -> 'ep -> Tokenf.txt -> Locf.t -> 'ep ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat : 'pat Gramf.t );
       olevel =
         ({
            label = (Some 10);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Bar (_loc, p1, p2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                         (`Bar (_loc, p1, p2) : 'pat ) : 'pat ->
                                                           Tokenf.txt ->
                                                             'pat ->
                                                               Locf.t -> 'pat ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat : 'pat Gramf.t );
       olevel =
         ({
            label = (Some 20);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ".."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`PaRng (_loc, p1, p2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                         (`PaRng (_loc, p1, p2) : 'pat ) : 'pat ->
                                                             Tokenf.txt ->
                                                               'pat ->
                                                                 Locf.t ->
                                                                   'pat ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat : 'pat Gramf.t );
       olevel =
         ({
            label = (Some 30);
            lassoc = false;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "::"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p1, p2)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'pat)  _  (p1 : 'pat)  (_loc : Locf.t)  ->
                         (`App
                            (_loc, (`Uid (_loc, "::")),
                              (`Par (_loc, (`Com (_loc, p1, p2))))) : 
                         'pat ) : 'pat ->
                                    Tokenf.txt -> 'pat -> Locf.t -> 'pat ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat : 'pat Gramf.t );
       olevel =
         ({
            label = (Some 40);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t ));
                   Self];
                 annot =
                   "(`App (_loc, (p1 :>Astf.pat), (p2 :>Astf.pat)) :>Astf.pat)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'pat)  (p1 : 'pat_constr)  (_loc : Locf.t) 
                         ->
                         ((`App (_loc, (p1 :>Astf.pat), (p2 :>Astf.pat)) :>
                         Astf.pat) : 'pat ) : 'pat ->
                                                'pat_constr -> Locf.t -> 'pat ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (pat_constr : 'pat_constr Gramf.t ))];
                annot = "p1\n";
                fn =
                  (Gramf.mk_action
                     (fun (p1 : 'pat_constr)  (_loc : Locf.t)  ->
                        (p1 : 'pat ) : 'pat_constr -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "lazy"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self];
                annot = "`Lazy (_loc, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  _  (_loc : Locf.t)  ->
                        (`Lazy (_loc, p) : 'pat ) : 'pat ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'pat ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat : 'pat Gramf.t );
       olevel =
         ({
            label = (Some 50);
            lassoc = true;
            productions =
              [{
                 symbols = [Nterm (Gramf.obj (vid : 'vid Gramf.t ))];
                 annot = "(i : vid  :>pat)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  (_loc : Locf.t)  ->
                         ((i : vid  :>pat) : 'pat ) : 'vid -> Locf.t -> 'pat ))
               };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Int; word = Any; tag_name = "Int" }
                      } : Tokenf.pattern )];
                annot = "`Int (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Int32; word = Any; tag_name = "Int32" }
                      } : Tokenf.pattern )];
                annot = "`Int32 (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int32 (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Int64; word = Any; tag_name = "Int64" }
                      } : Tokenf.pattern )];
                annot = "`Int64 (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Int64 (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Nativeint;
                            word = Any;
                            tag_name = "Nativeint"
                          }
                      } : Tokenf.pattern )];
                annot = "`Nativeint (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Nativeint (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" }
                      } : Tokenf.pattern )];
                annot = "`Flo (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Flo (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" }
                      } : Tokenf.pattern )];
                annot = "`Chr (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Chr (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                      } : Tokenf.pattern )];
                annot = "`Str (_loc, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let s = __fan_0.txt in (`Str (_loc, s) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "true"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Bool (_loc, true)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Bool (_loc, true) : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "false"); tag_name = "Key"
                          }
                      } : Tokenf.pattern )];
                annot = "`Bool (_loc, false)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  ->
                        (`Bool (_loc, false) : 'pat ) : Tokenf.txt ->
                                                          Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Int; word = Any; tag_name = "Int" } } : 
                    Tokenf.pattern )];
                annot = "`Int (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int (_loc, (Stringf.neg s)) : 'pat ) : Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Int32; word = Any; tag_name = "Int32" }
                     } : Tokenf.pattern )];
                annot = "`Int32 (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int32 (_loc, (Stringf.neg s)) : 'pat ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Int64; word = Any; tag_name = "Int64" }
                     } : Tokenf.pattern )];
                annot = "`Int64 (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Int64 (_loc, (Stringf.neg s)) : 'pat ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         {
                           tag = `Nativeint;
                           word = Any;
                           tag_name = "Nativeint"
                         }
                     } : Tokenf.pattern )];
                annot = "`Nativeint (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Nativeint (_loc, (Stringf.neg s)) : 'pat ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" } } : 
                    Tokenf.pattern )];
                annot = "`Flo (_loc, (Stringf.neg s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                        let s = __fan_1.txt in
                        (`Flo (_loc, (Stringf.neg s)) : 'pat ) : Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Uid (_loc, \"[]\")\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  ->
                        (`Uid (_loc, "[]") : 'pat ) : Tokenf.txt ->
                                                        Tokenf.txt ->
                                                          Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (sem_pat_for_list : 'sem_pat_for_list Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "s\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (s : 'sem_pat_for_list)  _  (_loc : Locf.t)  ->
                        (s : 'pat ) : Tokenf.txt ->
                                        'sem_pat_for_list ->
                                          Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ArrayEmpty _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  ->
                        (`ArrayEmpty _loc : 'pat ) : Tokenf.txt ->
                                                       Tokenf.txt ->
                                                         Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (sem_pat : 'sem_pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Array (_loc, pl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'sem_pat)  _  (_loc : Locf.t)  ->
                        (`Array (_loc, pl) : 'pat ) : Tokenf.txt ->
                                                        'sem_pat ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "{"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "}"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Record (_loc, pl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                        (`Record (_loc, pl) : 'pat ) : Tokenf.txt ->
                                                         'label_pat_list ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Unit _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  -> (`Unit _loc : 'pat ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ModuleUnpack (_loc, m)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                        (`ModuleUnpack (_loc, m) : 'pat ) : Tokenf.txt ->
                                                              'a_uident ->
                                                                Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "`ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                        (_loc : Locf.t)  ->
                        (`ModuleConstraint (_loc, m, (`Package (_loc, pt))) : 
                        'pat ) : Tokenf.txt ->
                                   'mtyp ->
                                     Tokenf.txt ->
                                       'a_uident ->
                                         Tokenf.txt ->
                                           Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Ant; word = (Kind "opt"); tag_name = "Ant"
                         }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ModuleConstraint (_loc, m, (mk_ant s))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_4 : Tokenf.ant)  _  (m : 'a_uident)  _  _
                         (_loc : Locf.t)  ->
                        let s = __fan_4 in
                        (`ModuleConstraint (_loc, m, (mk_ant s)) : 'pat ) : 
                     Tokenf.txt ->
                       Tokenf.ant ->
                         Tokenf.txt ->
                           'a_uident ->
                             Tokenf.txt -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'pat ) : 
                     Tokenf.txt -> 'pat -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Constraint (_loc, p, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t) 
                        -> (`Constraint (_loc, p, t) : 'pat ) : Tokenf.txt ->
                                                                  'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'pat ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "as"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Alias (_loc, p, s)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (s : 'a_lident)  _  (p : 'pat)  _ 
                        (_loc : Locf.t)  -> (`Alias (_loc, p, s) : 'pat ) : 
                     Tokenf.txt ->
                       'a_lident ->
                         Tokenf.txt -> 'pat -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ","); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (comma_pat : 'comma_pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Par (_loc, (`Com (_loc, p, pl)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'comma_pat)  _  (p : 'pat)  _ 
                        (_loc : Locf.t)  ->
                        (`Par (_loc, (`Com (_loc, p, pl))) : 'pat ) : 
                     Tokenf.txt ->
                       'comma_pat ->
                         Tokenf.txt -> 'pat -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "#"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                annot = "`ClassPath (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'type_longident)  _  (_loc : Locf.t)  ->
                        (`ClassPath (_loc, i) : 'pat ) : 'type_longident ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "~"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Label (_loc, i, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  _  (i : 'a_lident)  _  (_loc : Locf.t) 
                        -> (`Label (_loc, i, p) : 'pat ) : 'pat ->
                                                             Tokenf.txt ->
                                                               'a_lident ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Label; word = Any; tag_name = "Label" }
                      } : Tokenf.pattern );
                  Self];
                annot = "`Label (_loc, (`Lid (_loc, i)), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)
                         ->
                        let i = __fan_0.txt in
                        (`Label (_loc, (`Lid (_loc, i)), p) : 'pat ) : 
                     'pat -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Quot; word = Any; tag_name = "Quot" }
                      } : Tokenf.pattern )];
                annot = "Ast_quotation.expand x Dyn_tag.pat\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                        let x = __fan_0 in
                        (Ast_quotation.expand x Dyn_tag.pat : 'pat ) : 
                     Tokenf.quot -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (luident : 'luident Gramf.t ))];
                annot = "(`Vrn (_loc, s) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                        ((`Vrn (_loc, s) :>Astf.pat) : 'pat ) : 'luident ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "_"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Any _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Any _loc : 'pat ) : 
                     Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "~"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`LabelS (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`LabelS (_loc, i) : 'pat ) : 'a_lident ->
                                                        Tokenf.txt ->
                                                          Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Optlabel;
                            word = Any;
                            tag_name = "Optlabel"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let i = __fan_0.txt in
                        let e = None in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                          'pat ) : Tokenf.txt ->
                                     'pat_tcon ->
                                       Tokenf.txt ->
                                         Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Optlabel;
                            word = Any;
                            tag_name = "Optlabel"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                        (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let i = __fan_0.txt in
                        let e = Some e in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                          'pat ) : Tokenf.txt ->
                                     'exp ->
                                       Tokenf.txt ->
                                         'pat_tcon ->
                                           Tokenf.txt ->
                                             Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                        (_loc : Locf.t)  ->
                        let e = None in
                        (match e with
                         | None  -> `OptLabl (_loc, i, p)
                         | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat ) : 
                     Tokenf.txt ->
                       'pat_tcon ->
                         Tokenf.txt ->
                           Tokenf.txt ->
                             'a_lident -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _ 
                        (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        let e = Some e in
                        (match e with
                         | None  -> `OptLabl (_loc, i, p)
                         | Some e -> `OptLablExpr (_loc, i, p, e) : 'pat ) : 
                     Tokenf.txt ->
                       'exp ->
                         Tokenf.txt ->
                           'pat_tcon ->
                             Tokenf.txt ->
                               Tokenf.txt ->
                                 'a_lident -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Ant; word = (Kind "opt"); tag_name = "Ant"
                         }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`OptLablExpr (_loc, i, p, (mk_ant s))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_6 : Tokenf.ant)  _  (p : 'pat_tcon)  _  _
                         (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        let s = __fan_6 in
                        (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'pat ) : 
                     Tokenf.txt ->
                       Tokenf.ant ->
                         Tokenf.txt ->
                           'pat_tcon ->
                             Tokenf.txt ->
                               Tokenf.txt ->
                                 'a_lident -> Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`OptLablS (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`OptLablS (_loc, i) : 'pat ) : 'a_lident ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                        let e = None in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                          'pat ) : Tokenf.txt ->
                                     'ipat_tcon ->
                                       Tokenf.txt ->
                                         Tokenf.txt -> Locf.t -> 'pat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                        (_loc : Locf.t)  ->
                        let e = Some e in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                          'pat ) : Tokenf.txt ->
                                     'exp ->
                                       Tokenf.txt ->
                                         'ipat_tcon ->
                                           Tokenf.txt ->
                                             Tokenf.txt -> Locf.t -> 'pat ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ipat : 'ipat Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (label_pat_list : 'label_pat_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "(`Record (_loc, (pl :>Astf.rec_pat)) :>Astf.pat)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (pl : 'label_pat_list)  _  (_loc : Locf.t)  ->
                         ((`Record (_loc, (pl :>Astf.rec_pat)) :>Astf.pat) : 
                         'ipat ) : Tokenf.txt ->
                                     'label_pat_list ->
                                       Tokenf.txt -> Locf.t -> 'ipat ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'ipat ) : Tokenf.ant -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "pat"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'ipat ) : Tokenf.ant -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "par"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                          'ipat ) : Tokenf.ant -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Unit _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  -> (`Unit _loc : 'ipat ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match pt with\n| None  -> `ModuleUnpack (_loc, m)\n| Some pt -> `ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                        let pt = None in
                        (match pt with
                         | None  -> `ModuleUnpack (_loc, m)
                         | Some pt ->
                             `ModuleConstraint
                               (_loc, m, (`Package (_loc, pt))) : 'ipat ) : 
                     Tokenf.txt ->
                       'a_uident ->
                         Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match pt with\n| None  -> `ModuleUnpack (_loc, m)\n| Some pt -> `ModuleConstraint (_loc, m, (`Package (_loc, pt)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pt : 'mtyp)  _  (m : 'a_uident)  _  _ 
                        (_loc : Locf.t)  ->
                        let pt = Some pt in
                        (match pt with
                         | None  -> `ModuleUnpack (_loc, m)
                         | Some pt ->
                             `ModuleConstraint
                               (_loc, m, (`Package (_loc, pt))) : 'ipat ) : 
                     Tokenf.txt ->
                       'mtyp ->
                         Tokenf.txt ->
                           'a_uident ->
                             Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Ant; word = (Kind "opt"); tag_name = "Ant"
                         }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ModuleConstraint (_loc, m, (mk_ant s))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_4 : Tokenf.ant)  _  (m : 'a_uident)  _  _
                         (_loc : Locf.t)  ->
                        let s = __fan_4 in
                        (`ModuleConstraint (_loc, m, (mk_ant s)) : 'ipat ) : 
                     Tokenf.txt ->
                       Tokenf.ant ->
                         Tokenf.txt ->
                           'a_uident ->
                             Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat)  _  (_loc : Locf.t)  -> (p : 'ipat ) : 
                     Tokenf.txt -> 'pat -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "(`Constraint (_loc, (p :>Astf.pat), (t :>Astf.ctyp)) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'ctyp)  _  (p : 'pat)  _  (_loc : Locf.t) 
                        ->
                        ((`Constraint (_loc, (p :>Astf.pat), (t :>Astf.ctyp)) :>
                        Astf.pat) : 'ipat ) : Tokenf.txt ->
                                                'ctyp ->
                                                  Tokenf.txt ->
                                                    'pat ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "as"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "(`Alias (_loc, (p :>Astf.pat), s) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (s : 'a_lident)  _  (p : 'pat)  _ 
                        (_loc : Locf.t)  ->
                        ((`Alias (_loc, (p :>Astf.pat), s) :>Astf.pat) : 
                        'ipat ) : Tokenf.txt ->
                                    'a_lident ->
                                      Tokenf.txt ->
                                        'pat -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ","); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (comma_ipat : 'comma_ipat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "(`Par (_loc, (`Com (_loc, (p :>Astf.pat), (pl :>Astf.pat)))) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (pl : 'comma_ipat)  _  (p : 'pat)  _ 
                        (_loc : Locf.t)  ->
                        ((`Par
                            (_loc,
                              (`Com (_loc, (p :>Astf.pat), (pl :>Astf.pat)))) :>
                        Astf.pat) : 'ipat ) : Tokenf.txt ->
                                                'comma_ipat ->
                                                  Tokenf.txt ->
                                                    'pat ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'ipat ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(s : alident  :>pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'a_lident)  (_loc : Locf.t)  ->
                        ((s : alident  :>pat) : 'ipat ) : 'a_lident ->
                                                            Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Label; word = Any; tag_name = "Label" }
                      } : Tokenf.pattern );
                  Self];
                annot =
                  "(`Label (_loc, (`Lid (_loc, i)), (p :>Astf.pat)) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'ipat)  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let i = __fan_0.txt in
                        ((`Label (_loc, (`Lid (_loc, i)), (p :>Astf.pat)) :>
                          Astf.pat) : 'ipat ) : 'ipat ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "~"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "(`Label (_loc, i, (p :>Astf.pat)) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'ipat)  _  (i : 'a_lident)  _  (_loc : Locf.t)
                         ->
                        ((`Label (_loc, i, (p :>Astf.pat)) :>Astf.pat) : 
                        'ipat ) : 'ipat ->
                                    Tokenf.txt ->
                                      'a_lident ->
                                        Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Quot; word = Any; tag_name = "Quot" }
                      } : Tokenf.pattern )];
                annot = "Ast_quotation.expand x Dyn_tag.pat\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                        let x = __fan_0 in
                        (Ast_quotation.expand x Dyn_tag.pat : 'ipat ) : 
                     Tokenf.quot -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (luident : 'luident Gramf.t ))];
                annot = "(`Vrn (_loc, s) :>Astf.pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                        ((`Vrn (_loc, s) :>Astf.pat) : 'ipat ) : 'luident ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "_"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Any _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Any _loc : 'ipat ) : 
                     Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "~"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`LabelS (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`LabelS (_loc, i) : 'ipat ) : 'a_lident ->
                                                         Tokenf.txt ->
                                                           Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Optlabel;
                            word = Any;
                            tag_name = "Optlabel"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat_tcon)  _  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let i = __fan_0.txt in
                        let e = None in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                          'ipat ) : Tokenf.txt ->
                                      'pat_tcon ->
                                        Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Optlabel;
                            word = Any;
                            tag_name = "Optlabel"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, i)), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _ 
                        (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let i = __fan_0.txt in
                        let e = Some e in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, i)), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, i)), p) : 
                          'ipat ) : Tokenf.txt ->
                                      'exp ->
                                        Tokenf.txt ->
                                          'pat_tcon ->
                                            Tokenf.txt ->
                                              Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat_tcon)  _  _  (i : 'a_lident)  _ 
                        (_loc : Locf.t)  ->
                        let e = None in
                        (match e with
                         | None  -> `OptLabl (_loc, i, p)
                         | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat ) : 
                     Tokenf.txt ->
                       'pat_tcon ->
                         Tokenf.txt ->
                           Tokenf.txt ->
                             'a_lident -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| None  -> `OptLabl (_loc, i, p)\n| Some e -> `OptLablExpr (_loc, i, p, e)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'pat_tcon)  _  _ 
                        (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        let e = Some e in
                        (match e with
                         | None  -> `OptLabl (_loc, i, p)
                         | Some e -> `OptLablExpr (_loc, i, p, e) : 'ipat ) : 
                     Tokenf.txt ->
                       'exp ->
                         Tokenf.txt ->
                           'pat_tcon ->
                             Tokenf.txt ->
                               Tokenf.txt ->
                                 'a_lident -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat_tcon : 'pat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Ant; word = (Kind "opt"); tag_name = "Ant"
                         }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`OptLablExpr (_loc, i, p, (mk_ant s))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_6 : Tokenf.ant)  _  (p : 'pat_tcon)  _  _
                         (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        let s = __fan_6 in
                        (`OptLablExpr (_loc, i, p, (mk_ant s)) : 'ipat ) : 
                     Tokenf.txt ->
                       Tokenf.ant ->
                         Tokenf.txt ->
                           'pat_tcon ->
                             Tokenf.txt ->
                               Tokenf.txt ->
                                 'a_lident -> Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`OptLablS (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`OptLablS (_loc, i) : 'ipat ) : 'a_lident ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'ipat_tcon)  _  _  (_loc : Locf.t)  ->
                        let e = None in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                          'ipat ) : Tokenf.txt ->
                                      'ipat_tcon ->
                                        Tokenf.txt ->
                                          Tokenf.txt -> Locf.t -> 'ipat ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ipat_tcon : 'ipat_tcon Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match e with\n| Some e -> `OptLablExpr (_loc, (`Lid (_loc, \"\")), p, e)\n| None  -> `OptLabl (_loc, (`Lid (_loc, \"\")), p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (e : 'exp)  _  (p : 'ipat_tcon)  _  _ 
                        (_loc : Locf.t)  ->
                        let e = Some e in
                        (match e with
                         | Some e ->
                             `OptLablExpr (_loc, (`Lid (_loc, "")), p, e)
                         | None  -> `OptLabl (_loc, (`Lid (_loc, "")), p) : 
                          'ipat ) : Tokenf.txt ->
                                      'exp ->
                                        Tokenf.txt ->
                                          'ipat_tcon ->
                                            Tokenf.txt ->
                                              Tokenf.txt -> Locf.t -> 'ipat ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (sem_pat_for_list : 'sem_pat_for_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`App (_loc, (`Uid (_loc, \"::\")), (`Par (_loc, (`Com (_loc, p, pl)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (pl : 'sem_pat_for_list)  _  (p : 'pat) 
                         (_loc : Locf.t)  ->
                         (`App
                            (_loc, (`Uid (_loc, "::")),
                              (`Par (_loc, (`Com (_loc, p, pl))))) : 
                         'sem_pat_for_list ) : 'sem_pat_for_list ->
                                                 Tokenf.txt ->
                                                   'pat ->
                                                     Locf.t ->
                                                       'sem_pat_for_list ))
               };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot =
                  "`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (_loc : Locf.t)  ->
                        (`App
                           (_loc, (`Uid (_loc, "::")),
                             (`Par
                                (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                        'sem_pat_for_list ) : 'pat ->
                                                Locf.t -> 'sem_pat_for_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "`App\n  (_loc, (`Uid (_loc, \"::\")),\n    (`Par (_loc, (`Com (_loc, p, (`Uid (_loc, \"[]\")))))))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'pat)  (_loc : Locf.t)  ->
                        (`App
                           (_loc, (`Uid (_loc, "::")),
                             (`Par
                                (_loc, (`Com (_loc, p, (`Uid (_loc, "[]"))))))) : 
                        'sem_pat_for_list ) : Tokenf.txt ->
                                                'pat ->
                                                  Locf.t -> 'sem_pat_for_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (pat_tcon : 'pat_tcon Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`Constraint (_loc, p, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (p : 'pat)  (_loc : Locf.t)  ->
                         (`Constraint (_loc, p, t) : 'pat_tcon ) : 'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'pat ->
                                                                    Locf.t ->
                                                                    'pat_tcon ))
               };
              {
                symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot = "p\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  (_loc : Locf.t)  -> (p : 'pat_tcon ) : 
                     'pat -> Locf.t -> 'pat_tcon ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ipat_tcon : 'ipat_tcon Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s : 
                           'ipat_tcon ) : Tokenf.ant -> Locf.t -> 'ipat_tcon ))
               };
              {
                symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(i : alident  :>pat)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                        ((i : alident  :>pat) : 'ipat_tcon ) : 'a_lident ->
                                                                 Locf.t ->
                                                                   'ipat_tcon ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot =
                  "(`Constraint (_loc, (i : alident  :>pat), t) : pat )\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (i : 'a_lident)  (_loc : Locf.t) 
                        ->
                        ((`Constraint (_loc, (i : alident  :>pat), t) : 
                        pat ) : 'ipat_tcon ) : 'ctyp ->
                                                 Tokenf.txt ->
                                                   'a_lident ->
                                                     Locf.t -> 'ipat_tcon ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_pat_list : 'label_pat_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, p1, p2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p2 : 'label_pat_list)  _  (p1 : 'label_pat) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, p1, p2) : 'label_pat_list ) : 'label_pat_list
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'label_pat
                                                                    ->
                                                                    Locf.t ->
                                                                    'label_pat_list ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "_"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Sem (_loc, p1, (`Any _loc))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                        (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list ) : 
                     Tokenf.txt ->
                       Tokenf.txt -> 'label_pat -> Locf.t -> 'label_pat_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "_"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Sem (_loc, p1, (`Any _loc))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                        (`Sem (_loc, p1, (`Any _loc)) : 'label_pat_list ) : 
                     Tokenf.txt ->
                       Tokenf.txt ->
                         Tokenf.txt ->
                           'label_pat -> Locf.t -> 'label_pat_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ))];
                annot = "p1\n";
                fn =
                  (Gramf.mk_action
                     (fun (p1 : 'label_pat)  (_loc : Locf.t)  ->
                        (p1 : 'label_pat_list ) : 'label_pat ->
                                                    Locf.t -> 'label_pat_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (label_pat : 'label_pat Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "p1\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p1 : 'label_pat)  (_loc : Locf.t)  ->
                        (p1 : 'label_pat_list ) : Tokenf.txt ->
                                                    'label_pat ->
                                                      Locf.t ->
                                                        'label_pat_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_exp : 'label_exp Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "rec_exp");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s : 
                           'label_exp ) : Tokenf.ant -> Locf.t -> 'label_exp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s : 
                          'label_exp ) : Tokenf.ant -> Locf.t -> 'label_exp ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ));
                  Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))];
                annot =
                  "(`RecBind (_loc, (i :>Astf.vid), (e :>Astf.exp)) :>Astf.rec_exp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (e : 'fun_bind)  (i : 'label_longident) 
                        (_loc : Locf.t)  ->
                        ((`RecBind (_loc, (i :>Astf.vid), (e :>Astf.exp)) :>
                        Astf.rec_exp) : 'label_exp ) : 'fun_bind ->
                                                         'label_longident ->
                                                           Locf.t ->
                                                             'label_exp ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ))];
                annot =
                  "`RecBind (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                        (`RecBind
                           (_loc, i, (`Lid (_loc, (Fan_ops.to_lid i)))) : 
                        'label_exp ) : 'label_longident ->
                                         Locf.t -> 'label_exp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_ep : 'label_ep Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "rec_exp");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_bind) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_bind) s : 
                           'label_ep ) : Tokenf.ant -> Locf.t -> 'label_ep ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_bind) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_bind) s : 
                          'label_ep ) : Tokenf.ant -> Locf.t -> 'label_ep ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ))];
                annot =
                  "let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                        let p = None in
                        (let p =
                           match p with
                           | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                           | Some p -> p in
                         `RecBind (_loc, i, p) : 'label_ep ) : 'label_longident
                                                                 ->
                                                                 Locf.t ->
                                                                   'label_ep ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ep : 'ep Gramf.t ))];
                annot =
                  "let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'ep)  _  (i : 'label_longident) 
                        (_loc : Locf.t)  ->
                        let p = Some p in
                        (let p =
                           match p with
                           | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                           | Some p -> p in
                         `RecBind (_loc, i, p) : 'label_ep ) : 'ep ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'label_longident
                                                                    ->
                                                                    Locf.t ->
                                                                    'label_ep ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_pat : 'label_pat Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_pat) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_pat) s : 
                           'label_pat ) : Tokenf.ant -> Locf.t -> 'label_pat ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "pat"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_pat) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_pat) s : 
                          'label_pat ) : Tokenf.ant -> Locf.t -> 'label_pat ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ))];
                annot =
                  "let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'label_longident)  (_loc : Locf.t)  ->
                        let p = None in
                        (let p =
                           match p with
                           | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                           | Some p -> p in
                         `RecBind (_loc, i, p) : 'label_pat ) : 'label_longident
                                                                  ->
                                                                  Locf.t ->
                                                                    'label_pat ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
                annot =
                  "let p = match p with | None  -> `Lid (_loc, (Fan_ops.to_lid i)) | Some p -> p in\n`RecBind (_loc, i, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun (p : 'pat)  _  (i : 'label_longident) 
                        (_loc : Locf.t)  ->
                        let p = Some p in
                        (let p =
                           match p with
                           | None  -> `Lid (_loc, (Fan_ops.to_lid i))
                           | Some p -> p in
                         `RecBind (_loc, i, p) : 'label_pat ) : 'pat ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'label_longident
                                                                    ->
                                                                    Locf.t ->
                                                                    'label_pat ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let () =
  make_semi field_exp field_exp_list;
  make_semi exp sem_exp;
  make_semi label_exp label_exp_list;
  make_semi label_ep label_ep_list;
  make_semi pat sem_pat;
  make_semi ep sem_ep;
  make_semi clfield clfield_quot;
  make_semi clsigi clsigi_quot;
  make_comma pat comma_pat;
  make_comma ep comma_ep;
  make_comma ipat comma_ipat;
  make_comma exp comma_exp;
  make_case exp pat;
  make_pat exp;
  make_ants ~c:"pat" ~i:50 pat
    ["";
    "pat";
    "par";
    "int";
    "int32";
    "int64";
    "vrn";
    "flo";
    "chr";
    "nativeint";
    "str";
    "int'";
    "int32'";
    "int64'";
    "nativeint'";
    "flo'";
    "chr'";
    "str'"];
  make_ants ~c:(Dyn_tag.to_string Dyn_tag.ep) ~i:50 ep
    ["";
    "pat";
    "par";
    "int";
    "int32";
    "int64";
    "vrn";
    "flo";
    "chr";
    "nativeint";
    "str";
    "int'";
    "int32'";
    "int64'";
    "nativeint'";
    "flo'";
    "chr'";
    "str'";
    "bool'"];
  make_quot Dyn_tag.exp ~i:170 exp;
  make_ants ~c:(Dyn_tag.to_string Dyn_tag.exp) ~i:170 exp
    ["exp";
    "";
    "par";
    "seq";
    "chr";
    "int";
    "int32";
    "str";
    "int64";
    "flo";
    "nativeint";
    "vrn";
    "chr'";
    "int64'";
    "nativeint'";
    "bool'";
    "int'";
    "int32'";
    "flo'";
    "str'"];
  make_ants ~c:"mexp" ~i:30 mexp [""; "mexp"];
  make_quot Dyn_tag.mexp ~i:30 mexp;
  make_quot Dyn_tag.mbind mbind;
  make_ants ~c:"mexp" mbind ["mbind"; ""];
  make_quot Dyn_tag.mbind module_rec_declaration;
  make_ants ~c:"mbind" module_rec_declaration ["mbind"; ""];
  make_quot Dyn_tag.constr constr;
  make_ants ~c:"constr" constr [""; "constr"];
  make_quot ~i:60 Dyn_tag.mtyp mtyp;
  make_ants ~c:"mtyp" ~i:60 mtyp [""; "mtyp"];
  make_quot Dyn_tag.sigi sigi;
  make_ants ~c:"sigi" sigi [""; "sigi"];
  make_quot Dyn_tag.stru stru;
  make_ants ~c:"stru" stru [""; "stri"];
  make_quot Dyn_tag.clsigi clsigi;
  make_ants ~c:"clsigi" clsigi [""; "csg"];
  make_quot Dyn_tag.clfield clfield;
  make_ants ~c:"clfield" clfield [""; "cst"];
  make_quot Dyn_tag.clexp ~i:30 clexp;
  make_ants ~c:"clexp" ~i:30 clexp [""; "cexp"];
  make_quot Dyn_tag.cltyp cltyp;
  make_ants ~c:"cltyp" cltyp [""; "ctyp"];
  make_ants ~c:"ctyp" meth_decl [""; "typ"]
let apply () =
  (Gramf.extend_single
     ({
        entry = (mexp_quot : 'mexp_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'mexp)  (_loc : Locf.t)  ->
                          (x : 'mexp_quot ) : 'mexp -> Locf.t -> 'mexp_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mbind0 : 'mbind0 Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "("); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`Functor (_loc, m, mt, mb)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mb : 'mbind0)  _  (mt : 'mtyp)  _ 
                          (m : 'a_uident)  _  (_loc : Locf.t)  ->
                          (`Functor (_loc, m, mt, mb) : 'mbind0 ) : 'mbind0
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'mtyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mbind0 ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                 annot = "`Constraint (_loc, me, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (_loc : Locf.t) 
                         -> (`Constraint (_loc, me, mt) : 'mbind0 ) : 
                      'mexp ->
                        Tokenf.txt ->
                          'mtyp -> Tokenf.txt -> Locf.t -> 'mbind0 ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "="); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                 annot = "me\n";
                 fn =
                   (Gramf.mk_action
                      (fun (me : 'mexp)  _  (_loc : Locf.t)  ->
                         (me : 'mbind0 ) : 'mexp ->
                                             Tokenf.txt -> Locf.t -> 'mbind0 ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mexp : 'mexp Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "functor");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "->"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`Functor (_loc, i, t, me)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (me : 'mexp)  _  _  (t : 'mtyp)  _ 
                          (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                          (`Functor (_loc, i, t, me) : 'mexp ) : 'mexp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'mtyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mexp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "struct");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match st with | Some st -> `Struct (_loc, st) | None  -> `StructEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         let st = None in
                         (match st with
                          | Some st -> `Struct (_loc, st)
                          | None  -> `StructEnd _loc : 'mexp ) : Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "struct");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (strus : 'strus Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match st with | Some st -> `Struct (_loc, st) | None  -> `StructEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (st : 'strus)  _  (_loc : Locf.t)  ->
                         let st = Some st in
                         (match st with
                          | Some st -> `Struct (_loc, st)
                          | None  -> `StructEnd _loc : 'mexp ) : Tokenf.txt
                                                                   ->
                                                                   'strus ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mexp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mexp : 'mexp Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols = [Self; Self];
                  annot = "`App (_loc, me1, me2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (me2 : 'mexp)  (me1 : 'mexp)  (_loc : Locf.t)  ->
                          (`App (_loc, me1, me2) : 'mexp ) : 'mexp ->
                                                               'mexp ->
                                                                 Locf.t ->
                                                                   'mexp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mexp : 'mexp Gramf.t );
        olevel =
          ({
             label = (Some 30);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj
                          (module_longident : 'module_longident Gramf.t ))];
                  annot = "(i :>mexp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (i : 'module_longident)  (_loc : Locf.t)  ->
                          ((i :>mexp) : 'mexp ) : 'module_longident ->
                                                    Locf.t -> 'mexp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Constraint (_loc, me, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (mt : 'mtyp)  _  (me : 'mexp)  _ 
                         (_loc : Locf.t)  ->
                         (`Constraint (_loc, me, mt) : 'mexp ) : Tokenf.txt
                                                                   ->
                                                                   'mtyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'mexp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "me\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (me : 'mexp)  _  (_loc : Locf.t)  ->
                         (me : 'mexp ) : Tokenf.txt ->
                                           'mexp ->
                                             Tokenf.txt -> Locf.t -> 'mexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "val"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`PackageModule (_loc, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e : 'exp)  _  _  (_loc : Locf.t)  ->
                         (`PackageModule (_loc, e) : 'mexp ) : Tokenf.txt ->
                                                                 'exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "val"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "`PackageModule (_loc, (`Constraint (_loc, e, (`Package (_loc, p)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (p : 'mtyp)  _  (e : 'exp)  _  _ 
                         (_loc : Locf.t)  ->
                         (`PackageModule
                            (_loc,
                              (`Constraint (_loc, e, (`Package (_loc, p))))) : 
                         'mexp ) : Tokenf.txt ->
                                     'mtyp ->
                                       Tokenf.txt ->
                                         'exp ->
                                           Tokenf.txt ->
                                             Tokenf.txt -> Locf.t -> 'mexp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (mbind_quot : 'mbind_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "and"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`And (_loc, b1, b2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (b2 : 'mbind_quot)  _  (b1 : 'mbind_quot) 
                          (_loc : Locf.t)  ->
                          (`And (_loc, b1, b2) : 'mbind_quot ) : 'mbind_quot
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'mbind_quot
                                                                    ->
                                                                    Locf.t ->
                                                                    'mbind_quot ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "mbind");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.mbind) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.mbind) s : 
                           'mbind_quot ) : Tokenf.ant ->
                                             Locf.t -> 'mbind_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.mbind) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.mbind) s : 
                           'mbind_quot ) : Tokenf.ant ->
                                             Locf.t -> 'mbind_quot ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                 annot = "`Constraint (_loc, m, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t) 
                         -> (`Constraint (_loc, m, mt) : 'mbind_quot ) : 
                      'mtyp ->
                        Tokenf.txt -> 'a_uident -> Locf.t -> 'mbind_quot ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                 annot = "`ModuleBind (_loc, m, mt, me)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                         (_loc : Locf.t)  ->
                         (`ModuleBind (_loc, m, mt, me) : 'mbind_quot ) : 
                      'mexp ->
                        Tokenf.txt ->
                          'mtyp ->
                            Tokenf.txt -> 'a_uident -> Locf.t -> 'mbind_quot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mbind : 'mbind Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "and"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`And (_loc, b1, b2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (b2 : 'mbind)  _  (b1 : 'mbind)  (_loc : Locf.t) 
                          -> (`And (_loc, b1, b2) : 'mbind ) : 'mbind ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'mbind ->
                                                                    Locf.t ->
                                                                    'mbind ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                 annot = "`ModuleBind (_loc, m, mt, me)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (me : 'mexp)  _  (mt : 'mtyp)  _  (m : 'a_uident) 
                         (_loc : Locf.t)  ->
                         (`ModuleBind (_loc, m, mt, me) : 'mbind ) : 
                      'mexp ->
                        Tokenf.txt ->
                          'mtyp ->
                            Tokenf.txt -> 'a_uident -> Locf.t -> 'mbind ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (module_rec_declaration : 'module_rec_declaration Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "and"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`And (_loc, m1, m2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (m2 : 'module_rec_declaration)  _ 
                          (m1 : 'module_rec_declaration)  (_loc : Locf.t)  ->
                          (`And (_loc, m1, m2) : 'module_rec_declaration ) : 
                       'module_rec_declaration ->
                         Tokenf.txt ->
                           'module_rec_declaration ->
                             Locf.t -> 'module_rec_declaration ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                 annot = "`Constraint (_loc, m, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'mtyp)  _  (m : 'a_uident)  (_loc : Locf.t) 
                         ->
                         (`Constraint (_loc, m, mt) : 'module_rec_declaration ) : 
                      'mtyp ->
                        Tokenf.txt ->
                          'a_uident -> Locf.t -> 'module_rec_declaration ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (constr_quot : 'constr_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (constr : 'constr Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'constr)  (_loc : Locf.t)  ->
                          (x : 'constr_quot ) : 'constr ->
                                                  Locf.t -> 'constr_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (constr : 'constr Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "and"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`And (_loc, wc1, wc2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (wc2 : 'constr)  _  (wc1 : 'constr) 
                          (_loc : Locf.t)  ->
                          (`And (_loc, wc1, wc2) : 'constr ) : 'constr ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'constr ->
                                                                    Locf.t ->
                                                                    'constr ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_longident_and_parameters : 'type_longident_and_parameters
                                                           Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot =
                   "match p with\n| Some _ -> `TypeEqPriv (_loc, t1, t2)\n| None  -> `TypeEq (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _ 
                         (t1 : 'type_longident_and_parameters)  _ 
                         (_loc : Locf.t)  ->
                         let p = None in
                         (match p with
                          | Some _ -> `TypeEqPriv (_loc, t1, t2)
                          | None  -> `TypeEq (_loc, t1, t2) : 'constr ) : 
                      'ctyp ->
                        Tokenf.txt ->
                          'type_longident_and_parameters ->
                            Tokenf.txt -> Locf.t -> 'constr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_longident_and_parameters : 'type_longident_and_parameters
                                                           Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "private");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot =
                   "match p with\n| Some _ -> `TypeEqPriv (_loc, t1, t2)\n| None  -> `TypeEq (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  (p : Tokenf.txt)  _ 
                         (t1 : 'type_longident_and_parameters)  _ 
                         (_loc : Locf.t)  ->
                         let p = Some p in
                         (match p with
                          | Some _ -> `TypeEqPriv (_loc, t1, t2)
                          | None  -> `TypeEq (_loc, t1, t2) : 'constr ) : 
                      'ctyp ->
                        Tokenf.txt ->
                          Tokenf.txt ->
                            'type_longident_and_parameters ->
                              Tokenf.txt -> Locf.t -> 'constr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_longident_and_parameters : 'type_longident_and_parameters
                                                           Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`TypeSubst (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _ 
                         (t1 : 'type_longident_and_parameters)  _ 
                         (_loc : Locf.t)  ->
                         (`TypeSubst (_loc, t1, t2) : 'constr ) : 'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'type_longident_and_parameters
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'constr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident_with_app : 'module_longident_with_app
                                                       Gramf.t ))];
                 annot =
                   "let i = (i1 : vid  :>ident) in\nif v = \"=\" then `ModuleEq (_loc, i, i2) else `ModuleSubst (_loc, i, i2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i2 : 'module_longident_with_app) 
                         (__fan_2 : Tokenf.txt)  (i1 : 'module_longident)  _ 
                         (_loc : Locf.t)  ->
                         let v = __fan_2.txt in
                         (let i = (i1 : vid  :>ident) in
                          if v = "="
                          then `ModuleEq (_loc, i, i2)
                          else `ModuleSubst (_loc, i, i2) : 'constr ) : 
                      'module_longident_with_app ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt -> Locf.t -> 'constr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident_with_app : 'module_longident_with_app
                                                       Gramf.t ))];
                 annot =
                   "let i = (i1 : vid  :>ident) in\nif v = \"=\" then `ModuleEq (_loc, i, i2) else `ModuleSubst (_loc, i, i2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i2 : 'module_longident_with_app) 
                         (__fan_2 : Tokenf.txt)  (i1 : 'module_longident)  _ 
                         (_loc : Locf.t)  ->
                         let v = __fan_2.txt in
                         (let i = (i1 : vid  :>ident) in
                          if v = "="
                          then `ModuleEq (_loc, i, i2)
                          else `ModuleSubst (_loc, i, i2) : 'constr ) : 
                      'module_longident_with_app ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt -> Locf.t -> 'constr ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (sigis : 'sigis Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s : 
                            'sigis ) : Tokenf.ant -> Locf.t -> 'sigis ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "sigi");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s : 
                           'sigis ) : Tokenf.ant -> Locf.t -> 'sigis ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s), sg)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigis)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s),
                              sg) : 'sigis ) : 'sigis ->
                                                 Tokenf.ant ->
                                                   Locf.t -> 'sigis ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "sigi");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s), sg)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigis)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s),
                              sg) : 'sigis ) : 'sigis ->
                                                 Tokenf.ant ->
                                                   Locf.t -> 'sigis ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s), sg)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s),
                              sg) : 'sigis ) : 'sigis ->
                                                 Tokenf.txt ->
                                                   Tokenf.ant ->
                                                     Locf.t -> 'sigis ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "sigi");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s), sg)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigis)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.sigi) s),
                              sg) : 'sigis ) : 'sigis ->
                                                 Tokenf.txt ->
                                                   Tokenf.ant ->
                                                     Locf.t -> 'sigis ))
               };
               {
                 symbols = [Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); Self];
                 annot = "`Sem (_loc, sg, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (s : 'sigis)  (sg : 'sigi)  (_loc : Locf.t)  ->
                         (`Sem (_loc, sg, s) : 'sigis ) : 'sigis ->
                                                            'sigi ->
                                                              Locf.t ->
                                                                'sigis ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, sg, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (s : 'sigis)  _  (sg : 'sigi)  (_loc : Locf.t)  ->
                         (`Sem (_loc, sg, s) : 'sigis ) : 'sigis ->
                                                            Tokenf.txt ->
                                                              'sigi ->
                                                                Locf.t ->
                                                                  'sigis ))
               };
               {
                 symbols = [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))];
                 annot = "sg\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigi)  (_loc : Locf.t)  -> (sg : 'sigis ) : 
                      'sigi -> Locf.t -> 'sigis ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "sg\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (sg : 'sigi)  (_loc : Locf.t)  ->
                         (sg : 'sigis ) : Tokenf.txt ->
                                            'sigi -> Locf.t -> 'sigis ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "functor");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "->"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`Functor (_loc, i, t, mt)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mt : 'mtyp)  _  _  (t : 'mtyp)  _ 
                          (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                          (`Functor (_loc, i, t, mt) : 'mtyp ) : 'mtyp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'mtyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mtyp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "with"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (constr : 'constr Gramf.t ))];
                  annot = "`With (_loc, mt, wc)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (wc : 'constr)  _  (mt : 'mtyp)  (_loc : Locf.t) 
                          -> (`With (_loc, mt, wc) : 'mtyp ) : 'constr ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'mtyp ->
                                                                    Locf.t ->
                                                                    'mtyp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 30);
             lassoc = true;
             productions =
               [{
                  symbols = [Self; Self];
                  annot =
                    "match (mt1, mt2) with\n| ((#ident as i1),(#ident as i2)) -> apply i1 i2\n| _ -> raise Streamf.NotConsumed\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mt2 : 'mtyp)  (mt1 : 'mtyp)  (_loc : Locf.t)  ->
                          (match (mt1, mt2) with
                           | ((#ident as i1),(#ident as i2)) -> apply i1 i2
                           | _ -> raise Streamf.NotConsumed : 'mtyp ) : 
                       'mtyp -> 'mtyp -> Locf.t -> 'mtyp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 40);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "let acc0 mt1 mt2 =\n  match (mt1, mt2) with\n  | ((#ident as i1),(#ident as i2)) -> dot i1 i2\n  | _ -> raise Streamf.NotConsumed in\nacc0 mt1 mt2\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mt2 : 'mtyp)  _  (mt1 : 'mtyp)  (_loc : Locf.t) 
                          ->
                          (let acc0 mt1 mt2 =
                             match (mt1, mt2) with
                             | ((#ident as i1),(#ident as i2)) -> dot i1 i2
                             | _ -> raise Streamf.NotConsumed in
                           acc0 mt1 mt2 : 'mtyp ) : 'mtyp ->
                                                      Tokenf.txt ->
                                                        'mtyp ->
                                                          Locf.t -> 'mtyp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 50);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "sig"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "end"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot =
                    "match sg with | Some sg -> `Sig (_loc, sg) | None  -> `SigEnd _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  _  (_loc : Locf.t)  ->
                          let sg = None in
                          (match sg with
                           | Some sg -> `Sig (_loc, sg)
                           | None  -> `SigEnd _loc : 'mtyp ) : Tokenf.txt ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'mtyp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "sig"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (sigis : 'sigis Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match sg with | Some sg -> `Sig (_loc, sg) | None  -> `SigEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (sg : 'sigis)  _  (_loc : Locf.t)  ->
                         let sg = Some sg in
                         (match sg with
                          | Some sg -> `Sig (_loc, sg)
                          | None  -> `SigEnd _loc : 'mtyp ) : Tokenf.txt ->
                                                                'sigis ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mtyp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp : 'mtyp Gramf.t );
        olevel =
          ({
             label = (Some 60);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj
                          (module_longident_with_app : 'module_longident_with_app
                                                         Gramf.t ))];
                  annot = "(i : ident  :>mtyp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (i : 'module_longident_with_app)  (_loc : Locf.t)
                           -> ((i : ident  :>mtyp) : 'mtyp ) : 'module_longident_with_app
                                                                 ->
                                                                 Locf.t ->
                                                                   'mtyp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mt\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                         (mt : 'mtyp ) : Tokenf.txt ->
                                           'mtyp ->
                                             Tokenf.txt -> Locf.t -> 'mtyp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "of"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                 annot = "`ModuleTypeOf (_loc, me)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (me : 'mexp)  _  _  _  (_loc : Locf.t)  ->
                         (`ModuleTypeOf (_loc, me) : 'mtyp ) : 'mexp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'mtyp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (module_declaration : 'module_declaration Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A ":"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                  annot = "mt\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                          (mt : 'module_declaration ) : 'mtyp ->
                                                          Tokenf.txt ->
                                                            Locf.t ->
                                                              'module_declaration ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Functor (_loc, i, t, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'module_declaration)  _  (t : 'mtyp)  _ 
                         (i : 'a_uident)  _  (_loc : Locf.t)  ->
                         (`Functor (_loc, i, t, mt) : 'module_declaration ) : 
                      'module_declaration ->
                        Tokenf.txt ->
                          'mtyp ->
                            Tokenf.txt ->
                              'a_uident ->
                                Tokenf.txt -> Locf.t -> 'module_declaration ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mtyp_quot : 'mtyp_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'mtyp)  (_loc : Locf.t)  ->
                          (x : 'mtyp_quot ) : 'mtyp -> Locf.t -> 'mtyp_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  ();
  (Gramf.extend_single
     ({
        entry = (sigi_quot : 'sigi_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "#"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                  annot = "`DirectiveSimple (_loc, s)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (s : 'a_lident)  _  (_loc : Locf.t)  ->
                          (`DirectiveSimple (_loc, s) : 'sigi_quot ) : 
                       'a_lident -> Tokenf.txt -> Locf.t -> 'sigi_quot ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "#"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "`Directive (_loc, s, dp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (dp : 'exp)  (s : 'a_lident)  _  (_loc : Locf.t) 
                         -> (`Directive (_loc, s, dp) : 'sigi_quot ) : 
                      'exp -> 'a_lident -> Tokenf.txt -> Locf.t -> 'sigi_quot ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, sg1, sg2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg2 : 'sigi_quot)  _  (sg1 : 'sigi) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, sg1, sg2) : 'sigi_quot ) : 'sigi_quot
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'sigi ->
                                                                    Locf.t ->
                                                                    'sigi_quot ))
               };
               {
                 symbols = [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ))];
                 annot = "sg\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sg : 'sigi)  (_loc : Locf.t)  ->
                         (sg : 'sigi_quot ) : 'sigi -> Locf.t -> 'sigi_quot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (sigi : 'sigi Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "include");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                  annot = "`Include (_loc, mt)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (mt : 'mtyp)  _  (_loc : Locf.t)  ->
                          (`Include (_loc, mt) : 'sigi ) : 'mtyp ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'sigi ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Nterm
                     (Gramf.obj
                        (module_declaration : 'module_declaration Gramf.t ))];
                 annot = "`Module (_loc, i, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'module_declaration)  (i : 'a_uident)  _ 
                         (_loc : Locf.t)  -> (`Module (_loc, i, mt) : 
                         'sigi ) : 'module_declaration ->
                                     'a_uident ->
                                       Tokenf.txt -> Locf.t -> 'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "rec"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_rec_declaration : 'module_rec_declaration
                                                    Gramf.t ))];
                 annot = "`RecModule (_loc, mb)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mb : 'module_rec_declaration)  _  _ 
                         (_loc : Locf.t)  -> (`RecModule (_loc, mb) : 
                         'sigi ) : 'module_rec_declaration ->
                                     Tokenf.txt ->
                                       Tokenf.txt -> Locf.t -> 'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))];
                 annot = "`ModuleTypeEnd (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_uident)  _  _  (_loc : Locf.t)  ->
                         (`ModuleTypeEnd (_loc, i) : 'sigi ) : 'a_uident ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "open"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ))];
                 annot =
                   "`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                         let bang = None in
                         (`Open
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident)) : 'sigi ) : 'module_longident
                                                                ->
                                                                Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "open"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ))];
                 annot =
                   "`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  (bang : Tokenf.txt)  _ 
                         (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (`Open
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident)) : 'sigi ) : 'module_longident
                                                                ->
                                                                Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_declaration : 'type_declaration Gramf.t ))];
                 annot = "`Type (_loc, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                         (`Type (_loc, t) : 'sigi ) : 'type_declaration ->
                                                        Tokenf.txt ->
                                                          Locf.t -> 'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                 annot = "`ModuleType (_loc, i, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _ 
                         (_loc : Locf.t)  ->
                         (`ModuleType (_loc, i, mt) : 'sigi ) : 'mtyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "class"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (cltyp_declaration : 'cltyp_declaration Gramf.t ))];
                 annot = "`ClassType (_loc, ctd)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t) 
                         -> (`ClassType (_loc, ctd) : 'sigi ) : 'cltyp_declaration
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "exception");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (constructor_declaration : 'constructor_declaration
                                                     Gramf.t ))];
                 annot = "`Exception (_loc, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)
                          -> (`Exception (_loc, t) : 'sigi ) : 'constructor_declaration
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "external");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))];
                 annot = "`External (_loc, i, t, sl)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sl : 'string_list)  _  (t : 'ctyp)  _ 
                         (i : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`External (_loc, i, t, sl) : 'sigi ) : 'string_list
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_lident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`Val (_loc, i, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (i : 'a_lident)  _ 
                         (_loc : Locf.t)  -> (`Val (_loc, i, t) : 'sigi ) : 
                      'ctyp ->
                        Tokenf.txt ->
                          'a_lident -> Tokenf.txt -> Locf.t -> 'sigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "class"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (class_description : 'class_description Gramf.t ))];
                 annot = "`Class (_loc, cd)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (cd : 'class_description)  _  (_loc : Locf.t)  ->
                         (`Class (_loc, cd) : 'sigi ) : 'class_description ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'sigi ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (interf : 'interf Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (sigi : 'sigi Gramf.t )); Self];
                  annot =
                    "let (sil,stopped) = rest in ((si :: sil), stopped)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (rest : 'interf)  (si : 'sigi)  (_loc : Locf.t) 
                          ->
                          (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                          'interf ) : 'interf -> 'sigi -> Locf.t -> 'interf ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (sigi : 'sigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "let (sil,stopped) = rest in ((si :: sil), stopped)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'interf)  _  (si : 'sigi)  (_loc : Locf.t)
                          ->
                         (let (sil,stopped) = rest in ((si :: sil), stopped) : 
                         'interf ) : 'interf ->
                                       Tokenf.txt ->
                                         'sigi -> Locf.t -> 'interf ))
               };
               {
                 symbols = [];
                 annot = "([], None)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  -> (([], None) : 'interf ) : 
                      Locf.t -> 'interf ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (let name_space: 'name_space Gramf.t = Gramf.mk "name_space"
   and fun_def_pat: 'fun_def_pat Gramf.t = Gramf.mk "fun_def_pat" in
   Gramf.extend_single
     ({
        entry = (exp_quot : 'exp_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ","); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ))];
                  annot = "`Com (_loc, e1, e2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e2 : 'comma_exp)  _  (e1 : 'exp) 
                          (_loc : Locf.t)  ->
                          (`Com (_loc, e1, e2) : 'exp_quot ) : 'comma_exp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'exp ->
                                                                    Locf.t ->
                                                                    'exp_quot ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ))];
                 annot = "`Sem (_loc, e1, e2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e2 : 'sem_exp)  _  (e1 : 'exp)  (_loc : Locf.t) 
                         -> (`Sem (_loc, e1, e2) : 'exp_quot ) : 'sem_exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Locf.t ->
                                                                    'exp_quot ))
               };
               {
                 symbols = [Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (_loc : Locf.t)  -> (e : 'exp_quot ) : 
                      'exp -> Locf.t -> 'exp_quot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (cvalue_bind : 'cvalue_bind Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "="); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                  annot = "e\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                          (e : 'cvalue_bind ) : 'exp ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'cvalue_bind ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (unquoted_typevars : 'unquoted_typevars Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "let u = (`TyPol (_loc, (t1 :>Astf.ctyp), (t2 :>Astf.ctyp)) :>Astf.ctyp) in\n(`Constraint (_loc, (e :>Astf.exp), (u :>Astf.ctyp)) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (t2 : 'ctyp)  _ 
                         (t1 : 'unquoted_typevars)  _  _  (_loc : Locf.t)  ->
                         (let u =
                            (`TyPol
                               (_loc, (t1 :>Astf.ctyp), (t2 :>Astf.ctyp)) :>
                            Astf.ctyp) in
                          (`Constraint
                             (_loc, (e :>Astf.exp), (u :>Astf.ctyp)) :>
                            Astf.exp) : 'cvalue_bind ) : 'exp ->
                                                           Tokenf.txt ->
                                                             'ctyp ->
                                                               Tokenf.txt ->
                                                                 'unquoted_typevars
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'cvalue_bind ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(`Constraint (_loc, (e :>Astf.exp), (t :>Astf.ctyp)) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                         ((`Constraint
                             (_loc, (e :>Astf.exp), (t :>Astf.ctyp)) :>
                         Astf.exp) : 'cvalue_bind ) : 'exp ->
                                                        Tokenf.txt ->
                                                          'ctyp ->
                                                            Tokenf.txt ->
                                                              Locf.t ->
                                                                'cvalue_bind ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":>"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "match t with\n| (`TyPol (_loc,_,_) : Astf.ctyp) ->\n    raise (Streamf.Error \"unexpected polytype here\")\n| _ ->\n    (`Coercion (_loc, (e :>Astf.exp), (t :>Astf.ctyp), (t2 :>Astf.ctyp)) :>\n    Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (t2 : 'ctyp)  _  (t : 'ctyp)  _ 
                         (_loc : Locf.t)  ->
                         (match t with
                          | (`TyPol (_loc,_,_) : Astf.ctyp) ->
                              raise
                                (Streamf.Error "unexpected polytype here")
                          | _ ->
                              (`Coercion
                                 (_loc, (e :>Astf.exp), (t :>Astf.ctyp),
                                   (t2 :>Astf.ctyp)) :>Astf.exp) : 'cvalue_bind ) : 
                      'exp ->
                        Tokenf.txt ->
                          'ctyp ->
                            Tokenf.txt ->
                              'ctyp -> Tokenf.txt -> Locf.t -> 'cvalue_bind ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":>"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "`Subtype (_loc, e, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (t : 'ctyp)  _  (_loc : Locf.t)  ->
                         (`Subtype (_loc, e, t) : 'cvalue_bind ) : 'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'cvalue_bind ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (fun_bind : 'fun_bind Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "("); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`LocalTypeFun (_loc, i, e)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'fun_bind)  _  (i : 'a_lident)  _  _ 
                          (_loc : Locf.t)  ->
                          (`LocalTypeFun (_loc, i, e) : 'fun_bind ) : 
                       'fun_bind ->
                         Tokenf.txt ->
                           'a_lident ->
                             Tokenf.txt -> Tokenf.txt -> Locf.t -> 'fun_bind ))
                };
               {
                 symbols = [Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); Self];
                 annot = "`Fun (_loc, (`Case (_loc, p, e)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_bind)  (p : 'ipat)  (_loc : Locf.t)  ->
                         (`Fun (_loc, (`Case (_loc, p, e))) : 'fun_bind ) : 
                      'fun_bind -> 'ipat -> Locf.t -> 'fun_bind ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))];
                 annot = "bi\n";
                 fn =
                   (Gramf.mk_action
                      (fun (bi : 'cvalue_bind)  (_loc : Locf.t)  ->
                         (bi : 'fun_bind ) : 'cvalue_bind ->
                                               Locf.t -> 'fun_bind ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (lang : 'lang Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))];
                  annot =
                    "let old = !Ast_quotation.default in\nmatch Ast_quotation.resolve_name ls with\n| Some x -> (Ast_quotation.default := (Some x); old)\n| None  ->\n    Locf.failf _loc \"DDSL `%s' can not be resolved\"\n      (Tokenf.string_of_name ls)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (ls : 'dot_lstrings)  (_loc : Locf.t)  ->
                          (let old = !Ast_quotation.default in
                           match Ast_quotation.resolve_name ls with
                           | Some x ->
                               (Ast_quotation.default := (Some x); old)
                           | None  ->
                               Locf.failf _loc
                                 "DDSL `%s' can not be resolved"
                                 (Tokenf.string_of_name ls) : 'lang ) : 
                       'dot_lstrings -> Locf.t -> 'lang ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (pos_exps : 'pos_exps Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [List1sep
                       ((Nterm
                           (Gramf.obj (name_space : 'name_space Gramf.t ))),
                         (Token
                            ({
                               descr =
                                 {
                                   tag = `Key;
                                   word = (A ";");
                                   tag_name = "Key"
                                 }
                             } : Tokenf.pattern )))];
                  annot =
                    "let old = !Ast_quotation.map in\nAst_quotation.map := (Mapf.String.add_list xys old); old\n";
                  fn =
                    (Gramf.mk_action
                       (fun (xys : 'name_space list)  (_loc : Locf.t)  ->
                          (let old = !Ast_quotation.map in
                           Ast_quotation.map :=
                             (Mapf.String.add_list xys old);
                           old : 'pos_exps ) : 'name_space list ->
                                                 Locf.t -> 'pos_exps ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (name_space : 'name_space Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Lid; word = Any; tag_name = "Lid" }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (dot_lstrings : 'dot_lstrings Gramf.t ))];
                  annot =
                    "(x,\n  (match Ast_quotation.resolve_name y with\n   | None  ->\n       Locf.failf _loc \"DDSL `%s' can not be resolved\"\n         (Tokenf.string_of_name y)\n   | Some x -> x))\n";
                  fn =
                    (Gramf.mk_action
                       (fun (y : 'dot_lstrings)  _  (__fan_0 : Tokenf.txt) 
                          (_loc : Locf.t)  ->
                          let x = __fan_0.txt in
                          ((x,
                             (match Ast_quotation.resolve_name y with
                              | None  ->
                                  Locf.failf _loc
                                    "DDSL `%s' can not be resolved"
                                    (Tokenf.string_of_name y)
                              | Some x -> x)) : 'name_space ) : 'dot_lstrings
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'name_space ))
                };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot =
                   "(x,\n  (match Ast_quotation.resolve_name { domain = (`Sub []); name = x } with\n   | None  -> Locf.failf _loc \"DDSL `%s' can not be resolved\" x\n   | Some x -> x))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         ((x,
                            (match Ast_quotation.resolve_name
                                     { domain = (`Sub []); name = x }
                             with
                             | None  ->
                                 Locf.failf _loc
                                   "DDSL `%s' can not be resolved" x
                             | Some x -> x)) : 'name_space ) : Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'name_space ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (fun_def_pat : 'fun_def_pat Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "("); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "fun e  -> `LocalTypeFun (_loc, i, e)\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (i : 'a_lident)  _  _  (_loc : Locf.t)  ->
                          (fun e  -> `LocalTypeFun (_loc, i, e) : 'fun_def_pat ) : 
                       Tokenf.txt ->
                         'a_lident ->
                           Tokenf.txt -> Tokenf.txt -> Locf.t -> 'fun_def_pat ))
                };
               {
                 symbols = [Nterm (Gramf.obj (ipat : 'ipat Gramf.t ))];
                 annot = "fun e  -> `Fun (_loc, (`Case (_loc, p, e)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (p : 'ipat)  (_loc : Locf.t)  ->
                         (fun e  -> `Fun (_loc, (`Case (_loc, p, e))) : 
                         'fun_def_pat ) : 'ipat -> Locf.t -> 'fun_def_pat ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "when"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (w : 'exp)  _  (p : 'ipat)  (_loc : Locf.t)  ->
                         (fun e  -> `Fun (_loc, (`CaseWhen (_loc, p, w, e))) : 
                         'fun_def_pat ) : 'exp ->
                                            Tokenf.txt ->
                                              'ipat -> Locf.t -> 'fun_def_pat ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (fun_def : 'fun_def Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "->"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                  annot = "f e\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  _  (f : 'fun_def_pat) 
                          (_loc : Locf.t)  -> (f e : 'fun_def ) : 'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'fun_def_pat
                                                                    ->
                                                                    Locf.t ->
                                                                    'fun_def ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (fun_def_pat : 'fun_def_pat Gramf.t ));
                   Self];
                 annot = "f e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_def)  (f : 'fun_def_pat) 
                         (_loc : Locf.t)  -> (f e : 'fun_def ) : 'fun_def ->
                                                                   'fun_def_pat
                                                                    ->
                                                                    Locf.t ->
                                                                    'fun_def ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "let"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                    Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "in"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`LetIn (_loc, r, bi, x)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                          (_loc : Locf.t)  ->
                          (`LetIn (_loc, r, bi, x) : 'exp ) : 'exp ->
                                                                Tokenf.txt ->
                                                                  'bind ->
                                                                    'opt_rec
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "module"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`LetModule (_loc, m, mb, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _ 
                         _  (_loc : Locf.t)  ->
                         (`LetModule (_loc, m, mb, e) : 'exp ) : 'exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'mbind0
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                         (_loc : Locf.t)  ->
                         let bang = None in
                         (`LetOpen
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident), e) : 'exp ) : 'exp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'module_longident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (i : 'module_longident) 
                         (bang : Tokenf.txt)  _  _  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (`LetOpen
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident), e) : 'exp ) : 'exp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'module_longident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "try"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (case : 'case Gramf.t ))];
                 annot = "`LetTryInWith (_loc, r, bi, x, a)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                         (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                         (`LetTryInWith (_loc, r, bi, x, a) : 'exp ) : 
                      'case ->
                        Tokenf.txt ->
                          'exp ->
                            Tokenf.txt ->
                              'bind ->
                                'opt_rec ->
                                  Tokenf.txt -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "match"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (case : 'case Gramf.t ))];
                 annot = "`Match (_loc, e, a)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                         (`Match (_loc, e, a) : 'exp ) : 'case ->
                                                           Tokenf.txt ->
                                                             'exp ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "try"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (case : 'case Gramf.t ))];
                 annot = "`Try (_loc, e, a)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (a : 'case)  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                         (`Try (_loc, e, a) : 'exp ) : 'case ->
                                                         Tokenf.txt ->
                                                           'exp ->
                                                             Tokenf.txt ->
                                                               Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "if"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "then"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "else"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`IfThenElse (_loc, e1, e2, e3)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e3 : 'exp)  _  (e2 : 'exp)  _  (e1 : 'exp)  _ 
                         (_loc : Locf.t)  ->
                         (`IfThenElse (_loc, e1, e2, e3) : 'exp ) : 'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "if"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "then"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`IfThen (_loc, e1, e2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e2 : 'exp)  _  (e1 : 'exp)  _  (_loc : Locf.t) 
                         -> (`IfThen (_loc, e1, e2) : 'exp ) : 'exp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "do"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "done"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Seq (_loc, seq)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                         (`Seq (_loc, seq) : 'exp ) : Tokenf.txt ->
                                                        'sequence ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "with"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (lang : 'lang Gramf.t ));
                   Self];
                 annot = "Ast_quotation.default := old; x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'exp)  (old : 'lang)  _  (_loc : Locf.t)  ->
                         (Ast_quotation.default := old; x : 'exp ) : 
                      'exp -> 'lang -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "with"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "{"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pos_exps : 'pos_exps Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "Ast_quotation.map := old; x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'exp)  _  (old : 'pos_exps)  _  _ 
                         (_loc : Locf.t)  ->
                         (Ast_quotation.map := old; x : 'exp ) : 'exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'pos_exps
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "for"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Nterm (Gramf.obj (flag : 'flag Gramf.t ));
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "do"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "done"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`For (_loc, i, e1, e2, df, seq)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (seq : 'sequence)  _  (e2 : 'exp)  (df : 'flag)
                          (e1 : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)
                          -> (`For (_loc, i, e1, e2, df, seq) : 'exp ) : 
                      Tokenf.txt ->
                        'sequence ->
                          Tokenf.txt ->
                            'exp ->
                              'flag ->
                                'exp ->
                                  Tokenf.txt ->
                                    'a_lident -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "while"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "do"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "done"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`While (_loc, e, seq)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (seq : 'sequence)  _  (e : 'exp)  _ 
                         (_loc : Locf.t)  -> (`While (_loc, e, seq) : 
                         'exp ) : Tokenf.txt ->
                                    'sequence ->
                                      Tokenf.txt ->
                                        'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 70);
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "::"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "let op = (`Uid (xloc, op) :>Astf.exp) in\n(`App\n   (_loc, (`App (_loc, (op :>Astf.exp), (e1 :>Astf.exp))), (e2 :>Astf.exp)) :>\n  Astf.exp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e2 : 'exp)  (__fan_1 : Tokenf.txt)  (e1 : 'exp) 
                          (_loc : Locf.t)  ->
                          let xloc = __fan_1.loc in
                          let op = __fan_1.txt in
                          (let op = (`Uid (xloc, op) :>Astf.exp) in
                           (`App
                              (_loc,
                                (`App
                                   (_loc, (op :>Astf.exp), (e1 :>Astf.exp))),
                                (e2 :>Astf.exp)) :>Astf.exp) : 'exp ) : 
                       'exp -> Tokenf.txt -> 'exp -> Locf.t -> 'exp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 110);
             lassoc = false;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "fun"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "|"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    List1sep
                      ((Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                        (Token
                           ({
                              descr =
                                {
                                  tag = `Key;
                                  word = (A "|");
                                  tag_name = "Key"
                                }
                            } : Tokenf.pattern )))];
                  annot = "let cases = bar_of_list a in `Fun (_loc, cases)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                          (let cases = bar_of_list a in `Fun (_loc, cases) : 
                          'exp ) : 'case0 list ->
                                     Tokenf.txt ->
                                       Tokenf.txt -> Locf.t -> 'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "function");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   List1sep
                     ((Nterm (Gramf.obj (case0 : 'case0 Gramf.t ))),
                       (Token
                          ({
                             descr =
                               { tag = `Key; word = (A "|"); tag_name = "Key"
                               }
                           } : Tokenf.pattern )))];
                 annot = "let cases = bar_of_list a in `Fun (_loc, cases)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (a : 'case0 list)  _  _  (_loc : Locf.t)  ->
                         (let cases = bar_of_list a in `Fun (_loc, cases) : 
                         'exp ) : 'case0 list ->
                                    Tokenf.txt ->
                                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "fun"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))];
                 annot = "e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp ) : 
                      'fun_def -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "function");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (fun_def : 'fun_def Gramf.t ))];
                 annot = "e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_def)  _  (_loc : Locf.t)  -> (e : 'exp ) : 
                      'fun_def -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                         let cst = None in
                         (match cst with
                          | Some cst -> `ObjPat (_loc, p, cst)
                          | None  -> `ObjPatEnd (_loc, p) : 'exp ) : 
                      Tokenf.txt ->
                        Tokenf.txt ->
                          'pat -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with\n| Some cst -> `ObjPat (_loc, p, cst)\n| None  -> `ObjPatEnd (_loc, p)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                         (_loc : Locf.t)  ->
                         let cst = Some cst in
                         (match cst with
                          | Some cst -> `ObjPat (_loc, p, cst)
                          | None  -> `ObjPatEnd (_loc, p) : 'exp ) : 
                      Tokenf.txt ->
                        'class_structure ->
                          Tokenf.txt ->
                            'pat ->
                              Tokenf.txt -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with\n| Some cst -> `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n| None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _ 
                         (_loc : Locf.t)  ->
                         let cst = None in
                         (match cst with
                          | Some cst ->
                              `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)
                          | None  ->
                              `ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                           'exp ) : Tokenf.txt ->
                                      Tokenf.txt ->
                                        'ctyp ->
                                          Tokenf.txt ->
                                            'pat ->
                                              Tokenf.txt ->
                                                Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with\n| Some cst -> `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n| None  -> `ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                         (p : 'pat)  _  _  (_loc : Locf.t)  ->
                         let cst = Some cst in
                         (match cst with
                          | Some cst ->
                              `ObjPat (_loc, (`Constraint (_loc, p, t)), cst)
                          | None  ->
                              `ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                           'exp ) : Tokenf.txt ->
                                      'class_structure ->
                                        Tokenf.txt ->
                                          'ctyp ->
                                            Tokenf.txt ->
                                              'pat ->
                                                Tokenf.txt ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         let cst = None in
                         (match cst with
                          | Some cst -> `Obj (_loc, cst)
                          | None  -> `ObjEnd _loc : 'exp ) : Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match cst with | Some cst -> `Obj (_loc, cst) | None  -> `ObjEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (_loc : Locf.t) 
                         ->
                         let cst = Some cst in
                         (match cst with
                          | Some cst -> `Obj (_loc, cst)
                          | None  -> `ObjEnd _loc : 'exp ) : Tokenf.txt ->
                                                               'class_structure
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 120);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "-"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Self];
                  annot = "Fan_ops.mkumin _loc x e\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                          (_loc : Locf.t)  ->
                          let x = __fan_0.txt in
                          (Fan_ops.mkumin _loc x e : 'exp ) : 'exp ->
                                                                Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "-."); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self];
                 annot = "Fan_ops.mkumin _loc x e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (Fan_ops.mkumin _loc x e : 'exp ) : 'exp ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 130);
             lassoc = true;
             productions =
               [{
                  symbols = [Self; Self];
                  annot = "`App (_loc, e1, e2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e2 : 'exp)  (e1 : 'exp)  (_loc : Locf.t)  ->
                          (`App (_loc, e1, e2) : 'exp ) : 'exp ->
                                                            'exp ->
                                                              Locf.t -> 'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "assert");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`Assert (_loc, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                         (`Assert (_loc, e) : 'exp ) : 'exp ->
                                                         Tokenf.txt ->
                                                           Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "new"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_longident : 'class_longident Gramf.t ))];
                 annot = "`New (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                         (`New (_loc, i) : 'exp ) : 'class_longident ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "lazy"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`Lazy (_loc, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (_loc : Locf.t)  ->
                         (`Lazy (_loc, e) : 'exp ) : 'exp ->
                                                       Tokenf.txt ->
                                                         Locf.t -> 'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 140);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "~"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`Label (_loc, i, e)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  _  (i : 'a_lident)  _ 
                          (_loc : Locf.t)  -> (`Label (_loc, i, e) : 
                          'exp ) : 'exp ->
                                     Tokenf.txt ->
                                       'a_lident ->
                                         Tokenf.txt -> Locf.t -> 'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "~"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`LabelS (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`LabelS (_loc, i) : 'exp ) : 'a_lident ->
                                                         Tokenf.txt ->
                                                           Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Label; word = Any; tag_name = "Label" }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Label (_loc, (`Lid (_loc, i)), (e :>Astf.exp)) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Label (_loc, (`Lid (_loc, i)), (e :>Astf.exp)) :>
                           Astf.exp) : 'exp ) : 'exp ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Optlabel;
                             word = Any;
                             tag_name = "Optlabel"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`OptLabl (_loc, (`Lid (_loc, i)), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (`OptLabl (_loc, (`Lid (_loc, i)), e) : 'exp ) : 
                      'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "?"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`OptLabl (_loc, i, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (i : 'a_lident)  _  (_loc : Locf.t)
                          -> (`OptLabl (_loc, i, e) : 'exp ) : 'exp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'a_lident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "?"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`OptLablS (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`OptLablS (_loc, i) : 'exp ) : 'a_lident ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 150);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ")"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "`ArrayDot (_loc, e1, e2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (e2 : 'exp)  _  _  (e1 : 'exp) 
                          (_loc : Locf.t)  ->
                          (`ArrayDot (_loc, e1, e2) : 'exp ) : Tokenf.txt ->
                                                                 'exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Locf.t ->
                                                                    'exp ))
                };
               {
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`StringDot (_loc, e1, e2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e2 : 'exp)  _  _  (e1 : 'exp)  (_loc : Locf.t)
                          -> (`StringDot (_loc, e1, e2) : 'exp ) : Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "{"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "Fan_ops.bigarray_get _loc e1 e2\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e2 : 'comma_exp)  _  _  (e1 : 'exp) 
                         (_loc : Locf.t)  ->
                         (Fan_ops.bigarray_get _loc e1 e2 : 'exp ) : 
                      Tokenf.txt ->
                        'comma_exp ->
                          Tokenf.txt -> Tokenf.txt -> 'exp -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (label_longident : 'label_longident Gramf.t ))];
                 annot = "`Field (_loc, e1, e2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e2 : 'label_longident)  _  (e1 : 'exp) 
                         (_loc : Locf.t)  -> (`Field (_loc, e1, e2) : 
                         'exp ) : 'label_longident ->
                                    Tokenf.txt -> 'exp -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "#"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`Send (_loc, e, lab)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (lab : 'a_lident)  _  (e : 'exp)  (_loc : Locf.t) 
                         -> (`Send (_loc, e, lab) : 'exp ) : 'a_lident ->
                                                               Tokenf.txt ->
                                                                 'exp ->
                                                                   Locf.t ->
                                                                    'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 160);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "!"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Self];
                  annot = "`App (_loc, (`Lid (xloc, x)), e)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                          (_loc : Locf.t)  ->
                          let xloc = __fan_0.loc in
                          let x = __fan_0.txt in
                          (`App (_loc, (`Lid (xloc, x)), e) : 'exp ) : 
                       'exp -> Tokenf.txt -> Locf.t -> 'exp ))
                };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Pre; word = Any; tag_name = "Pre" }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`App (_loc, (`Lid (xloc, x)), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let xloc = __fan_0.loc in
                         let x = __fan_0.txt in
                         (`App (_loc, (`Lid (xloc, x)), e) : 'exp ) : 
                      'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (exp : 'exp Gramf.t );
        olevel =
          ({
             label = (Some 170);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Int; word = Any; tag_name = "Int" }
                        } : Tokenf.pattern )];
                  annot = "`Int (_loc, s)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                          let s = __fan_0.txt in (`Int (_loc, s) : 'exp ) : 
                       Tokenf.txt -> Locf.t -> 'exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Int32; word = Any; tag_name = "Int32" }
                       } : Tokenf.pattern )];
                 annot = "`Int32 (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Int32 (_loc, s) : 'exp ) : 
                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Int64; word = Any; tag_name = "Int64" }
                       } : Tokenf.pattern )];
                 annot = "`Int64 (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Int64 (_loc, s) : 'exp ) : 
                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Nativeint;
                             word = Any;
                             tag_name = "Nativeint"
                           }
                       } : Tokenf.pattern )];
                 annot = "`Nativeint (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         (`Nativeint (_loc, s) : 'exp ) : Tokenf.txt ->
                                                            Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Flo; word = Any; tag_name = "Flo" }
                       } : Tokenf.pattern )];
                 annot = "`Flo (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Flo (_loc, s) : 'exp ) : 
                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Chr; word = Any; tag_name = "Chr" }
                       } : Tokenf.pattern )];
                 annot = "`Chr (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Chr (_loc, s) : 'exp ) : 
                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                       } : Tokenf.pattern )];
                 annot = "`Str (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Str (_loc, s) : 'exp ) : 
                      Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "true"); tag_name = "Key"
                           }
                       } : Tokenf.pattern )];
                 annot = "`Bool (_loc, true)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (_loc : Locf.t)  ->
                         (`Bool (_loc, true) : 'exp ) : Tokenf.txt ->
                                                          Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "false"); tag_name = "Key"
                           }
                       } : Tokenf.pattern )];
                 annot = "`Bool (_loc, false)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (_loc : Locf.t)  ->
                         (`Bool (_loc, false) : 'exp ) : Tokenf.txt ->
                                                           Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Try
                      (Nterm
                         (Gramf.obj
                            (module_longident_dot_lparen : 'module_longident_dot_lparen
                                                             Gramf.t )));
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`LetOpen (_loc, (`Negative _loc), i, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e : 'exp)  (i : 'module_longident_dot_lparen) 
                         (_loc : Locf.t)  ->
                         (`LetOpen (_loc, (`Negative _loc), i, e) : 'exp ) : 
                      Tokenf.txt ->
                        'exp ->
                          'module_longident_dot_lparen -> Locf.t -> 'exp ))
               };
               {
                 symbols = [Nterm (Gramf.obj (vid : 'vid Gramf.t ))];
                 annot = "(i : vid  :>exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  (_loc : Locf.t)  ->
                         ((i : vid  :>exp) : 'exp ) : 'vid -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "`"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (luident : 'luident Gramf.t ))];
                 annot = "`Vrn (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (s : 'luident)  _  (_loc : Locf.t)  ->
                         (`Vrn (_loc, s) : 'exp ) : 'luident ->
                                                      Tokenf.txt ->
                                                        Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "["); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "(`Uid (_loc, \"[]\") :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         ((`Uid (_loc, "[]") :>Astf.exp) : 'exp ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "["); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (sem_exp_for_list : 'sem_exp_for_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (s : 'sem_exp_for_list)  _  (_loc : Locf.t)  ->
                         (s : 'exp ) : Tokenf.txt ->
                                         'sem_exp_for_list ->
                                           Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "[|"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ArrayEmpty _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         (`ArrayEmpty _loc : 'exp ) : Tokenf.txt ->
                                                        Tokenf.txt ->
                                                          Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "[|"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (sem_exp : 'sem_exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Array (_loc, el)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (el : 'sem_exp)  _  (_loc : Locf.t)  ->
                         (`Array (_loc, el) : 'exp ) : Tokenf.txt ->
                                                         'sem_exp ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`RecordWith (_loc, el, (`Lid (xloc, x)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (el : 'label_exp_list)  _ 
                         (__fan_1 : Tokenf.txt)  _  (_loc : Locf.t)  ->
                         let xloc = __fan_1.loc in
                         let x = __fan_1.txt in
                         (`RecordWith (_loc, el, (`Lid (xloc, x))) : 
                           'exp ) : Tokenf.txt ->
                                      'label_exp_list ->
                                        Tokenf.txt ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Record (_loc, el)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (el : 'label_exp_list)  _  (_loc : Locf.t)  ->
                         (`Record (_loc, el) : 'exp ) : Tokenf.txt ->
                                                          'label_exp_list ->
                                                            Tokenf.txt ->
                                                              Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`RecordWith (_loc, el, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (el : 'label_exp_list)  _  _  (e : 'exp)  _  _ 
                         (_loc : Locf.t)  ->
                         (`RecordWith (_loc, el, e) : 'exp ) : Tokenf.txt ->
                                                                 'label_exp_list
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{<"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ">}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`OvrInstEmpty _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         (`OvrInstEmpty _loc : 'exp ) : Tokenf.txt ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "{<"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (field_exp_list : 'field_exp_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ">}"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`OvrInst (_loc, fel)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (fel : 'field_exp_list)  _  (_loc : Locf.t)  ->
                         (`OvrInst (_loc, fel) : 'exp ) : Tokenf.txt ->
                                                            'field_exp_list
                                                              ->
                                                              Tokenf.txt ->
                                                                Locf.t ->
                                                                  'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Unit _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  -> (`Unit _loc : 'exp ) : 
                      Tokenf.txt -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Constraint (_loc, e, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t) 
                         -> (`Constraint (_loc, e, t) : 'exp ) : Tokenf.txt
                                                                   ->
                                                                   'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ","); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (comma_exp : 'comma_exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Par (_loc, (`Com (_loc, e, el)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (el : 'comma_exp)  _  (e : 'exp)  _ 
                         (_loc : Locf.t)  ->
                         (`Par (_loc, (`Com (_loc, e, el))) : 'exp ) : 
                      Tokenf.txt ->
                        'comma_exp ->
                          Tokenf.txt -> 'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Seq (_loc, (`Sem (_loc, e, seq)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (seq : 'sequence)  _  (e : 'exp)  _ 
                         (_loc : Locf.t)  ->
                         (`Seq (_loc, (`Sem (_loc, e, seq))) : 'exp ) : 
                      Tokenf.txt ->
                        'sequence ->
                          Tokenf.txt -> 'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Seq (_loc, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (e : 'exp)  _  (_loc : Locf.t)  ->
                         (`Seq (_loc, e) : 'exp ) : Tokenf.txt ->
                                                      Tokenf.txt ->
                                                        'exp ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":>"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Coercion (_loc, e, t, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (t2 : 'ctyp)  _  (t : 'ctyp)  _  (e : 'exp)  _ 
                         (_loc : Locf.t)  ->
                         (`Coercion (_loc, e, t, t2) : 'exp ) : Tokenf.txt ->
                                                                  'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":>"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Subtype (_loc, e, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (t : 'ctyp)  _  (e : 'exp)  _  (_loc : Locf.t) 
                         -> (`Subtype (_loc, e, t) : 'exp ) : Tokenf.txt ->
                                                                'ctyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "e\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e : 'exp)  _  (_loc : Locf.t)  -> (e : 'exp ) : 
                      Tokenf.txt -> 'exp -> Tokenf.txt -> Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "begin"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match seq with | Some seq -> `Seq (_loc, seq) | None  -> `Unit _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  ->
                         let seq = None in
                         (match seq with
                          | Some seq -> `Seq (_loc, seq)
                          | None  -> `Unit _loc : 'exp ) : Tokenf.txt ->
                                                             Tokenf.txt ->
                                                               Locf.t -> 'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "begin"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match seq with | Some seq -> `Seq (_loc, seq) | None  -> `Unit _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (seq : 'sequence)  _  (_loc : Locf.t)  ->
                         let seq = Some seq in
                         (match seq with
                          | Some seq -> `Seq (_loc, seq)
                          | None  -> `Unit _loc : 'exp ) : Tokenf.txt ->
                                                             'sequence ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "module"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Package_exp (_loc, me)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (me : 'mexp)  _  _  (_loc : Locf.t)  ->
                         (`Package_exp (_loc, me) : 'exp ) : Tokenf.txt ->
                                                               'mexp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'exp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "module"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mexp : 'mexp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "`Package_exp (_loc, (`Constraint (_loc, me, pt)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (pt : 'mtyp)  _  (me : 'mexp)  _  _ 
                         (_loc : Locf.t)  ->
                         (`Package_exp (_loc, (`Constraint (_loc, me, pt))) : 
                         'exp ) : Tokenf.txt ->
                                    'mtyp ->
                                      Tokenf.txt ->
                                        'mexp ->
                                          Tokenf.txt ->
                                            Tokenf.txt -> Locf.t -> 'exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (sem_exp_for_list : 'sem_exp_for_list Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ";"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "(`App\n   (_loc, (`App (_loc, (`Uid (_loc, \"::\")), (e :>Astf.exp))),\n     (el :>Astf.exp)) :>Astf.exp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (el : 'sem_exp_for_list)  _  (e : 'exp) 
                          (_loc : Locf.t)  ->
                          ((`App
                              (_loc,
                                (`App
                                   (_loc, (`Uid (_loc, "::")),
                                     (e :>Astf.exp))), (el :>Astf.exp)) :>
                          Astf.exp) : 'sem_exp_for_list ) : 'sem_exp_for_list
                                                              ->
                                                              Tokenf.txt ->
                                                                'exp ->
                                                                  Locf.t ->
                                                                    'sem_exp_for_list ))
                };
               {
                 symbols = [Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(`App\n   (_loc, (`App (_loc, (`Uid (_loc, \"::\")), (e :>Astf.exp))),\n     (`Uid (_loc, \"[]\"))) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (_loc : Locf.t)  ->
                         ((`App
                             (_loc,
                               (`App
                                  (_loc, (`Uid (_loc, "::")), (e :>Astf.exp))),
                               (`Uid (_loc, "[]"))) :>Astf.exp) : 'sem_exp_for_list ) : 
                      'exp -> Locf.t -> 'sem_exp_for_list ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "(`App\n   (_loc, (`App (_loc, (`Uid (_loc, \"::\")), (e :>Astf.exp))),\n     (`Uid (_loc, \"[]\"))) :>Astf.exp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (e : 'exp)  (_loc : Locf.t)  ->
                         ((`App
                             (_loc,
                               (`App
                                  (_loc, (`Uid (_loc, "::")), (e :>Astf.exp))),
                               (`Uid (_loc, "[]"))) :>Astf.exp) : 'sem_exp_for_list ) : 
                      Tokenf.txt -> 'exp -> Locf.t -> 'sem_exp_for_list ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (sequence : 'sequence Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "let"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                    Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "in"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                    Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))];
                  annot = "k (`LetIn (_loc, rf, bi, e))\n";
                  fn =
                    (Gramf.mk_action
                       (fun (k : 'sequence')  (e : 'exp)  _  (bi : 'bind) 
                          (rf : 'opt_rec)  _  (_loc : Locf.t)  ->
                          (k (`LetIn (_loc, rf, bi, e)) : 'sequence ) : 
                       'sequence' ->
                         'exp ->
                           Tokenf.txt ->
                             'bind ->
                               'opt_rec -> Tokenf.txt -> Locf.t -> 'sequence ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "try"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (case : 'case Gramf.t ));
                   Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))];
                 annot = "k (`LetTryInWith (_loc, r, bi, x, a))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (k : 'sequence')  (a : 'case)  _  (x : 'sequence) 
                         _  (bi : 'bind)  (r : 'opt_rec)  _  _ 
                         (_loc : Locf.t)  ->
                         (k (`LetTryInWith (_loc, r, bi, x, a)) : 'sequence ) : 
                      'sequence' ->
                        'case ->
                          Tokenf.txt ->
                            'sequence ->
                              Tokenf.txt ->
                                'bind ->
                                  'opt_rec ->
                                    Tokenf.txt ->
                                      Tokenf.txt -> Locf.t -> 'sequence ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "module"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))];
                 annot = "k (`LetModule (_loc, m, mb, e))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (k : 'sequence')  (e : 'exp)  _  (mb : 'mbind0) 
                         (m : 'a_uident)  _  _  (_loc : Locf.t)  ->
                         (k (`LetModule (_loc, m, mb, e)) : 'sequence ) : 
                      'sequence' ->
                        'exp ->
                          Tokenf.txt ->
                            'mbind0 ->
                              'a_uident ->
                                Tokenf.txt ->
                                  Tokenf.txt -> Locf.t -> 'sequence ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'sequence)  _  (i : 'module_longident)  _  _ 
                         (_loc : Locf.t)  ->
                         let bang = None in
                         (`LetOpen
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident), e) : 'sequence ) : 
                      'sequence ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt -> Tokenf.txt -> Locf.t -> 'sequence ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`LetOpen\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'sequence)  _  (i : 'module_longident) 
                         (bang : Tokenf.txt)  _  _  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (`LetOpen
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident), e) : 'sequence ) : 
                      'sequence ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt ->
                              Tokenf.txt -> Tokenf.txt -> Locf.t -> 'sequence ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Nterm (Gramf.obj (sequence' : 'sequence' Gramf.t ))];
                 annot = "k e\n";
                 fn =
                   (Gramf.mk_action
                      (fun (k : 'sequence')  (e : 'exp)  (_loc : Locf.t)  ->
                         (k e : 'sequence ) : 'sequence' ->
                                                'exp -> Locf.t -> 'sequence ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (sequence' : 'sequence' Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [];
                  annot = "fun e  -> e\n";
                  fn =
                    (Gramf.mk_action
                       (fun (_loc : Locf.t)  -> (fun e  -> e : 'sequence' ) : 
                       Locf.t -> 'sequence' ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ";"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                 annot = "fun e  -> e\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (_loc : Locf.t)  -> (fun e  -> e : 'sequence' ) : 
                      Tokenf.txt -> Locf.t -> 'sequence' ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ";"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (sequence : 'sequence Gramf.t ))];
                 annot = "fun e  -> `Sem (_loc, e, el)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (el : 'sequence)  _  (_loc : Locf.t)  ->
                         (fun e  -> `Sem (_loc, e, el) : 'sequence' ) : 
                      'sequence -> Tokenf.txt -> Locf.t -> 'sequence' ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  Gramf.extend_single
    ({
       entry = (with_exp_lang : 'with_exp_lang Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (lang : 'lang Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "Ast_quotation.default := old; x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'exp)  _  (old : 'lang)  (_loc : Locf.t)  ->
                         (Ast_quotation.default := old; x : 'with_exp_lang ) : 
                      'exp -> Tokenf.txt -> 'lang -> Locf.t -> 'with_exp_lang ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (with_stru_lang : 'with_stru_lang Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (lang : 'lang Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (stru : 'stru Gramf.t ))];
                 annot = "Ast_quotation.default := old; x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'stru)  _  (old : 'lang)  (_loc : Locf.t)  ->
                         (Ast_quotation.default := old; x : 'with_stru_lang ) : 
                      'stru ->
                        Tokenf.txt -> 'lang -> Locf.t -> 'with_stru_lang ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  (Gramf.extend_single
     ({
        entry = (bind_quot : 'bind_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (bind : 'bind Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'bind)  (_loc : Locf.t)  ->
                          (x : 'bind_quot ) : 'bind -> Locf.t -> 'bind_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (bind : 'bind Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Ant;
                              word = (Kind "bind");
                              tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.bind) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.bind) s : 
                            'bind ) : Tokenf.ant -> Locf.t -> 'bind ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.bind) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.bind) s : 
                           'bind ) : Tokenf.ant -> Locf.t -> 'bind ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(`Bind\n   (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s :>Astf.pat),\n     (e :>Astf.exp)) :>Astf.bind)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Bind
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.pat) s :>
                               Astf.pat), (e :>Astf.exp)) :>Astf.bind) : 
                           'bind ) : 'exp ->
                                       Tokenf.txt ->
                                         Tokenf.ant -> Locf.t -> 'bind ))
               };
               {
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "and"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`And (_loc, b1, b2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (b2 : 'bind)  _  (b1 : 'bind)  (_loc : Locf.t)  ->
                         (`And (_loc, b1, b2) : 'bind ) : 'bind ->
                                                            Tokenf.txt ->
                                                              'bind ->
                                                                Locf.t ->
                                                                  'bind ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))];
                 annot = "`Bind (_loc, p, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_bind)  (p : 'pat)  (_loc : Locf.t)  ->
                         (`Bind (_loc, p, e) : 'bind ) : 'fun_bind ->
                                                           'pat ->
                                                             Locf.t -> 'bind ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (rec_exp_quot : 'rec_exp_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj (label_exp_list : 'label_exp_list Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'label_exp_list)  (_loc : Locf.t)  ->
                          (x : 'rec_exp_quot ) : 'label_exp_list ->
                                                   Locf.t -> 'rec_exp_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (field_exp : 'field_exp Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s : 
                            'field_exp ) : Tokenf.ant -> Locf.t -> 'field_exp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "bi"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.rec_exp) s : 
                           'field_exp ) : Tokenf.ant -> Locf.t -> 'field_exp ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "`RecBind (_loc, (l :>vid), e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (l : 'a_lident)  (_loc : Locf.t) 
                         -> (`RecBind (_loc, (l :>vid), e) : 'field_exp ) : 
                      'exp -> Tokenf.txt -> 'a_lident -> Locf.t -> 'field_exp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (luident : 'luident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Lid; word = Any; tag_name = "Lid" }
                        } : Tokenf.pattern )];
                  annot = "i\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                          let i = __fan_0.txt in (i : 'luident ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'luident ))
                };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "i\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (i : 'luident ) : Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'luident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (aident : 'aident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                  annot = "(i :>ident)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                          ((i :>ident) : 'aident ) : 'a_lident ->
                                                       Locf.t -> 'aident ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))];
                 annot = "(i :>ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_uident)  (_loc : Locf.t)  ->
                         ((i :>ident) : 'aident ) : 'a_uident ->
                                                      Locf.t -> 'aident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (astr : 'astr Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Lid; word = Any; tag_name = "Lid" }
                        } : Tokenf.pattern )];
                  annot = "`C (_loc, i)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                          let i = __fan_0.txt in (`C (_loc, i) : 'astr ) : 
                       Tokenf.txt -> Locf.t -> 'astr ))
                };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`C (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (`C (_loc, i) : 'astr ) : 
                      Tokenf.txt -> Locf.t -> 'astr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'astr ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'astr ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "vrn");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'astr ) : Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'astr ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (ident_quot : 'ident_quot Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "(`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>Astf.ident)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (j : 'ident_quot)  _  (i : 'ident_quot) 
                          (_loc : Locf.t)  ->
                          ((`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>
                          Astf.ident) : 'ident_quot ) : 'ident_quot ->
                                                          Tokenf.txt ->
                                                            'ident_quot ->
                                                              Locf.t ->
                                                                'ident_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (ident_quot : 'ident_quot Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                            'ident_quot ) : Tokenf.ant ->
                                              Locf.t -> 'ident_quot ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident_quot ) : Tokenf.ant ->
                                             Locf.t -> 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident_quot ) : Tokenf.ant ->
                                             Locf.t -> 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident_quot ) : Tokenf.ant ->
                                             Locf.t -> 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident_quot ) : 'ident_quot ->
                                                     Tokenf.txt ->
                                                       Tokenf.ant ->
                                                         Locf.t ->
                                                           'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident_quot ) : 'ident_quot ->
                                                     Tokenf.txt ->
                                                       Tokenf.ant ->
                                                         Locf.t ->
                                                           'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident_quot)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident_quot ) : 'ident_quot ->
                                                     Tokenf.txt ->
                                                       Tokenf.ant ->
                                                         Locf.t ->
                                                           'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "(`Lid (_loc, i) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Lid (_loc, i) :>Astf.ident) : 'ident_quot ) : 
                      Tokenf.txt -> Locf.t -> 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "(`Uid (_loc, i) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Uid (_loc, i) :>Astf.ident) : 'ident_quot ) : 
                      Tokenf.txt -> Locf.t -> 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Dot (_loc, (`Uid (_loc, s)), (j :>Astf.ident)) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (j : 'ident_quot)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         ((`Dot (_loc, (`Uid (_loc, s)), (j :>Astf.ident)) :>
                           Astf.ident) : 'ident_quot ) : 'ident_quot ->
                                                           Tokenf.txt ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'ident_quot ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Apply (_loc, i, j)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (j : 'ident_quot)  (i : 'ident_quot)  _ 
                         (_loc : Locf.t)  ->
                         (`Apply (_loc, i, j) : 'ident_quot ) : Tokenf.txt ->
                                                                  'ident_quot
                                                                    ->
                                                                    'ident_quot
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ident_quot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (ident : 'ident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                            'ident ) : Tokenf.ant -> Locf.t -> 'ident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident ) : Tokenf.ant -> Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident ) : Tokenf.ant -> Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'ident ) : Tokenf.ant -> Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident ) : 'ident ->
                                                Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident ) : 'ident ->
                                                Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'ident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                              i) : 'ident ) : 'ident ->
                                                Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "`Lid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (`Lid (_loc, i) : 'ident ) : 
                      Tokenf.txt -> Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`Uid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (`Uid (_loc, i) : 'ident ) : 
                      Tokenf.txt -> Locf.t -> 'ident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, s)), j)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (j : 'ident)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, s)), j) : 'ident ) : 
                      'ident -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (vid : 'vid Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                            'vid ) : Tokenf.ant -> Locf.t -> 'vid ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'vid ) : Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'vid ) : Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'vid ) : Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              i) : 'vid ) : 'vid ->
                                              Tokenf.txt ->
                                                Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              i) : 'vid ) : 'vid ->
                                              Tokenf.txt ->
                                                Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'vid)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              i) : 'vid ) : 'vid ->
                                              Tokenf.txt ->
                                                Tokenf.ant -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "`Lid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (`Lid (_loc, i) : 'vid ) : 
                      Tokenf.txt -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`Uid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in (`Uid (_loc, i) : 'vid ) : 
                      Tokenf.txt -> Locf.t -> 'vid ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, s)), j)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (j : 'vid)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, s)), j) : 'vid ) : 
                      'vid -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'vid ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (uident : 'uident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Uid; word = Any; tag_name = "Uid" }
                        } : Tokenf.pattern )];
                  annot = "`Uid (_loc, s)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                          let s = __fan_0.txt in (`Uid (_loc, s) : 'uident ) : 
                       Tokenf.txt -> Locf.t -> 'uident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s : 
                           'uident ) : Tokenf.ant -> Locf.t -> 'uident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s : 
                           'uident ) : Tokenf.ant -> Locf.t -> 'uident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s : 
                           'uident ) : Tokenf.ant -> Locf.t -> 'uident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "dot (`Uid (_loc, s)) l\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'uident)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0.txt in
                         (dot (`Uid (_loc, s)) l : 'uident ) : 'uident ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'uident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "dot (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s) i\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'uident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (dot
                            (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s)
                            i : 'uident ) : 'uident ->
                                              Tokenf.txt ->
                                                Tokenf.ant ->
                                                  Locf.t -> 'uident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "dot (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s) i\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'uident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (dot
                            (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s)
                            i : 'uident ) : 'uident ->
                                              Tokenf.txt ->
                                                Tokenf.ant ->
                                                  Locf.t -> 'uident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "dot (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s) i\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'uident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (dot
                            (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.uident) s)
                            i : 'uident ) : 'uident ->
                                              Tokenf.txt ->
                                                Tokenf.ant ->
                                                  Locf.t -> 'uident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (dot_lstrings : 'dot_lstrings Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Lid; word = Any; tag_name = "Lid" }
                        } : Tokenf.pattern )];
                  annot = "{ domain = (`Sub []); name = i }\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                          let i = __fan_0.txt in
                          ({ domain = (`Sub []); name = i } : 'dot_lstrings ) : 
                       Tokenf.txt -> Locf.t -> 'dot_lstrings ))
                };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "match x with\n| { domain = `Sub xs;_} -> { x with domain = (`Sub (i :: xs)) }\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'dot_lstrings)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (match x with
                          | { domain = `Sub xs;_} ->
                              { x with domain = (`Sub (i :: xs)) }
                          | _ ->
                              raise (Streamf.Error "impossible dot_lstrings") : 
                           'dot_lstrings ) : 'dot_lstrings ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Locf.t -> 'dot_lstrings ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "match x with\n| { domain = `Sub xs;_} -> { x with domain = (`Absolute (i :: xs)) }\n| _ -> raise (Streamf.Error \"impossible dot_lstrings\")\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'dot_lstrings)  _  (__fan_1 : Tokenf.txt)  _ 
                         (_loc : Locf.t)  ->
                         let i = __fan_1.txt in
                         (match x with
                          | { domain = `Sub xs;_} ->
                              { x with domain = (`Absolute (i :: xs)) }
                          | _ ->
                              raise (Streamf.Error "impossible dot_lstrings") : 
                           'dot_lstrings ) : 'dot_lstrings ->
                                               Tokenf.txt ->
                                                 Tokenf.txt ->
                                                   Tokenf.txt ->
                                                     Locf.t -> 'dot_lstrings ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry =
          (module_longident_dot_lparen : 'module_longident_dot_lparen Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                            'module_longident_dot_lparen ) : Tokenf.txt ->
                                                               Tokenf.txt ->
                                                                 Tokenf.ant
                                                                   ->
                                                                   Locf.t ->
                                                                    'module_longident_dot_lparen ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'module_longident_dot_lparen ) : Tokenf.txt ->
                                                              Tokenf.txt ->
                                                                Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'module_longident_dot_lparen ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'module_longident_dot_lparen ) : Tokenf.txt ->
                                                              Tokenf.txt ->
                                                                Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'module_longident_dot_lparen ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Dot (_loc, (`Uid (_loc, i)), (l :>Astf.ident)) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident_dot_lparen)  _ 
                         (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Dot (_loc, (`Uid (_loc, i)), (l :>Astf.ident)) :>
                           Astf.ident) : 'module_longident_dot_lparen ) : 
                      'module_longident_dot_lparen ->
                        Tokenf.txt ->
                          Tokenf.txt ->
                            Locf.t -> 'module_longident_dot_lparen ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "(`Uid (_loc, i) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Uid (_loc, i) :>Astf.ident) : 'module_longident_dot_lparen ) : 
                      Tokenf.txt ->
                        Tokenf.txt ->
                          Tokenf.txt ->
                            Locf.t -> 'module_longident_dot_lparen ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Dot\n   (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s :>Astf.ident),\n     (l :>Astf.ident)) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident_dot_lparen)  _ 
                         (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Dot
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s :>
                               Astf.ident), (l :>Astf.ident)) :>Astf.ident) : 
                           'module_longident_dot_lparen ) : 'module_longident_dot_lparen
                                                              ->
                                                              Tokenf.txt ->
                                                                Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'module_longident_dot_lparen ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Dot\n   (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s :>Astf.ident),\n     (l :>Astf.ident)) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident_dot_lparen)  _ 
                         (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Dot
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s :>
                               Astf.ident), (l :>Astf.ident)) :>Astf.ident) : 
                           'module_longident_dot_lparen ) : 'module_longident_dot_lparen
                                                              ->
                                                              Tokenf.txt ->
                                                                Tokenf.ant ->
                                                                  Locf.t ->
                                                                    'module_longident_dot_lparen ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (module_longident : 'module_longident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                            'module_longident ) : Tokenf.ant ->
                                                    Locf.t ->
                                                      'module_longident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'module_longident ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'module_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'module_longident ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'module_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (_loc, i)), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.txt)
                          (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (`Dot (_loc, (`Uid (_loc, i)), l) : 'module_longident ) : 
                      'module_longident ->
                        Tokenf.txt ->
                          Tokenf.txt -> Locf.t -> 'module_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`Uid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (`Uid (_loc, i) : 'module_longident ) : Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'module_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.ant)
                          (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              l) : 'module_longident ) : 'module_longident ->
                                                           Tokenf.txt ->
                                                             Tokenf.ant ->
                                                               Locf.t ->
                                                                 'module_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'module_longident)  _  (__fan_0 : Tokenf.ant)
                          (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              l) : 'module_longident ) : 'module_longident ->
                                                           Tokenf.txt ->
                                                             Tokenf.ant ->
                                                               Locf.t ->
                                                                 'module_longident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry =
          (module_longident_with_app : 'module_longident_with_app Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols = [Self; Self];
                  annot = "`Apply (_loc, i, j)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (j : 'module_longident_with_app) 
                          (i : 'module_longident_with_app)  (_loc : Locf.t) 
                          ->
                          (`Apply (_loc, i, j) : 'module_longident_with_app ) : 
                       'module_longident_with_app ->
                         'module_longident_with_app ->
                           Locf.t -> 'module_longident_with_app ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry =
          (module_longident_with_app : 'module_longident_with_app Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "(`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>Astf.ident)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (j : 'module_longident_with_app)  _ 
                          (i : 'module_longident_with_app)  (_loc : Locf.t) 
                          ->
                          ((`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>
                          Astf.ident) : 'module_longident_with_app ) : 
                       'module_longident_with_app ->
                         Tokenf.txt ->
                           'module_longident_with_app ->
                             Locf.t -> 'module_longident_with_app ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry =
          (module_longident_with_app : 'module_longident_with_app Gramf.t );
        olevel =
          ({
             label = (Some 30);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                            'module_longident_with_app ) : Tokenf.ant ->
                                                             Locf.t ->
                                                               'module_longident_with_app ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'module_longident_with_app ) : Tokenf.ant ->
                                                            Locf.t ->
                                                              'module_longident_with_app ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'module_longident_with_app ) : Tokenf.ant ->
                                                            Locf.t ->
                                                              'module_longident_with_app ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`Uid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (`Uid (_loc, i) : 'module_longident_with_app ) : 
                      Tokenf.txt -> Locf.t -> 'module_longident_with_app ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "i\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (i : 'module_longident_with_app)  _ 
                         (_loc : Locf.t)  ->
                         (i : 'module_longident_with_app ) : Tokenf.txt ->
                                                               'module_longident_with_app
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'module_longident_with_app ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (type_longident : 'type_longident Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols = [Self; Self];
                  annot = "`Apply (_loc, i, j)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (j : 'type_longident)  (i : 'type_longident) 
                          (_loc : Locf.t)  ->
                          (`Apply (_loc, i, j) : 'type_longident ) : 
                       'type_longident ->
                         'type_longident -> Locf.t -> 'type_longident ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (type_longident : 'type_longident Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "."); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot =
                    "(`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>Astf.ident)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (j : 'type_longident)  _  (i : 'type_longident) 
                          (_loc : Locf.t)  ->
                          ((`Dot (_loc, (i :>Astf.ident), (j :>Astf.ident)) :>
                          Astf.ident) : 'type_longident ) : 'type_longident
                                                              ->
                                                              Tokenf.txt ->
                                                                'type_longident
                                                                  ->
                                                                  Locf.t ->
                                                                    'type_longident ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (type_longident : 'type_longident Gramf.t );
        olevel =
          ({
             label = (Some 30);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                            'type_longident ) : Tokenf.ant ->
                                                  Locf.t -> 'type_longident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'type_longident ) : Tokenf.ant ->
                                                 Locf.t -> 'type_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'type_longident ) : Tokenf.ant ->
                                                 Locf.t -> 'type_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s : 
                           'type_longident ) : Tokenf.ant ->
                                                 Locf.t -> 'type_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "(`Lid (_loc, i) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Lid (_loc, i) :>Astf.ident) : 'type_longident ) : 
                      Tokenf.txt -> Locf.t -> 'type_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "(`Uid (_loc, i) :>Astf.ident)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         ((`Uid (_loc, i) :>Astf.ident) : 'type_longident ) : 
                      Tokenf.txt -> Locf.t -> 'type_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "i\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (i : 'type_longident)  _  (_loc : Locf.t)  ->
                         (i : 'type_longident ) : Tokenf.txt ->
                                                    'type_longident ->
                                                      Tokenf.txt ->
                                                        Locf.t ->
                                                          'type_longident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (label_longident : 'label_longident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                            'label_longident ) : Tokenf.ant ->
                                                   Locf.t -> 'label_longident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'label_longident ) : Tokenf.ant ->
                                                  Locf.t -> 'label_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s : 
                           'label_longident ) : Tokenf.ant ->
                                                  Locf.t -> 'label_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "`Lid (_loc, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let i = __fan_0.txt in
                         (`Lid (_loc, i) : 'label_longident ) : Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'label_longident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Dot (_loc, (`Uid (iloc, i)), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let iloc = __fan_0.loc in
                         let i = __fan_0.txt in
                         (`Dot (_loc, (`Uid (iloc, i)), l) : 'label_longident ) : 
                      'label_longident ->
                        Tokenf.txt ->
                          Tokenf.txt -> Locf.t -> 'label_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              l) : 'label_longident ) : 'label_longident ->
                                                          Tokenf.txt ->
                                                            Tokenf.ant ->
                                                              Locf.t ->
                                                                'label_longident ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s), l)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (l : 'label_longident)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Dot
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.vid) s),
                              l) : 'label_longident ) : 'label_longident ->
                                                          Tokenf.txt ->
                                                            Tokenf.ant ->
                                                              Locf.t ->
                                                                'label_longident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (cltyp_longident : 'cltyp_longident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'type_longident)  (_loc : Locf.t)  ->
                          (x : 'cltyp_longident ) : 'type_longident ->
                                                      Locf.t ->
                                                        'cltyp_longident ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (val_longident : 'val_longident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (ident : 'ident Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'ident)  (_loc : Locf.t)  ->
                          (x : 'val_longident ) : 'ident ->
                                                    Locf.t -> 'val_longident ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (class_longident : 'class_longident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj
                          (label_longident : 'label_longident Gramf.t ))];
                  annot = "(x : vid  :>ident)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'label_longident)  (_loc : Locf.t)  ->
                          ((x : vid  :>ident) : 'class_longident ) : 
                       'label_longident -> Locf.t -> 'class_longident ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_override : 'opt_override Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [];
                  annot =
                    "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun (_loc : Locf.t)  ->
                          let bang = None in
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc : 'opt_override ) : 
                       Locf.t -> 'opt_override ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "!"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                 annot =
                   "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (bang : Tokenf.txt)  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc : 'opt_override ) : 
                      Tokenf.txt -> Locf.t -> 'opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "!"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_override ) : Tokenf.ant ->
                                               Locf.t -> 'opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "override");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_override ) : Tokenf.ant ->
                                               Locf.t -> 'opt_override ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (flag : 'flag Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "to"); tag_name = "Key" }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  -> (`Positive _loc : 'flag ) : 
                       Tokenf.txt -> Locf.t -> 'flag ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "downto");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern )];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (_loc : Locf.t)  -> (`Negative _loc : 'flag ) : 
                      Tokenf.txt -> Locf.t -> 'flag ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind "to"); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'flag ) : Tokenf.ant -> Locf.t -> 'flag ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'flag ) : Tokenf.ant -> Locf.t -> 'flag ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_private : 'opt_private Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "private");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          (`Positive _loc : 'opt_private ) : Tokenf.txt ->
                                                               Locf.t ->
                                                                 'opt_private ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "private");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_private ) : Tokenf.ant ->
                                              Locf.t -> 'opt_private ))
               };
               {
                 symbols = [];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  ->
                         (`Negative _loc : 'opt_private ) : Locf.t ->
                                                              'opt_private ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_mutable : 'opt_mutable Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "mutable");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          (`Positive _loc : 'opt_mutable ) : Tokenf.txt ->
                                                               Locf.t ->
                                                                 'opt_mutable ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "mutable");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_mutable ) : Tokenf.ant ->
                                              Locf.t -> 'opt_mutable ))
               };
               {
                 symbols = [];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  ->
                         (`Negative _loc : 'opt_mutable ) : Locf.t ->
                                                              'opt_mutable ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_virtual : 'opt_virtual Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "virtual");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          (`Positive _loc : 'opt_virtual ) : Tokenf.txt ->
                                                               Locf.t ->
                                                                 'opt_virtual ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "virtual");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_virtual ) : Tokenf.ant ->
                                              Locf.t -> 'opt_virtual ))
               };
               {
                 symbols = [];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  ->
                         (`Negative _loc : 'opt_virtual ) : Locf.t ->
                                                              'opt_virtual ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_dot_dot : 'opt_dot_dot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A ".."); tag_name = "Key" }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          (`Positive _loc : 'opt_dot_dot ) : Tokenf.txt ->
                                                               Locf.t ->
                                                                 'opt_dot_dot ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ".."); tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_dot_dot ) : Tokenf.ant ->
                                              Locf.t -> 'opt_dot_dot ))
               };
               {
                 symbols = [];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  ->
                         (`Negative _loc : 'opt_dot_dot ) : Locf.t ->
                                                              'opt_dot_dot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (opt_rec : 'opt_rec Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "rec"); tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot = "`Positive _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          (`Positive _loc : 'opt_rec ) : Tokenf.txt ->
                                                           Locf.t -> 'opt_rec ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "rec");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'opt_rec ) : Tokenf.ant -> Locf.t -> 'opt_rec ))
               };
               {
                 symbols = [];
                 annot = "`Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  -> (`Negative _loc : 'opt_rec ) : 
                      Locf.t -> 'opt_rec ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (a_lident : 'a_lident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in (mk_ant s : 'a_lident ) : 
                       Tokenf.ant -> Locf.t -> 'a_lident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "lid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'a_lident ) : 
                      Tokenf.ant -> Locf.t -> 'a_lident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Lid; word = Any; tag_name = "Lid" }
                       } : Tokenf.pattern )];
                 annot = "`Lid (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Lid (_loc, s) : 'a_lident ) : 
                      Tokenf.txt -> Locf.t -> 'a_lident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (a_uident : 'a_uident Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in (mk_ant s : 'a_uident ) : 
                       Tokenf.ant -> Locf.t -> 'a_uident ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "uid");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'a_uident ) : 
                      Tokenf.ant -> Locf.t -> 'a_uident ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" }
                       } : Tokenf.pattern )];
                 annot = "`Uid (_loc, s)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let s = __fan_0.txt in (`Uid (_loc, s) : 'a_uident ) : 
                      Tokenf.txt -> Locf.t -> 'a_uident ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (string_list : 'string_list Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in (mk_ant s : 'string_list ) : 
                       Tokenf.ant -> Locf.t -> 'string_list ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`App (_loc, (mk_ant s), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'string_list)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`App (_loc, (mk_ant s), xs) : 'string_list ) : 
                      'string_list -> Tokenf.ant -> Locf.t -> 'string_list ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                       } : Tokenf.pattern )];
                 annot = "`Str (_loc, x)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`Str (_loc, x) : 'string_list ) : Tokenf.txt ->
                                                              Locf.t ->
                                                                'string_list ))
               };
               {
                 symbols =
                   [Token
                      ({ descr = { tag = `Str; word = Any; tag_name = "Str" }
                       } : Tokenf.pattern );
                   Self];
                 annot = "`App (_loc, (`Str (_loc, x)), xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'string_list)  (__fan_0 : Tokenf.txt) 
                         (_loc : Locf.t)  ->
                         let x = __fan_0.txt in
                         (`App (_loc, (`Str (_loc, x)), xs) : 'string_list ) : 
                      'string_list -> Tokenf.txt -> Locf.t -> 'string_list ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (rec_flag_quot : 'rec_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_rec)  (_loc : Locf.t)  ->
                          (x : 'rec_flag_quot ) : 'opt_rec ->
                                                    Locf.t -> 'rec_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (direction_flag_quot : 'direction_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (flag : 'flag Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'flag)  (_loc : Locf.t)  ->
                          (x : 'direction_flag_quot ) : 'flag ->
                                                          Locf.t ->
                                                            'direction_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (mutable_flag_quot : 'mutable_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_mutable)  (_loc : Locf.t)  ->
                          (x : 'mutable_flag_quot ) : 'opt_mutable ->
                                                        Locf.t ->
                                                          'mutable_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (private_flag_quot : 'private_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_private)  (_loc : Locf.t)  ->
                          (x : 'private_flag_quot ) : 'opt_private ->
                                                        Locf.t ->
                                                          'private_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (virtual_flag_quot : 'virtual_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_virtual)  (_loc : Locf.t)  ->
                          (x : 'virtual_flag_quot ) : 'opt_virtual ->
                                                        Locf.t ->
                                                          'virtual_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (row_var_flag_quot : 'row_var_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_dot_dot)  (_loc : Locf.t)  ->
                          (x : 'row_var_flag_quot ) : 'opt_dot_dot ->
                                                        Locf.t ->
                                                          'row_var_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (override_flag_quot : 'override_flag_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm
                       (Gramf.obj (opt_override : 'opt_override Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'opt_override)  (_loc : Locf.t)  ->
                          (x : 'override_flag_quot ) : 'opt_override ->
                                                         Locf.t ->
                                                           'override_flag_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (let quots: 'quots Gramf.t = Gramf.mk "quots" in
   Gramf.extend_single
     ({
        entry = (quots : 'quots Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `DirQuotation;
                              word = Any;
                              tag_name = "DirQuotation"
                            }
                        } : Tokenf.pattern );
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ";;"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "Ast_quotation.handle_quot x\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (__fan_0 : Tokenf.quot)  (_loc : Locf.t)  ->
                          let x = __fan_0 in
                          (Ast_quotation.handle_quot x : 'quots ) : Tokenf.txt
                                                                    ->
                                                                    Tokenf.quot
                                                                    ->
                                                                    Locf.t ->
                                                                    'quots ))
                };
               {
                 symbols = [Self; Self];
                 annot = "()\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  -> (() : 'quots ) : 
                      'quots -> 'quots -> Locf.t -> 'quots ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (implem : 'implem Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (_loc : Locf.t)  ->
                          let x = None in (x : 'implem ) : Locf.t -> 'implem ))
                };
               {
                 symbols = [Nterm (Gramf.obj (quots : 'quots Gramf.t ))];
                 annot = "x\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (_loc : Locf.t)  ->
                         let x = None in (x : 'implem ) : 'quots ->
                                                            Locf.t -> 'implem ))
               };
               {
                 symbols = [Nterm (Gramf.obj (strus : 'strus Gramf.t ))];
                 annot = "x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'strus)  (_loc : Locf.t)  ->
                         let x = Some x in (x : 'implem ) : 'strus ->
                                                              Locf.t ->
                                                                'implem ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (quots : 'quots Gramf.t ));
                   Nterm (Gramf.obj (strus : 'strus Gramf.t ))];
                 annot = "x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'strus)  _  (_loc : Locf.t)  ->
                         let x = Some x in (x : 'implem ) : 'strus ->
                                                              'quots ->
                                                                Locf.t ->
                                                                  'implem ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (use_file : 'use_file Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (stru : 'stru Gramf.t )); Self];
                  annot = "si :: rest\n";
                  fn =
                    (Gramf.mk_action
                       (fun (rest : 'use_file)  (si : 'stru)  (_loc : Locf.t)
                           -> (si :: rest : 'use_file ) : 'use_file ->
                                                            'stru ->
                                                              Locf.t ->
                                                                'use_file ))
                };
               {
                 symbols =
                   [Nterm (Gramf.obj (stru : 'stru Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "si :: rest\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'use_file)  _  (si : 'stru) 
                         (_loc : Locf.t)  -> (si :: rest : 'use_file ) : 
                      'use_file -> Tokenf.txt -> 'stru -> Locf.t -> 'use_file ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "#"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "(`Directive (_loc, n, dp)) :: rest\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'use_file)  _  (dp : 'exp) 
                         (n : 'a_lident)  _  (_loc : Locf.t)  ->
                         ((`Directive (_loc, n, dp)) :: rest : 'use_file ) : 
                      'use_file ->
                        Tokenf.txt ->
                          'exp ->
                            'a_lident -> Tokenf.txt -> Locf.t -> 'use_file ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "#"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "(`DirectiveSimple (_loc, n)) :: rest\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'use_file)  _  (n : 'a_lident)  _ 
                         (_loc : Locf.t)  -> ((`DirectiveSimple (_loc, n)) ::
                         rest : 'use_file ) : 'use_file ->
                                                Tokenf.txt ->
                                                  'a_lident ->
                                                    Tokenf.txt ->
                                                      Locf.t -> 'use_file ))
               };
               {
                 symbols = [];
                 annot = "[]\n";
                 fn =
                   (Gramf.mk_action
                      (fun (_loc : Locf.t)  -> ([] : 'use_file ) : Locf.t ->
                                                                    'use_file ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (top_phrase : 'top_phrase Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "#"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                    Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A ";;"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "`Directive (_loc, n, dp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (dp : 'exp)  (n : 'a_lident)  _ 
                          (_loc : Locf.t)  ->
                          (`Directive (_loc, n, dp) : 'top_phrase ) : 
                       Tokenf.txt ->
                         'exp ->
                           'a_lident -> Tokenf.txt -> Locf.t -> 'top_phrase ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "#"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`DirectiveSimple (_loc, n)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (n : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`DirectiveSimple (_loc, n) : 'top_phrase ) : 
                      Tokenf.txt ->
                        'a_lident -> Tokenf.txt -> Locf.t -> 'top_phrase ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (stru : 'stru Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "st\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (st : 'stru)  (_loc : Locf.t)  ->
                         (st : 'top_phrase ) : Tokenf.txt ->
                                                 'stru ->
                                                   Locf.t -> 'top_phrase ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (strus : 'strus Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s : 
                            'strus ) : Tokenf.ant -> Locf.t -> 'strus ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "stri");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s : 
                           'strus ) : Tokenf.ant -> Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s : 
                           'strus ) : Tokenf.txt ->
                                        Tokenf.ant -> Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "stri");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s : 
                           'strus ) : Tokenf.txt ->
                                        Tokenf.ant -> Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'strus)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s),
                              st) : 'strus ) : 'strus ->
                                                 Tokenf.ant ->
                                                   Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "stri");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'strus)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s),
                              st) : 'strus ) : 'strus ->
                                                 Tokenf.ant ->
                                                   Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'strus)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s),
                              st) : 'strus ) : 'strus ->
                                                 Tokenf.txt ->
                                                   Tokenf.ant ->
                                                     Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "stri");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'strus)  _  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.stru) s),
                              st) : 'strus ) : 'strus ->
                                                 Tokenf.txt ->
                                                   Tokenf.ant ->
                                                     Locf.t -> 'strus ))
               };
               {
                 symbols = [Nterm (Gramf.obj (stru : 'stru Gramf.t ))];
                 annot = "st\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'stru)  (_loc : Locf.t)  -> (st : 'strus ) : 
                      'stru -> Locf.t -> 'strus ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (stru : 'stru Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "st\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (st : 'stru)  (_loc : Locf.t)  ->
                         (st : 'strus ) : Tokenf.txt ->
                                            'stru -> Locf.t -> 'strus ))
               };
               {
                 symbols = [Nterm (Gramf.obj (stru : 'stru Gramf.t )); Self];
                 annot = "`Sem (_loc, st, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'strus)  (st : 'stru)  (_loc : Locf.t)  ->
                         (`Sem (_loc, st, xs) : 'strus ) : 'strus ->
                                                             'stru ->
                                                               Locf.t ->
                                                                 'strus ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (stru : 'stru Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";;"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, st, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'strus)  _  (st : 'stru)  (_loc : Locf.t) 
                         -> (`Sem (_loc, st, xs) : 'strus ) : 'strus ->
                                                                Tokenf.txt ->
                                                                  'stru ->
                                                                    Locf.t ->
                                                                    'strus ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (stru_quot : 'stru_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "#"); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                    Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                  annot = "`Directive (_loc, n, dp)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (dp : 'exp)  (n : 'a_lident)  _  (_loc : Locf.t) 
                          -> (`Directive (_loc, n, dp) : 'stru_quot ) : 
                       'exp ->
                         'a_lident -> Tokenf.txt -> Locf.t -> 'stru_quot ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "#"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`DirectiveSimple (_loc, n)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (n : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`DirectiveSimple (_loc, n) : 'stru_quot ) : 
                      'a_lident -> Tokenf.txt -> Locf.t -> 'stru_quot ))
               };
               {
                 symbols = [Nterm (Gramf.obj (strus : 'strus Gramf.t ))];
                 annot = "x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'strus)  (_loc : Locf.t)  ->
                         (x : 'stru_quot ) : 'strus -> Locf.t -> 'stru_quot ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (stru : 'stru Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "include");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (mexp : 'mexp Gramf.t ))];
                  annot = "`Include (_loc, me)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (me : 'mexp)  _  (_loc : Locf.t)  ->
                          (`Include (_loc, me) : 'stru ) : 'mexp ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'stru ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ))];
                 annot = "`Module (_loc, i, mb)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mb : 'mbind0)  (i : 'a_uident)  _ 
                         (_loc : Locf.t)  -> (`Module (_loc, i, mb) : 
                         'stru ) : 'mbind0 ->
                                     'a_uident ->
                                       Tokenf.txt -> Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "rec"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mbind : 'mbind Gramf.t ))];
                 annot = "`RecModule (_loc, mb)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mb : 'mbind)  _  _  (_loc : Locf.t)  ->
                         (`RecModule (_loc, mb) : 'stru ) : 'mbind ->
                                                              Tokenf.txt ->
                                                                Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "open"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ))];
                 annot =
                   "`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  _  (_loc : Locf.t)  ->
                         let bang = None in
                         (`Open
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident)) : 'stru ) : 'module_longident
                                                                ->
                                                                Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "open"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ))];
                 annot =
                   "`Open\n  (_loc,\n    (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n    (i : vid  :>ident))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'module_longident)  (bang : Tokenf.txt)  _ 
                         (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (`Open
                            (_loc,
                              (match bang with
                               | Some _ -> `Positive _loc
                               | None  -> `Negative _loc),
                              (i : vid  :>ident)) : 'stru ) : 'module_longident
                                                                ->
                                                                Tokenf.txt ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_declaration : 'type_declaration Gramf.t ))];
                 annot = "`Type (_loc, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                         (`Type (_loc, t) : 'stru ) : 'type_declaration ->
                                                        Tokenf.txt ->
                                                          Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "module");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ))];
                 annot = "`ModuleType (_loc, i, mt)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (mt : 'mtyp)  _  (i : 'a_uident)  _  _ 
                         (_loc : Locf.t)  ->
                         (`ModuleType (_loc, i, mt) : 'stru ) : 'mtyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'a_uident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "class"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "type"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (cltyp_declaration : 'cltyp_declaration Gramf.t ))];
                 annot = "`ClassType (_loc, ctd)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ctd : 'cltyp_declaration)  _  _  (_loc : Locf.t) 
                         -> (`ClassType (_loc, ctd) : 'stru ) : 'cltyp_declaration
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "exception");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (constructor_declaration : 'constructor_declaration
                                                     Gramf.t ))];
                 annot = "`Exception (_loc, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'constructor_declaration)  _  (_loc : Locf.t)
                          -> (`Exception (_loc, t) : 'stru ) : 'constructor_declaration
                                                                 ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "external");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (string_list : 'string_list Gramf.t ))];
                 annot = "`External (_loc, i, t, sl)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (sl : 'string_list)  _  (t : 'ctyp)  _ 
                         (i : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`External (_loc, i, t, sl) : 'stru ) : 'string_list
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_lident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "type"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (type_declaration : 'type_declaration Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (string_list : 'string_list Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`TypeWith (_loc, t, ns)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (ns : 'string_list)  _  _ 
                         (t : 'type_declaration)  _  (_loc : Locf.t)  ->
                         (`TypeWith (_loc, t, ns) : 'stru ) : Tokenf.txt ->
                                                                'string_list
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'type_declaration
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(fun x  -> (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))\n  (`LetIn (_loc, r, bi, x))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'exp)  _  (bi : 'bind)  (r : 'opt_rec)  _ 
                         (_loc : Locf.t)  ->
                         ((fun x  ->
                             (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))
                            (`LetIn (_loc, r, bi, x)) : 'stru ) : 'exp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'bind ->
                                                                    'opt_rec
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "module"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                   Nterm (Gramf.obj (mbind0 : 'mbind0 Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(fun x  -> (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))\n  (`LetModule (_loc, m, mb, e))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (mb : 'mbind0)  (m : 'a_uident)  _ 
                         _  (_loc : Locf.t)  ->
                         ((fun x  ->
                             (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))
                            (`LetModule (_loc, m, mb, e)) : 'stru ) : 
                      'exp ->
                        Tokenf.txt ->
                          'mbind0 ->
                            'a_uident ->
                              Tokenf.txt -> Tokenf.txt -> Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(fun x  -> (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))\n  (`LetOpen\n     (_loc,\n       (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n       (i : vid  :>ident), e))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (i : 'module_longident)  _  _ 
                         (_loc : Locf.t)  ->
                         let bang = None in
                         ((fun x  ->
                             (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))
                            (`LetOpen
                               (_loc,
                                 (match bang with
                                  | Some _ -> `Positive _loc
                                  | None  -> `Negative _loc),
                                 (i : vid  :>ident), e)) : 'stru ) : 
                      'exp ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt -> Tokenf.txt -> Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "open"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (module_longident : 'module_longident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot =
                   "(fun x  -> (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))\n  (`LetOpen\n     (_loc,\n       (match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc),\n       (i : vid  :>ident), e))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  _  (i : 'module_longident) 
                         (bang : Tokenf.txt)  _  _  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         ((fun x  ->
                             (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))
                            (`LetOpen
                               (_loc,
                                 (match bang with
                                  | Some _ -> `Positive _loc
                                  | None  -> `Negative _loc),
                                 (i : vid  :>ident), e)) : 'stru ) : 
                      'exp ->
                        Tokenf.txt ->
                          'module_longident ->
                            Tokenf.txt ->
                              Tokenf.txt -> Tokenf.txt -> Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "try"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "with"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (case : 'case Gramf.t ))];
                 annot =
                   "(fun x  -> (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))\n  (`LetTryInWith (_loc, r, bi, x, a))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (a : 'case)  _  (x : 'exp)  _  (bi : 'bind) 
                         (r : 'opt_rec)  _  _  (_loc : Locf.t)  ->
                         ((fun x  ->
                             (`StExp (_loc, (x :>Astf.exp)) :>Astf.stru))
                            (`LetTryInWith (_loc, r, bi, x, a)) : 'stru ) : 
                      'case ->
                        Tokenf.txt ->
                          'exp ->
                            Tokenf.txt ->
                              'bind ->
                                'opt_rec ->
                                  Tokenf.txt -> Tokenf.txt -> Locf.t -> 'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ))];
                 annot =
                   "match bi with\n| `Bind (_loc,`Any _,e) -> `StExp (_loc, e)\n| _ -> `Value (_loc, r, bi)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (bi : 'bind)  (r : 'opt_rec)  _  (_loc : Locf.t) 
                         ->
                         (match bi with
                          | `Bind (_loc,`Any _,e) -> `StExp (_loc, e)
                          | _ -> `Value (_loc, r, bi) : 'stru ) : 'bind ->
                                                                    'opt_rec
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'stru ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "class"); tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (class_declaration : 'class_declaration Gramf.t ))];
                 annot = "`Class (_loc, cd)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (cd : 'class_declaration)  _  (_loc : Locf.t)  ->
                         (`Class (_loc, cd) : 'stru ) : 'class_declaration ->
                                                          Tokenf.txt ->
                                                            Locf.t -> 'stru ))
               };
               {
                 symbols = [Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "`StExp (_loc, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'exp)  (_loc : Locf.t)  ->
                         (`StExp (_loc, e) : 'stru ) : 'exp ->
                                                         Locf.t -> 'stru ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (class_signature : 'class_signature Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s : 
                            'class_signature ) : Tokenf.ant ->
                                                   Locf.t -> 'class_signature ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "csg");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s : 
                           'class_signature ) : Tokenf.ant ->
                                                  Locf.t -> 'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s : 
                           'class_signature ) : Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "csg");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s : 
                           'class_signature ) : Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s), csg) : \nAstf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (csg : 'class_signature)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Sem
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi)
                                  s), csg) : Astf.clsigi ) : 'class_signature ) : 
                      'class_signature ->
                        Tokenf.ant -> Locf.t -> 'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "csg");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s), csg) : \nAstf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (csg : 'class_signature)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Sem
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi)
                                  s), csg) : Astf.clsigi ) : 'class_signature ) : 
                      'class_signature ->
                        Tokenf.ant -> Locf.t -> 'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s), csg) : \nAstf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (csg : 'class_signature)  _ 
                         (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Sem
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi)
                                  s), csg) : Astf.clsigi ) : 'class_signature ) : 
                      'class_signature ->
                        Tokenf.txt ->
                          Tokenf.ant -> Locf.t -> 'class_signature ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "csg");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "(`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi) s), csg) : \nAstf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (csg : 'class_signature)  _ 
                         (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         ((`Sem
                             (_loc,
                               (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clsigi)
                                  s), csg) : Astf.clsigi ) : 'class_signature ) : 
                      'class_signature ->
                        Tokenf.txt ->
                          Tokenf.ant -> Locf.t -> 'class_signature ))
               };
               {
                 symbols = [Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ))];
                 annot = "csg\n";
                 fn =
                   (Gramf.mk_action
                      (fun (csg : 'clsigi)  (_loc : Locf.t)  ->
                         (csg : 'class_signature ) : 'clsigi ->
                                                       Locf.t ->
                                                         'class_signature ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "csg\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (csg : 'clsigi)  (_loc : Locf.t)  ->
                         (csg : 'class_signature ) : Tokenf.txt ->
                                                       'clsigi ->
                                                         Locf.t ->
                                                           'class_signature ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t )); Self];
                 annot = "`Sem (_loc, csg, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'class_signature)  (csg : 'clsigi) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, csg, xs) : 'class_signature ) : 
                      'class_signature ->
                        'clsigi -> Locf.t -> 'class_signature ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clsigi : 'clsigi Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, csg, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'class_signature)  _  (csg : 'clsigi) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, csg, xs) : 'class_signature ) : 
                      'class_signature ->
                        Tokenf.txt -> 'clsigi -> Locf.t -> 'class_signature ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (clsigi : 'clsigi Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "inherit");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))];
                  annot = "`SigInherit (_loc, cs)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (cs : 'cltyp)  _  (_loc : Locf.t)  ->
                          (`SigInherit (_loc, cs) : 'clsigi ) : 'cltyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clsigi ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
                   Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "(`CgVal (_loc, l, mf, mv, t) : Astf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (l : 'a_lident) 
                         (mv : 'opt_virtual)  (mf : 'opt_mutable)  _ 
                         (_loc : Locf.t)  ->
                         ((`CgVal (_loc, l, mf, mv, t) : Astf.clsigi ) : 
                         'clsigi ) : 'ctyp ->
                                       Tokenf.txt ->
                                         'a_lident ->
                                           'opt_virtual ->
                                             'opt_mutable ->
                                               Tokenf.txt ->
                                                 Locf.t -> 'clsigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "virtual");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "(`VirMeth (_loc, l, pf, t) : Astf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (l : 'a_lident) 
                         (pf : 'opt_private)  _  _  (_loc : Locf.t)  ->
                         ((`VirMeth (_loc, l, pf, t) : Astf.clsigi ) : 
                         'clsigi ) : 'ctyp ->
                                       Tokenf.txt ->
                                         'a_lident ->
                                           'opt_private ->
                                             Tokenf.txt ->
                                               Tokenf.txt ->
                                                 Locf.t -> 'clsigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "(`Method (_loc, l, pf, t) : Astf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (l : 'a_lident) 
                         (pf : 'opt_private)  _  (_loc : Locf.t)  ->
                         ((`Method (_loc, l, pf, t) : Astf.clsigi ) : 
                         'clsigi ) : 'ctyp ->
                                       Tokenf.txt ->
                                         'a_lident ->
                                           'opt_private ->
                                             Tokenf.txt -> Locf.t -> 'clsigi ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "constraint");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "(`Eq (_loc, t1, t2) : Astf.clsigi )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t) 
                         -> ((`Eq (_loc, t1, t2) : Astf.clsigi ) : 'clsigi ) : 
                      'ctyp ->
                        Tokenf.txt ->
                          'ctyp -> Tokenf.txt -> Locf.t -> 'clsigi ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (class_structure : 'class_structure Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Ant; word = (Kind ""); tag_name = "Ant"
                            }
                        } : Tokenf.pattern )];
                  annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s\n";
                  fn =
                    (Gramf.mk_action
                       (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                          let s = __fan_0 in
                          (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s : 
                            'class_structure ) : Tokenf.ant ->
                                                   Locf.t -> 'class_structure ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "cst");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s : 
                           'class_structure ) : Tokenf.ant ->
                                                  Locf.t -> 'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s : 
                           'class_structure ) : Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "cst");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s : 
                           'class_structure ) : Tokenf.txt ->
                                                  Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'class_structure)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield)
                                 s), st) : 'class_structure ) : 'class_structure
                                                                  ->
                                                                  Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "cst");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'class_structure)  (__fan_0 : Tokenf.ant) 
                         (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield)
                                 s), st) : 'class_structure ) : 'class_structure
                                                                  ->
                                                                  Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'class_structure)  _  (__fan_0 : Tokenf.ant)
                          (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield)
                                 s), st) : 'class_structure ) : 'class_structure
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'class_structure ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "cst");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot =
                   "`Sem (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield) s), st)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'class_structure)  _  (__fan_0 : Tokenf.ant)
                          (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (`Sem
                            (_loc,
                              (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.clfield)
                                 s), st) : 'class_structure ) : 'class_structure
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.ant
                                                                    ->
                                                                    Locf.t ->
                                                                    'class_structure ))
               };
               {
                 symbols = [Nterm (Gramf.obj (clfield : 'clfield Gramf.t ))];
                 annot = "st\n";
                 fn =
                   (Gramf.mk_action
                      (fun (st : 'clfield)  (_loc : Locf.t)  ->
                         (st : 'class_structure ) : 'clfield ->
                                                      Locf.t ->
                                                        'class_structure ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "st\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (st : 'clfield)  (_loc : Locf.t)  ->
                         (st : 'class_structure ) : Tokenf.txt ->
                                                      'clfield ->
                                                        Locf.t ->
                                                          'class_structure ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clfield : 'clfield Gramf.t )); Self];
                 annot = "`Sem (_loc, st, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'class_structure)  (st : 'clfield) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, st, xs) : 'class_structure ) : 
                      'class_structure ->
                        'clfield -> Locf.t -> 'class_structure ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (clfield : 'clfield Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, st, xs)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (xs : 'class_structure)  _  (st : 'clfield) 
                         (_loc : Locf.t)  ->
                         (`Sem (_loc, st, xs) : 'class_structure ) : 
                      'class_structure ->
                        Tokenf.txt -> 'clfield -> Locf.t -> 'class_structure ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (value_val_opt_override : 'value_val_opt_override Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "val"); tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot =
                    "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          let bang = None in
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc : 'value_val_opt_override ) : 
                       Tokenf.txt -> Locf.t -> 'value_val_opt_override ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (bang : Tokenf.txt)  _  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc : 'value_val_opt_override ) : 
                      Tokenf.txt ->
                        Tokenf.txt -> Locf.t -> 'value_val_opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.ant)  _  (_loc : Locf.t)  ->
                         let s = __fan_1 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'value_val_opt_override ) : Tokenf.ant ->
                                                         Tokenf.txt ->
                                                           Locf.t ->
                                                             'value_val_opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Ant;
                            word = (Kind "override");
                            tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.ant)  _  (_loc : Locf.t)  ->
                         let s = __fan_1 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'value_val_opt_override ) : Tokenf.ant ->
                                                         Tokenf.txt ->
                                                           Locf.t ->
                                                             'value_val_opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "!"); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.ant)  _  (_loc : Locf.t)  ->
                         let s = __fan_1 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'value_val_opt_override ) : Tokenf.ant ->
                                                         Tokenf.txt ->
                                                           Locf.t ->
                                                             'value_val_opt_override ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (method_opt_override : 'method_opt_override Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "method");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern )];
                  annot =
                    "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (_loc : Locf.t)  ->
                          let bang = None in
                          (match bang with
                           | Some _ -> `Positive _loc
                           | None  -> `Negative _loc : 'method_opt_override ) : 
                       Tokenf.txt -> Locf.t -> 'method_opt_override ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "!"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot =
                   "match bang with | Some _ -> `Positive _loc | None  -> `Negative _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun (bang : Tokenf.txt)  _  (_loc : Locf.t)  ->
                         let bang = Some bang in
                         (match bang with
                          | Some _ -> `Positive _loc
                          | None  -> `Negative _loc : 'method_opt_override ) : 
                      Tokenf.txt ->
                        Tokenf.txt -> Locf.t -> 'method_opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.ant)  _  (_loc : Locf.t)  ->
                         let s = __fan_1 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'method_opt_override ) : Tokenf.ant ->
                                                      Tokenf.txt ->
                                                        Locf.t ->
                                                          'method_opt_override ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Ant;
                            word = (Kind "override");
                            tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_1 : Tokenf.ant)  _  (_loc : Locf.t)  ->
                         let s = __fan_1 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.flag) s : 
                           'method_opt_override ) : Tokenf.ant ->
                                                      Tokenf.txt ->
                                                        Locf.t ->
                                                          'method_opt_override ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (clfield : 'clfield Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            {
                              tag = `Key;
                              word = (A "inherit");
                              tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
                    Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))];
                  annot = "`Inherit (_loc, o, ce)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (ce : 'clexp)  (o : 'opt_override)  _ 
                          (_loc : Locf.t)  ->
                          (`Inherit (_loc, o, ce) : 'clfield ) : 'clexp ->
                                                                   'opt_override
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clfield ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "inherit");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_override : 'opt_override Gramf.t ));
                   Nterm (Gramf.obj (clexp : 'clexp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "as"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`InheritAs (_loc, o, ce, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  (ce : 'clexp) 
                         (o : 'opt_override)  _  (_loc : Locf.t)  ->
                         (`InheritAs (_loc, o, ce, i) : 'clfield ) : 
                      'a_lident ->
                        Tokenf.txt ->
                          'clexp ->
                            'opt_override -> Tokenf.txt -> Locf.t -> 'clfield ))
               };
               {
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (value_val_opt_override : 'value_val_opt_override
                                                     Gramf.t ));
                   Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Nterm (Gramf.obj (cvalue_bind : 'cvalue_bind Gramf.t ))];
                 annot = "(`CrVal (_loc, lab, o, mf, e) : Astf.clfield )\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'cvalue_bind)  (lab : 'a_lident) 
                         (mf : 'opt_mutable)  (o : 'value_val_opt_override) 
                         (_loc : Locf.t)  ->
                         ((`CrVal (_loc, lab, o, mf, e) : Astf.clfield ) : 
                         'clfield ) : 'cvalue_bind ->
                                        'a_lident ->
                                          'opt_mutable ->
                                            'value_val_opt_override ->
                                              Locf.t -> 'clfield ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "val"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "virtual");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_mutable : 'opt_mutable Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`VirVal (_loc, l, mf, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (l : 'a_lident) 
                         (mf : 'opt_mutable)  _  _  (_loc : Locf.t)  ->
                         (`VirVal (_loc, l, mf, t) : 'clfield ) : 'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_lident
                                                                    ->
                                                                    'opt_mutable
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clfield ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "method");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "virtual");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`VirMeth (_loc, l, pf, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (l : 'a_lident) 
                         (pf : 'opt_private)  _  _  (_loc : Locf.t)  ->
                         (`VirMeth (_loc, l, pf, t) : 'clfield ) : 'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'a_lident
                                                                    ->
                                                                    'opt_private
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clfield ))
               };
               {
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (method_opt_override : 'method_opt_override Gramf.t ));
                   Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))];
                 annot = "`CrMth (_loc, l, o, pf, e, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_bind)  (t : 'ctyp)  _  (l : 'a_lident) 
                         (pf : 'opt_private)  (o : 'method_opt_override) 
                         (_loc : Locf.t)  ->
                         (`CrMth (_loc, l, o, pf, e, t) : 'clfield ) : 
                      'fun_bind ->
                        'ctyp ->
                          Tokenf.txt ->
                            'a_lident ->
                              'opt_private ->
                                'method_opt_override -> Locf.t -> 'clfield ))
               };
               {
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (method_opt_override : 'method_opt_override Gramf.t ));
                   Nterm (Gramf.obj (opt_private : 'opt_private Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Nterm (Gramf.obj (fun_bind : 'fun_bind Gramf.t ))];
                 annot = "`CrMthS (_loc, l, o, pf, e)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (e : 'fun_bind)  (l : 'a_lident) 
                         (pf : 'opt_private)  (o : 'method_opt_override) 
                         (_loc : Locf.t)  ->
                         (`CrMthS (_loc, l, o, pf, e) : 'clfield ) : 
                      'fun_bind ->
                        'a_lident ->
                          'opt_private ->
                            'method_opt_override -> Locf.t -> 'clfield ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "constraint");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`Eq (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t) 
                         -> (`Eq (_loc, t1, t2) : 'clfield ) : 'ctyp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clfield ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "initializer");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
                 annot = "`Initializer (_loc, se)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (se : 'exp)  _  (_loc : Locf.t)  ->
                         (`Initializer (_loc, se) : 'clfield ) : 'exp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clfield ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  (Gramf.extend_single
     ({
        entry = (clexp_quot : 'clexp_quot Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))];
                  annot = "x\n";
                  fn =
                    (Gramf.mk_action
                       (fun (x : 'clexp)  (_loc : Locf.t)  ->
                          (x : 'clexp_quot ) : 'clexp ->
                                                 Locf.t -> 'clexp_quot ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (class_declaration : 'class_declaration Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self;
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "and"); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Self];
                  annot = "`And (_loc, c1, c2)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (c2 : 'class_declaration)  _ 
                          (c1 : 'class_declaration)  (_loc : Locf.t)  ->
                          (`And (_loc, c1, c2) : 'class_declaration ) : 
                       'class_declaration ->
                         Tokenf.txt ->
                           'class_declaration -> Locf.t -> 'class_declaration ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cldecl) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cldecl) s : 
                           'class_declaration ) : Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_declaration ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Ant;
                             word = (Kind "cdcl");
                             tag_name = "Ant"
                           }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cldecl) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cldecl) s : 
                           'class_declaration ) : Tokenf.ant ->
                                                    Locf.t ->
                                                      'class_declaration ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (comma_type_parameter : 'comma_type_parameter Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))];
                 annot = "`ClDecl (_loc, mv, (i :>ident), x, ce)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'class_fun_bind)  _ 
                         (x : 'comma_type_parameter)  _  (i : 'a_lident) 
                         (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                         (`ClDecl (_loc, mv, (i :>ident), x, ce) : 'class_declaration ) : 
                      'class_fun_bind ->
                        Tokenf.txt ->
                          'comma_type_parameter ->
                            Tokenf.txt ->
                              'a_lident ->
                                'opt_virtual -> Locf.t -> 'class_declaration ))
               };
               {
                 symbols =
                   [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Nterm
                     (Gramf.obj (class_fun_bind : 'class_fun_bind Gramf.t ))];
                 annot = "`ClDeclS (_loc, mv, (i :>ident), ce)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'class_fun_bind)  (i : 'a_lident) 
                         (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                         (`ClDeclS (_loc, mv, (i :>ident), ce) : 'class_declaration ) : 
                      'class_fun_bind ->
                        'a_lident ->
                          'opt_virtual -> Locf.t -> 'class_declaration ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (class_fun_bind : 'class_fun_bind Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "="); tag_name = "Key" }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))];
                  annot = "ce\n";
                  fn =
                    (Gramf.mk_action
                       (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                          (ce : 'class_fun_bind ) : 'clexp ->
                                                      Tokenf.txt ->
                                                        Locf.t ->
                                                          'class_fun_bind ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A ":"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))];
                 annot = "`Constraint (_loc, ce, ct)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'clexp)  _  (ct : 'cltyp_plus)  _ 
                         (_loc : Locf.t)  ->
                         (`Constraint (_loc, ce, ct) : 'class_fun_bind ) : 
                      'clexp ->
                        Tokenf.txt ->
                          'cltyp_plus ->
                            Tokenf.txt -> Locf.t -> 'class_fun_bind ))
               };
               {
                 symbols = [Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); Self];
                 annot = "`CeFun (_loc, p, cfb)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (cfb : 'class_fun_bind)  (p : 'ipat) 
                         (_loc : Locf.t)  ->
                         (`CeFun (_loc, p, cfb) : 'class_fun_bind ) : 
                      'class_fun_bind -> 'ipat -> Locf.t -> 'class_fun_bind ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (class_fun_def : 'class_fun_def Gramf.t );
        olevel =
          ({
             label = None;
             lassoc = true;
             productions =
               [{
                  symbols = [Nterm (Gramf.obj (ipat : 'ipat Gramf.t )); Self];
                  annot = "`CeFun (_loc, p, ce)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (ce : 'class_fun_def)  (p : 'ipat) 
                          (_loc : Locf.t)  ->
                          (`CeFun (_loc, p, ce) : 'class_fun_def ) : 
                       'class_fun_def -> 'ipat -> Locf.t -> 'class_fun_def ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "->"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (clexp : 'clexp Gramf.t ))];
                 annot = "ce\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'clexp)  _  (_loc : Locf.t)  ->
                         (ce : 'class_fun_def ) : 'clexp ->
                                                    Tokenf.txt ->
                                                      Locf.t ->
                                                        'class_fun_def ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (clexp : 'clexp Gramf.t );
        olevel =
          ({
             label = (Some 10);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Token
                       ({
                          descr =
                            { tag = `Key; word = (A "fun"); tag_name = "Key"
                            }
                        } : Tokenf.pattern );
                    Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
                    Nterm
                      (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))];
                  annot = "`CeFun (_loc, p, ce)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (ce : 'class_fun_def)  (p : 'ipat)  _ 
                          (_loc : Locf.t)  ->
                          (`CeFun (_loc, p, ce) : 'clexp ) : 'class_fun_def
                                                               ->
                                                               'ipat ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   Locf.t ->
                                                                    'clexp ))
                };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "function");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ipat : 'ipat Gramf.t ));
                   Nterm
                     (Gramf.obj (class_fun_def : 'class_fun_def Gramf.t ))];
                 annot = "`CeFun (_loc, p, ce)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'class_fun_def)  (p : 'ipat)  _ 
                         (_loc : Locf.t)  -> (`CeFun (_loc, p, ce) : 
                         'clexp ) : 'class_fun_def ->
                                      'ipat -> Tokenf.txt -> Locf.t -> 'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "let"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (opt_rec : 'opt_rec Gramf.t ));
                   Nterm (Gramf.obj (bind : 'bind Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "in"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`LetIn (_loc, rf, bi, ce)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ce : 'clexp)  _  (bi : 'bind)  (rf : 'opt_rec)  _
                          (_loc : Locf.t)  ->
                         (`LetIn (_loc, rf, bi, ce) : 'clexp ) : 'clexp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'bind ->
                                                                    'opt_rec
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clexp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (clexp : 'clexp Gramf.t );
        olevel =
          ({
             label = (Some 20);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Self; Snterml ((Gramf.obj (exp : 'exp Gramf.t )), 140)];
                  annot = "`CeApp (_loc, ce, e)\n";
                  fn =
                    (Gramf.mk_action
                       (fun (e : 'exp)  (ce : 'clexp)  (_loc : Locf.t)  ->
                          (`CeApp (_loc, ce, e) : 'clexp ) : 'exp ->
                                                               'clexp ->
                                                                 Locf.t ->
                                                                   'clexp ))
                }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement );
   Gramf.extend_single
     ({
        entry = (clexp : 'clexp Gramf.t );
        olevel =
          ({
             label = (Some 30);
             lassoc = true;
             productions =
               [{
                  symbols =
                    [Nterm (Gramf.obj (vid : 'vid Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "["); tag_name = "Key" }
                       } : Tokenf.pattern );
                    Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
                    Token
                      ({
                         descr =
                           { tag = `Key; word = (A "]"); tag_name = "Key" }
                       } : Tokenf.pattern )];
                  annot = "`ClApply (_loc, ci, t)\n";
                  fn =
                    (Gramf.mk_action
                       (fun _  (t : 'comma_ctyp)  _  (ci : 'vid) 
                          (_loc : Locf.t)  ->
                          (`ClApply (_loc, ci, t) : 'clexp ) : Tokenf.txt ->
                                                                 'comma_ctyp
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'vid ->
                                                                    Locf.t ->
                                                                    'clexp ))
                };
               {
                 symbols = [Nterm (Gramf.obj (vid : 'vid Gramf.t ))];
                 annot = "(ci :>clexp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ci : 'vid)  (_loc : Locf.t)  ->
                         ((ci :>clexp) : 'clexp ) : 'vid -> Locf.t -> 'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ObjPat (_loc, p, cst)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (p : 'pat)  _  _ 
                         (_loc : Locf.t)  ->
                         (`ObjPat (_loc, p, cst) : 'clexp ) : Tokenf.txt ->
                                                                'class_structure
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'pat ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ObjPatEnd (_loc, p)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (p : 'pat)  _  _  (_loc : Locf.t)  ->
                         (`ObjPatEnd (_loc, p) : 'clexp ) : Tokenf.txt ->
                                                              Tokenf.txt ->
                                                                'pat ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ObjPat (_loc, (`Constraint (_loc, p, t)), cst)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (t : 'ctyp)  _ 
                         (p : 'pat)  _  _  (_loc : Locf.t)  ->
                         (`ObjPat (_loc, (`Constraint (_loc, p, t)), cst) : 
                         'clexp ) : Tokenf.txt ->
                                      'class_structure ->
                                        Tokenf.txt ->
                                          'ctyp ->
                                            Tokenf.txt ->
                                              'pat ->
                                                Tokenf.txt ->
                                                  Tokenf.txt ->
                                                    Locf.t -> 'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (pat : 'pat Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ObjPatEnd (_loc, (`Constraint (_loc, p, t)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (t : 'ctyp)  _  (p : 'pat)  _  _ 
                         (_loc : Locf.t)  ->
                         (`ObjPatEnd (_loc, (`Constraint (_loc, p, t))) : 
                         'clexp ) : Tokenf.txt ->
                                      Tokenf.txt ->
                                        'ctyp ->
                                          Tokenf.txt ->
                                            'pat ->
                                              Tokenf.txt ->
                                                Tokenf.txt ->
                                                  Locf.t -> 'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (class_structure : 'class_structure Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Obj (_loc, cst)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (cst : 'class_structure)  _  (_loc : Locf.t) 
                         -> (`Obj (_loc, cst) : 'clexp ) : Tokenf.txt ->
                                                             'class_structure
                                                               ->
                                                               Tokenf.txt ->
                                                                 Locf.t ->
                                                                   'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "object");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "end"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ObjEnd _loc\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'clexp ) : 
                      Tokenf.txt -> Tokenf.txt -> Locf.t -> 'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`Constraint (_loc, ce, ct)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (ct : 'cltyp)  _  (ce : 'clexp)  _ 
                         (_loc : Locf.t)  ->
                         (`Constraint (_loc, ce, ct) : 'clexp ) : Tokenf.txt
                                                                    ->
                                                                    'cltyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'clexp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'clexp ))
               };
               {
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "ce\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (ce : 'clexp)  _  (_loc : Locf.t)  ->
                         (ce : 'clexp ) : Tokenf.txt ->
                                            'clexp ->
                                              Tokenf.txt -> Locf.t -> 'clexp ))
               }]
           } : Gramf.olevel )
      } : _ Gramf.single_extend_statement ));
  Gramf.extend_single
    ({
       entry = (class_description : 'class_description Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "and"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`And (_loc, cd1, cd2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (cd2 : 'class_description)  _ 
                         (cd1 : 'class_description)  (_loc : Locf.t)  ->
                         (`And (_loc, cd1, cd2) : 'class_description ) : 
                      'class_description ->
                        Tokenf.txt ->
                          'class_description -> Locf.t -> 'class_description ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s : 
                          'class_description ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'class_description ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s : 
                          'class_description ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'class_description ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "["); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (comma_type_parameter : 'comma_type_parameter Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))];
                annot = "`CtDecl (_loc, mv, (i :>ident), x, ct)\n";
                fn =
                  (Gramf.mk_action
                     (fun (ct : 'cltyp_plus)  _  _ 
                        (x : 'comma_type_parameter)  _  (i : 'a_lident) 
                        (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                        (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'class_description ) : 
                     'cltyp_plus ->
                       Tokenf.txt ->
                         Tokenf.txt ->
                           'comma_type_parameter ->
                             Tokenf.txt ->
                               'a_lident ->
                                 'opt_virtual -> Locf.t -> 'class_description ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (cltyp_plus : 'cltyp_plus Gramf.t ))];
                annot = "`CtDeclS (_loc, mv, (i :>ident), ct)\n";
                fn =
                  (Gramf.mk_action
                     (fun (ct : 'cltyp_plus)  _  (i : 'a_lident) 
                        (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                        (`CtDeclS (_loc, mv, (i :>ident), ct) : 'class_description ) : 
                     'cltyp_plus ->
                       Tokenf.txt ->
                         'a_lident ->
                           'opt_virtual -> Locf.t -> 'class_description ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (cltyp_declaration : 'cltyp_declaration Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "and"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`And (_loc, cd1, cd2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (cd2 : 'cltyp_declaration)  _ 
                         (cd1 : 'cltyp_declaration)  (_loc : Locf.t)  ->
                         (`And (_loc, cd1, cd2) : 'cltyp_declaration ) : 
                      'cltyp_declaration ->
                        Tokenf.txt ->
                          'cltyp_declaration -> Locf.t -> 'cltyp_declaration ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s : 
                          'cltyp_declaration ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'cltyp_declaration ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.cltdecl) s : 
                          'cltyp_declaration ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'cltyp_declaration ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "["); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (comma_type_parameter : 'comma_type_parameter Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))];
                annot = "`CtDecl (_loc, mv, (i :>ident), x, ct)\n";
                fn =
                  (Gramf.mk_action
                     (fun (ct : 'cltyp)  _  _  (x : 'comma_type_parameter)  _
                         (i : 'a_lident)  (mv : 'opt_virtual) 
                        (_loc : Locf.t)  ->
                        (`CtDecl (_loc, mv, (i :>ident), x, ct) : 'cltyp_declaration ) : 
                     'cltyp ->
                       Tokenf.txt ->
                         Tokenf.txt ->
                           'comma_type_parameter ->
                             Tokenf.txt ->
                               'a_lident ->
                                 'opt_virtual -> Locf.t -> 'cltyp_declaration ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (opt_virtual : 'opt_virtual Gramf.t ));
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))];
                annot = "`CtDeclS (_loc, mv, (i :>ident), ct)\n";
                fn =
                  (Gramf.mk_action
                     (fun (ct : 'cltyp)  _  (i : 'a_lident) 
                        (mv : 'opt_virtual)  (_loc : Locf.t)  ->
                        (`CtDeclS (_loc, mv, (i :>ident), ct) : 'cltyp_declaration ) : 
                     'cltyp ->
                       Tokenf.txt ->
                         'a_lident ->
                           'opt_virtual -> Locf.t -> 'cltyp_declaration ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (cltyp_quot : 'cltyp_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))];
                 annot = "x\n";
                 fn =
                   (Gramf.mk_action
                      (fun (x : 'cltyp)  (_loc : Locf.t)  ->
                         (x : 'cltyp_quot ) : 'cltyp -> Locf.t -> 'cltyp_quot ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (cltyp_plus : 'cltyp_plus Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "["); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "->"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`CtFun (_loc, t, ct)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (ct : 'cltyp_plus)  _  _  (t : 'ctyp)  _ 
                         (_loc : Locf.t)  ->
                         (`CtFun (_loc, t, ct) : 'cltyp_plus ) : 'cltyp_plus
                                                                   ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'cltyp_plus ))
               };
              {
                symbols = [Nterm (Gramf.obj (cltyp : 'cltyp Gramf.t ))];
                annot = "ct\n";
                fn =
                  (Gramf.mk_action
                     (fun (ct : 'cltyp)  (_loc : Locf.t)  ->
                        (ct : 'cltyp_plus ) : 'cltyp -> Locf.t -> 'cltyp_plus ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (cltyp : 'cltyp Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (vid : 'vid Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (comma_ctyp : 'comma_ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "]"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                 annot = "`ClApply (_loc, i, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun _  (t : 'comma_ctyp)  _  (i : 'vid) 
                         (_loc : Locf.t)  ->
                         (`ClApply (_loc, i, t) : 'cltyp ) : Tokenf.txt ->
                                                               'comma_ctyp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'vid ->
                                                                    Locf.t ->
                                                                    'cltyp ))
               };
              {
                symbols = [Nterm (Gramf.obj (vid : 'vid Gramf.t ))];
                annot = "(i :>cltyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'vid)  (_loc : Locf.t)  ->
                        ((i :>cltyp) : 'cltyp ) : 'vid -> Locf.t -> 'cltyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "object"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (class_signature : 'class_signature Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "end"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ObjTy (_loc, t, csg)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (csg : 'class_signature)  _  (t : 'ctyp)  _  _ 
                        (_loc : Locf.t)  -> (`ObjTy (_loc, t, csg) : 
                        'cltyp ) : Tokenf.txt ->
                                     'class_signature ->
                                       Tokenf.txt ->
                                         'ctyp ->
                                           Tokenf.txt ->
                                             Tokenf.txt -> Locf.t -> 'cltyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "object"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (class_signature : 'class_signature Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "end"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Obj (_loc, csg)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (csg : 'class_signature)  _  (_loc : Locf.t)  ->
                        (`Obj (_loc, csg) : 'cltyp ) : Tokenf.txt ->
                                                         'class_signature ->
                                                           Tokenf.txt ->
                                                             Locf.t -> 'cltyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "object"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "("); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ObjTyEnd (_loc, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'ctyp)  _  _  (_loc : Locf.t)  ->
                        (`ObjTyEnd (_loc, t) : 'cltyp ) : Tokenf.txt ->
                                                            'ctyp ->
                                                              Tokenf.txt ->
                                                                Tokenf.txt ->
                                                                  Locf.t ->
                                                                    'cltyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "object"); tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "end"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`ObjEnd _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  _  (_loc : Locf.t)  -> (`ObjEnd _loc : 'cltyp ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'cltyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let apply_ctyp () =
  Gramf.extend_single
    ({
       entry = (ctyp_quot : 'ctyp_quot Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "*"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ))];
                 annot = "`Sta (_loc, x, y)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (y : 'star_ctyp)  _  (x : 'ctyp)  (_loc : Locf.t) 
                         -> (`Sta (_loc, x, y) : 'ctyp_quot ) : 'star_ctyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Locf.t ->
                                                                    'ctyp_quot ))
               };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "x\n";
                fn =
                  (Gramf.mk_action
                     (fun (x : 'ctyp)  (_loc : Locf.t)  -> (x : 'ctyp_quot ) : 
                     'ctyp -> Locf.t -> 'ctyp_quot ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (unquoted_typevars : 'unquoted_typevars Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [Self; Self];
                 annot = "`App (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'unquoted_typevars) 
                         (t1 : 'unquoted_typevars)  (_loc : Locf.t)  ->
                         (`App (_loc, t1, t2) : 'unquoted_typevars ) : 
                      'unquoted_typevars ->
                        'unquoted_typevars -> Locf.t -> 'unquoted_typevars ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'unquoted_typevars ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'unquoted_typevars ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'unquoted_typevars ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'unquoted_typevars ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(i :>ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                        ((i :>ctyp) : 'unquoted_typevars ) : 'a_lident ->
                                                               Locf.t ->
                                                                 'unquoted_typevars ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (type_parameter : 'type_parameter Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in (mk_ant s : 'type_parameter ) : 
                      Tokenf.ant -> Locf.t -> 'type_parameter ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in (mk_ant s : 'type_parameter ) : 
                     Tokenf.ant -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "'"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`Quote (_loc, (`Normal _loc), i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`Quote (_loc, (`Normal _loc), i) : 'type_parameter ) : 
                     'a_lident -> Tokenf.txt -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "+"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "'"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot =
                  "`Quote (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc), i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let p = __fan_0.txt in
                        (`Quote
                           (_loc,
                             (if p = "+"
                              then `Positive _loc
                              else `Negative _loc), i) : 'type_parameter ) : 
                     'a_lident ->
                       Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "'"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot =
                  "`Quote (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc), i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let p = __fan_0.txt in
                        (`Quote
                           (_loc,
                             (if p = "+"
                              then `Positive _loc
                              else `Negative _loc), i) : 'type_parameter ) : 
                     'a_lident ->
                       Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "+"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "_"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let p = __fan_0.txt in
                        (`QuoteAny
                           (_loc,
                             (if p = "+"
                              then `Positive _loc
                              else `Negative _loc)) : 'type_parameter ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "-"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "_"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "`QuoteAny (_loc, (if p = \"+\" then `Positive _loc else `Negative _loc))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                        let p = __fan_0.txt in
                        (`QuoteAny
                           (_loc,
                             (if p = "+"
                              then `Positive _loc
                              else `Negative _loc)) : 'type_parameter ) : 
                     Tokenf.txt -> Tokenf.txt -> Locf.t -> 'type_parameter ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "_"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Any _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  ->
                        (`Any _loc : 'type_parameter ) : Tokenf.txt ->
                                                           Locf.t ->
                                                             'type_parameter ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry =
         (type_longident_and_parameters : 'type_longident_and_parameters
                                            Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (type_parameters : 'type_parameters Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                 annot = "tpl (i :>ctyp)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'type_longident)  _  (tpl : 'type_parameters)
                          _  (_loc : Locf.t)  ->
                         (tpl (i :>ctyp) : 'type_longident_and_parameters ) : 
                      'type_longident ->
                        Tokenf.txt ->
                          'type_parameters ->
                            Tokenf.txt ->
                              Locf.t -> 'type_longident_and_parameters ))
               };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
                  Nterm
                    (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                annot = "`App (_loc, (i :>ctyp), (tpl :>ctyp))\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'type_longident)  (tpl : 'type_parameter) 
                        (_loc : Locf.t)  ->
                        (`App (_loc, (i :>ctyp), (tpl :>ctyp)) : 'type_longident_and_parameters ) : 
                     'type_longident ->
                       'type_parameter ->
                         Locf.t -> 'type_longident_and_parameters ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                annot = "(i :>ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'type_longident)  (_loc : Locf.t)  ->
                        ((i :>ctyp) : 'type_longident_and_parameters ) : 
                     'type_longident ->
                       Locf.t -> 'type_longident_and_parameters ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant s ~c:(Dyn_tag.to_string Dyn_tag.ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant s ~c:(Dyn_tag.to_string Dyn_tag.ctyp) : 
                          'type_longident_and_parameters ) : Tokenf.ant ->
                                                               Locf.t ->
                                                                 'type_longident_and_parameters ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (type_parameters : 'type_parameters Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm
                      (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
                   Self];
                 annot = "fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp)))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'type_parameters)  (t1 : 'type_parameter) 
                         (_loc : Locf.t)  ->
                         (fun acc  -> t2 (`App (_loc, acc, (t1 :>ctyp))) : 
                         'type_parameters ) : 'type_parameters ->
                                                'type_parameter ->
                                                  Locf.t -> 'type_parameters ))
               };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))];
                annot = "fun acc  -> `App (_loc, acc, (t :>ctyp))\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                        (fun acc  -> `App (_loc, acc, (t :>ctyp)) : 'type_parameters ) : 
                     'type_parameter -> Locf.t -> 'type_parameters ))
              };
              {
                symbols = [];
                annot = "fun t  -> t\n";
                fn =
                  (Gramf.mk_action
                     (fun (_loc : Locf.t)  ->
                        (fun t  -> t : 'type_parameters ) : Locf.t ->
                                                              'type_parameters ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (meth_list : 'meth_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "let (ml,v) = rest in ((`Sem (_loc, m, ml)), v)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'meth_list)  _  (m : 'meth_decl) 
                         (_loc : Locf.t)  ->
                         (let (ml,v) = rest in ((`Sem (_loc, m, ml)), v) : 
                         'meth_list ) : 'meth_list ->
                                          Tokenf.txt ->
                                            'meth_decl ->
                                              Locf.t -> 'meth_list ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
                  Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))];
                annot = "(m, v)\n";
                fn =
                  (Gramf.mk_action
                     (fun (v : 'opt_dot_dot)  (m : 'meth_decl) 
                        (_loc : Locf.t)  -> ((m, v) : 'meth_list ) : 
                     'opt_dot_dot -> 'meth_decl -> Locf.t -> 'meth_list ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (meth_decl : 'meth_decl Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))];
                annot = "(m, v)\n";
                fn =
                  (Gramf.mk_action
                     (fun (v : 'opt_dot_dot)  _  (m : 'meth_decl) 
                        (_loc : Locf.t)  -> ((m, v) : 'meth_list ) : 
                     'opt_dot_dot ->
                       Tokenf.txt -> 'meth_decl -> Locf.t -> 'meth_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (meth_decl : 'meth_decl Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`TyCol (_loc, lab, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (lab : 'a_lident)  (_loc : Locf.t)
                          -> (`TyCol (_loc, lab, t) : 'meth_decl ) : 
                      'ctyp ->
                        Tokenf.txt -> 'a_lident -> Locf.t -> 'meth_decl ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (opt_meth_list : 'opt_meth_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (meth_list : 'meth_list Gramf.t ))];
                 annot = "let (ml,v) = rest in `TyObj (_loc, ml, v)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (rest : 'meth_list)  (_loc : Locf.t)  ->
                         (let (ml,v) = rest in `TyObj (_loc, ml, v) : 
                         'opt_meth_list ) : 'meth_list ->
                                              Locf.t -> 'opt_meth_list ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (opt_dot_dot : 'opt_dot_dot Gramf.t ))];
                annot = "`TyObjEnd (_loc, v)\n";
                fn =
                  (Gramf.mk_action
                     (fun (v : 'opt_dot_dot)  (_loc : Locf.t)  ->
                        (`TyObjEnd (_loc, v) : 'opt_meth_list ) : 'opt_dot_dot
                                                                    ->
                                                                    Locf.t ->
                                                                    'opt_meth_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (row_field : 'row_field Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot =
                   "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.row_field) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.row_field) s : 
                           'row_field ) : Tokenf.ant -> Locf.t -> 'row_field ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.row_field) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.row_field) s : 
                          'row_field ) : Tokenf.ant -> Locf.t -> 'row_field ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot =
                  "`TyVrn (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.astring) s))\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (`TyVrn
                           (_loc,
                             (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.astring) s)) : 
                          'row_field ) : Tokenf.ant -> Locf.t -> 'row_field ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "vrn"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "of"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot =
                  "`TyVrnOf (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.astring) s), t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.ant) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (`TyVrnOf
                           (_loc,
                             (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.astring) s),
                             t) : 'row_field ) : 'ctyp ->
                                                   Tokenf.txt ->
                                                     Tokenf.ant ->
                                                       Locf.t -> 'row_field ))
              };
              {
                symbols =
                  [Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Bar (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'row_field)  _  (t1 : 'row_field) 
                        (_loc : Locf.t)  ->
                        (`Bar (_loc, t1, t2) : 'row_field ) : 'row_field ->
                                                                Tokenf.txt ->
                                                                  'row_field
                                                                    ->
                                                                    Locf.t ->
                                                                    'row_field ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (astr : 'astr Gramf.t ))];
                annot = "`TyVrn (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                        (`TyVrn (_loc, i) : 'row_field ) : 'astr ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'row_field ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (astr : 'astr Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "of"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyVrnOf (_loc, i, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (i : 'astr)  _  (_loc : Locf.t)  ->
                        (`TyVrnOf (_loc, i, t) : 'row_field ) : 'ctyp ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'astr ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'row_field ))
              };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`Ctyp (_loc, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                        (`Ctyp (_loc, t) : 'row_field ) : 'ctyp ->
                                                            Locf.t ->
                                                              'row_field ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (name_tags : 'name_tags Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot =
                   "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.tag_names) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.tag_names) s : 
                           'name_tags ) : Tokenf.ant -> Locf.t -> 'name_tags ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.tag_names) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.tag_names) s : 
                          'name_tags ) : Tokenf.ant -> Locf.t -> 'name_tags ))
              };
              {
                symbols = [Self; Self];
                annot = "`App (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'name_tags)  (t1 : 'name_tags) 
                        (_loc : Locf.t)  ->
                        (`App (_loc, t1, t2) : 'name_tags ) : 'name_tags ->
                                                                'name_tags ->
                                                                  Locf.t ->
                                                                    'name_tags ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "`"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (astr : 'astr Gramf.t ))];
                annot = "`TyVrn (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'astr)  _  (_loc : Locf.t)  ->
                        (`TyVrn (_loc, i) : 'name_tags ) : 'astr ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'name_tags ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry =
         (type_ident_and_parameters : 'type_ident_and_parameters Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "("); tag_name = "Key" }
                       } : Tokenf.pattern );
                   List1sep
                     ((Nterm
                         (Gramf.obj
                            (type_parameter : 'type_parameter Gramf.t ))),
                       (Token
                          ({
                             descr =
                               { tag = `Key; word = (A ","); tag_name = "Key"
                               }
                           } : Tokenf.pattern )));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ")"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot =
                   "(i, (`Some (_loc, (com_of_list (tpl :>decl_params list)))))\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  (tpl : 'type_parameter list) 
                         _  (_loc : Locf.t)  ->
                         ((i,
                            (`Some
                               (_loc, (com_of_list (tpl :>decl_params list))))) : 
                         'type_ident_and_parameters ) : 'a_lident ->
                                                          Tokenf.txt ->
                                                            'type_parameter
                                                              list ->
                                                              Tokenf.txt ->
                                                                Locf.t ->
                                                                  'type_ident_and_parameters ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Ant;
                            word = (Kind "param");
                            tag_name = "Ant"
                          }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot =
                  "(i, (`Some ((s.loc), (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.decl_params) s))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (__fan_0 : Tokenf.ant) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        ((i,
                           (`Some
                              ((s.loc),
                                (mk_ant
                                   ~c:(Dyn_tag.to_string Dyn_tag.decl_params)
                                   s)))) : 'type_ident_and_parameters ) : 
                     'a_lident ->
                       Tokenf.ant -> Locf.t -> 'type_ident_and_parameters ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (type_parameter : 'type_parameter Gramf.t ));
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(i, (`Some (_loc, (t :>decl_params))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (t : 'type_parameter) 
                        (_loc : Locf.t)  ->
                        ((i, (`Some (_loc, (t :>decl_params)))) : 'type_ident_and_parameters ) : 
                     'a_lident ->
                       'type_parameter ->
                         Locf.t -> 'type_ident_and_parameters ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(i, (`None _loc))\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                        ((i, (`None _loc)) : 'type_ident_and_parameters ) : 
                     'a_lident -> Locf.t -> 'type_ident_and_parameters ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (type_declaration : 'type_declaration Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.typedecl) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.typedecl) s : 
                           'type_declaration ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'type_declaration ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.typedecl) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.typedecl) s : 
                          'type_declaration ) : Tokenf.ant ->
                                                  Locf.t -> 'type_declaration ))
              };
              {
                symbols =
                  [Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "and"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`And (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'type_declaration)  _ 
                        (t1 : 'type_declaration)  (_loc : Locf.t)  ->
                        (`And (_loc, t1, t2) : 'type_declaration ) : 
                     'type_declaration ->
                       Tokenf.txt ->
                         'type_declaration -> Locf.t -> 'type_declaration ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj
                        (type_ident_and_parameters : 'type_ident_and_parameters
                                                       Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (type_info : 'type_info Gramf.t ));
                  List0 (Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))];
                annot =
                  "let (n,tpl) = rest in\n`TyDcl\n  (_loc, n, tpl, tk,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (cl : 'constrain list)  (tk : 'type_info)  _ 
                        (rest : 'type_ident_and_parameters)  (_loc : Locf.t) 
                        ->
                        (let (n,tpl) = rest in
                         `TyDcl
                           (_loc, n, tpl, tk,
                             (match cl with
                              | [] -> `None _loc
                              | _ -> `Some (_loc, (and_of_list cl)))) : 
                        'type_declaration ) : 'constrain list ->
                                                'type_info ->
                                                  Tokenf.txt ->
                                                    'type_ident_and_parameters
                                                      ->
                                                      Locf.t ->
                                                        'type_declaration ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj
                        (type_ident_and_parameters : 'type_ident_and_parameters
                                                       Gramf.t ));
                  List0 (Nterm (Gramf.obj (constrain : 'constrain Gramf.t )))];
                annot =
                  "let (n,tpl) = rest in\n`TyAbstr\n  (_loc, n, tpl,\n    (match cl with | [] -> `None _loc | _ -> `Some (_loc, (and_of_list cl))))\n";
                fn =
                  (Gramf.mk_action
                     (fun (cl : 'constrain list) 
                        (rest : 'type_ident_and_parameters)  (_loc : Locf.t) 
                        ->
                        (let (n,tpl) = rest in
                         `TyAbstr
                           (_loc, n, tpl,
                             (match cl with
                              | [] -> `None _loc
                              | _ -> `Some (_loc, (and_of_list cl)))) : 
                        'type_declaration ) : 'constrain list ->
                                                'type_ident_and_parameters ->
                                                  Locf.t -> 'type_declaration ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (type_info : 'type_info Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))];
                 annot = "`TyRepr (_loc, (`Negative _loc), t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'type_repr)  (_loc : Locf.t)  ->
                         (`TyRepr (_loc, (`Negative _loc), t2) : 'type_info ) : 
                      'type_repr -> Locf.t -> 'type_info ))
               };
              {
                symbols =
                  [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))];
                annot = "`TyMan (_loc, t1, (`Negative _loc), t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'type_repr)  _  (t1 : 'ctyp)  (_loc : Locf.t)
                         ->
                        (`TyMan (_loc, t1, (`Negative _loc), t2) : 'type_info ) : 
                     'type_repr ->
                       Tokenf.txt -> 'ctyp -> Locf.t -> 'type_info ))
              };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyEq (_loc, (`Negative _loc), t1)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t1 : 'ctyp)  (_loc : Locf.t)  ->
                        (`TyEq (_loc, (`Negative _loc), t1) : 'type_info ) : 
                     'ctyp -> Locf.t -> 'type_info ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "private");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyEq (_loc, (`Positive _loc), t1)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t1 : 'ctyp)  _  (_loc : Locf.t)  ->
                        (`TyEq (_loc, (`Positive _loc), t1) : 'type_info ) : 
                     'ctyp -> Tokenf.txt -> Locf.t -> 'type_info ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "="); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "private"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))];
                annot = "`TyMan (_loc, t1, (`Positive _loc), t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'type_repr)  _  _  (t1 : 'ctyp) 
                        (_loc : Locf.t)  ->
                        (`TyMan (_loc, t1, (`Positive _loc), t2) : 'type_info ) : 
                     'type_repr ->
                       Tokenf.txt ->
                         Tokenf.txt -> 'ctyp -> Locf.t -> 'type_info ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "private");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (type_repr : 'type_repr Gramf.t ))];
                annot = "`TyRepr (_loc, (`Positive _loc), t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'type_repr)  _  (_loc : Locf.t)  ->
                        (`TyRepr (_loc, (`Positive _loc), t2) : 'type_info ) : 
                     'type_repr -> Tokenf.txt -> Locf.t -> 'type_info ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (type_repr : 'type_repr Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "|"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm
                     (Gramf.obj
                        (constructor_declarations : 'constructor_declarations
                                                      Gramf.t ))];
                 annot = "`Sum (_loc, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'constructor_declarations)  _ 
                         (_loc : Locf.t)  -> (`Sum (_loc, t) : 'type_repr ) : 
                      'constructor_declarations ->
                        Tokenf.txt -> Locf.t -> 'type_repr ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "{"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (label_declaration_list : 'label_declaration_list
                                                   Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "}"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Record (_loc, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'label_declaration_list)  _ 
                        (_loc : Locf.t)  -> (`Record (_loc, t) : 'type_repr ) : 
                     Tokenf.txt ->
                       'label_declaration_list ->
                         Tokenf.txt -> Locf.t -> 'type_repr ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (constrain : 'constrain Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           {
                             tag = `Key;
                             word = (A "constraint");
                             tag_name = "Key"
                           }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "="); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                 annot = "`Eq (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  _  (_loc : Locf.t) 
                         -> (`Eq (_loc, t1, t2) : 'constrain ) : 'ctyp ->
                                                                   Tokenf.txt
                                                                    ->
                                                                    'ctyp ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'constrain ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (typevars : 'typevars Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols = [Self; Self];
                 annot = "`App (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'typevars)  (t1 : 'typevars) 
                         (_loc : Locf.t)  ->
                         (`App (_loc, t1, t2) : 'typevars ) : 'typevars ->
                                                                'typevars ->
                                                                  Locf.t ->
                                                                    'typevars ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'typevars ) : Tokenf.ant -> Locf.t -> 'typevars ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'typevars ) : Tokenf.ant -> Locf.t -> 'typevars ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "'"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "`Quote (_loc, (`Normal _loc), i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                        (`Quote (_loc, (`Normal _loc), i) : 'typevars ) : 
                     'a_lident -> Tokenf.txt -> Locf.t -> 'typevars ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 10);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "as"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "'"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`Alias (_loc, t1, i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  _  (t1 : 'ctyp) 
                         (_loc : Locf.t)  -> (`Alias (_loc, t1, i) : 
                         'ctyp ) : 'a_lident ->
                                     Tokenf.txt ->
                                       Tokenf.txt -> 'ctyp -> Locf.t -> 'ctyp ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 20);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "!"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (typevars : 'typevars Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "."); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`TyPol (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _  (t1 : 'typevars)  _ 
                         (_loc : Locf.t)  -> (`TyPol (_loc, t1, t2) : 
                         'ctyp ) : 'ctyp ->
                                     Tokenf.txt ->
                                       'typevars ->
                                         Tokenf.txt -> Locf.t -> 'ctyp ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 30);
            lassoc = false;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "->"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Arrow (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  _  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                         (`Arrow (_loc, t1, t2) : 'ctyp ) : 'ctyp ->
                                                              Tokenf.txt ->
                                                                'ctyp ->
                                                                  Locf.t ->
                                                                    'ctyp ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 40);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "~"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ":"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Label (_loc, i, t)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t : 'ctyp)  _  (i : 'a_lident)  _ 
                         (_loc : Locf.t)  -> (`Label (_loc, i, t) : 'ctyp ) : 
                      'ctyp ->
                        Tokenf.txt ->
                          'a_lident -> Tokenf.txt -> Locf.t -> 'ctyp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Label; word = Any; tag_name = "Label" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Label (_loc, (`Lid (_loc, s)), t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0.txt in
                        (`Label (_loc, (`Lid (_loc, s)), t) : 'ctyp ) : 
                     'ctyp -> Tokenf.txt -> Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Optlabel;
                            word = Any;
                            tag_name = "Optlabel"
                          }
                      } : Tokenf.pattern );
                  Self];
                annot = "`OptLabl (_loc, (`Lid (_loc, s)), t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (__fan_0 : Tokenf.txt) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0.txt in
                        (`OptLabl (_loc, (`Lid (_loc, s)), t) : 'ctyp ) : 
                     'ctyp -> Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "?"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`OptLabl (_loc, i, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (i : 'a_lident)  _  (_loc : Locf.t)
                         -> (`OptLabl (_loc, i, t) : 'ctyp ) : 'ctyp ->
                                                                 Tokenf.txt
                                                                   ->
                                                                   'a_lident
                                                                    ->
                                                                    Tokenf.txt
                                                                    ->
                                                                    Locf.t ->
                                                                    'ctyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 50);
            lassoc = true;
            productions =
              [{
                 symbols = [Self; Self];
                 annot = "`App (_loc, t2, t1)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'ctyp)  (t1 : 'ctyp)  (_loc : Locf.t)  ->
                         (`App (_loc, t2, t1) : 'ctyp ) : 'ctyp ->
                                                            'ctyp ->
                                                              Locf.t -> 'ctyp ))
               }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (ctyp : 'ctyp Gramf.t );
       olevel =
         ({
            label = (Some 60);
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Key; word = (A "'"); tag_name = "Key" }
                       } : Tokenf.pattern );
                   Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                 annot = "`Quote (_loc, (`Normal _loc), i)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (i : 'a_lident)  _  (_loc : Locf.t)  ->
                         (`Quote (_loc, (`Normal _loc), i) : 'ctyp ) : 
                      'a_lident -> Tokenf.txt -> Locf.t -> 'ctyp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "_"); tag_name = "Key" }
                      } : Tokenf.pattern )];
                annot = "`Any _loc\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (_loc : Locf.t)  -> (`Any _loc : 'ctyp ) : 
                     Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'ctyp ) : Tokenf.ant -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'ctyp ) : Tokenf.ant -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "par"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'ctyp ) : Tokenf.ant -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'ctyp ) : Tokenf.ant -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "id"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "."); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot =
                  "(try\n   let id = ident_of_ctyp t in\n   fun ()  ->\n     (`Dot (_loc, (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ident) s), id) : \n     ctyp )\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (__fan_0 : Tokenf.ant) 
                        (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        ((try
                            let id = ident_of_ctyp t in
                            fun ()  ->
                              (`Dot
                                 (_loc,
                                   (mk_ant
                                      ~c:(Dyn_tag.to_string Dyn_tag.ident) s),
                                   id) : ctyp )
                          with
                          | Invalid_argument s ->
                              (fun ()  -> raise (Streamf.Error s))) () : 
                          'ctyp ) : 'ctyp ->
                                      Tokenf.txt ->
                                        Tokenf.ant -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "."); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot =
                  "(try let id = ident_of_ctyp t in fun ()  -> `Dot (_loc, (i :>ident), id)\n with | Invalid_argument s -> (fun ()  -> raise (Streamf.Error s))) ()\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (i : 'a_uident)  (_loc : Locf.t) 
                        ->
                        ((try
                            let id = ident_of_ctyp t in
                            fun ()  -> `Dot (_loc, (i :>ident), id)
                          with
                          | Invalid_argument s ->
                              (fun ()  -> raise (Streamf.Error s))) () : 
                        'ctyp ) : 'ctyp ->
                                    Tokenf.txt ->
                                      'a_uident -> Locf.t -> 'ctyp ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
                annot = "(i :>ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'a_lident)  (_loc : Locf.t)  ->
                        ((i :>ctyp) : 'ctyp ) : 'a_lident -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "*"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (star_ctyp : 'star_ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Par (_loc, (`Sta (_loc, t, tl)))\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (tl : 'star_ctyp)  _  (t : 'ctyp)  _ 
                        (_loc : Locf.t)  ->
                        (`Par (_loc, (`Sta (_loc, t, tl))) : 'ctyp ) : 
                     Tokenf.txt ->
                       'star_ctyp ->
                         Tokenf.txt -> 'ctyp -> Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "t\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'ctyp)  _  (_loc : Locf.t)  -> (t : 'ctyp ) : 
                     Tokenf.txt -> 'ctyp -> Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ","); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (com_ctyp : 'com_ctyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (type_longident : 'type_longident Gramf.t ))];
                annot =
                  "appl_of_list ((j :>ctyp) :: t :: (Ast_basic.list_of_com tl []))\n";
                fn =
                  (Gramf.mk_action
                     (fun (j : 'type_longident)  _  (tl : 'com_ctyp)  _ 
                        (t : 'ctyp)  _  (_loc : Locf.t)  ->
                        (appl_of_list ((j :>ctyp) :: t ::
                           (Ast_basic.list_of_com tl [])) : 'ctyp ) : 
                     'type_longident ->
                       Tokenf.txt ->
                         'com_ctyp ->
                           Tokenf.txt ->
                             'ctyp -> Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "["); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`PolyEq (_loc, rfl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                        (`PolyEq (_loc, rfl) : 'ctyp ) : Tokenf.txt ->
                                                           'row_field ->
                                                             Tokenf.txt ->
                                                               Locf.t ->
                                                                 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[>"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`PolySup (_loc, rfl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                        (`PolySup (_loc, rfl) : 'ctyp ) : Tokenf.txt ->
                                                            'row_field ->
                                                              Tokenf.txt ->
                                                                Locf.t ->
                                                                  'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[<"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (rfl : 'row_field)  _  (_loc : Locf.t)  ->
                        let ntl = None in
                        (match ntl with
                         | None  -> `PolyInf (_loc, rfl)
                         | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 
                          'ctyp ) : Tokenf.txt ->
                                      'row_field ->
                                        Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "[<"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (row_field : 'row_field Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ">"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (name_tags : 'name_tags Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "]"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot =
                  "match ntl with\n| None  -> `PolyInf (_loc, rfl)\n| Some ntl -> `PolyInfSup (_loc, rfl, ntl)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (ntl : 'name_tags)  _  (rfl : 'row_field)  _ 
                        (_loc : Locf.t)  ->
                        let ntl = Some ntl in
                        (match ntl with
                         | None  -> `PolyInf (_loc, rfl)
                         | Some ntl -> `PolyInfSup (_loc, rfl, ntl) : 
                          'ctyp ) : Tokenf.txt ->
                                      'name_tags ->
                                        Tokenf.txt ->
                                          'row_field ->
                                            Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "#"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj (class_longident : 'class_longident Gramf.t ))];
                annot = "`ClassPath (_loc, i)\n";
                fn =
                  (Gramf.mk_action
                     (fun (i : 'class_longident)  _  (_loc : Locf.t)  ->
                        (`ClassPath (_loc, i) : 'ctyp ) : 'class_longident ->
                                                            Tokenf.txt ->
                                                              Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "<"); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (opt_meth_list : 'opt_meth_list Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ">"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "t\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t : 'opt_meth_list)  _  (_loc : Locf.t)  ->
                        (t : 'ctyp ) : Tokenf.txt ->
                                         'opt_meth_list ->
                                           Tokenf.txt -> Locf.t -> 'ctyp ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Key; word = (A "("); tag_name = "Key" }
                      } : Tokenf.pattern );
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "module"); tag_name = "Key"
                         }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (mtyp : 'mtyp Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ")"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "`Package (_loc, p)\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (p : 'mtyp)  _  _  (_loc : Locf.t)  ->
                        (`Package (_loc, p) : 'ctyp ) : Tokenf.txt ->
                                                          'mtyp ->
                                                            Tokenf.txt ->
                                                              Tokenf.txt ->
                                                                Locf.t ->
                                                                  'ctyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (comma_ctyp : 'comma_ctyp Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ","); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Com (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'comma_ctyp)  _  (t1 : 'comma_ctyp) 
                         (_loc : Locf.t)  ->
                         (`Com (_loc, t1, t2) : 'comma_ctyp ) : 'comma_ctyp
                                                                  ->
                                                                  Tokenf.txt
                                                                    ->
                                                                    'comma_ctyp
                                                                    ->
                                                                    Locf.t ->
                                                                    'comma_ctyp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                      } : Tokenf.pattern )];
                annot =
                  "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.type_parameters) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant
                           ~c:(Dyn_tag.to_string Dyn_tag.type_parameters) s : 
                          'comma_ctyp ) : Tokenf.ant -> Locf.t -> 'comma_ctyp ))
              };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`Ctyp (_loc, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                        (`Ctyp (_loc, t) : 'comma_ctyp ) : 'ctyp ->
                                                             Locf.t ->
                                                               'comma_ctyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (com_ctyp : 'com_ctyp Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                           'com_ctyp ) : Tokenf.ant -> Locf.t -> 'com_ctyp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'com_ctyp ) : Tokenf.ant -> Locf.t -> 'com_ctyp ))
              };
              {
                symbols =
                  [Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ","); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Com (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'com_ctyp)  _  (t1 : 'com_ctyp) 
                        (_loc : Locf.t)  ->
                        (`Com (_loc, t1, t2) : 'com_ctyp ) : 'com_ctyp ->
                                                               Tokenf.txt ->
                                                                 'com_ctyp ->
                                                                   Locf.t ->
                                                                    'com_ctyp ))
              };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "t\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'com_ctyp ) : 
                     'ctyp -> Locf.t -> 'com_ctyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (star_ctyp : 'star_ctyp Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                           'star_ctyp ) : Tokenf.ant -> Locf.t -> 'star_ctyp ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.ctyp) s : 
                          'star_ctyp ) : Tokenf.ant -> Locf.t -> 'star_ctyp ))
              };
              {
                symbols =
                  [Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "*"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Sta (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'star_ctyp)  _  (t1 : 'star_ctyp) 
                        (_loc : Locf.t)  ->
                        (`Sta (_loc, t1, t2) : 'star_ctyp ) : 'star_ctyp ->
                                                                Tokenf.txt ->
                                                                  'star_ctyp
                                                                    ->
                                                                    Locf.t ->
                                                                    'star_ctyp ))
              };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "t\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (_loc : Locf.t)  -> (t : 'star_ctyp ) : 
                     'ctyp -> Locf.t -> 'star_ctyp ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry =
         (constructor_declarations : 'constructor_declarations Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.or_ctyp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.or_ctyp) s : 
                           'constructor_declarations ) : Tokenf.ant ->
                                                           Locf.t ->
                                                             'constructor_declarations ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.or_ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.or_ctyp) s : 
                          'constructor_declarations ) : Tokenf.ant ->
                                                          Locf.t ->
                                                            'constructor_declarations ))
              };
              {
                symbols =
                  [Self;
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "|"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Self];
                annot = "`Bar (_loc, t1, t2)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t2 : 'constructor_declarations)  _ 
                        (t1 : 'constructor_declarations)  (_loc : Locf.t)  ->
                        (`Bar (_loc, t1, t2) : 'constructor_declarations ) : 
                     'constructor_declarations ->
                       Tokenf.txt ->
                         'constructor_declarations ->
                           Locf.t -> 'constructor_declarations ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "of"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (constructor_arg_list : 'constructor_arg_list Gramf.t ))];
                annot = "`Of (_loc, s, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                        (_loc : Locf.t)  ->
                        (`Of (_loc, s, t) : 'constructor_declarations ) : 
                     'constructor_arg_list ->
                       Tokenf.txt ->
                         'a_uident -> Locf.t -> 'constructor_declarations ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyCol (_loc, s, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (s : 'a_uident)  (_loc : Locf.t) 
                        -> (`TyCol (_loc, s, t) : 'constructor_declarations ) : 
                     'ctyp ->
                       Tokenf.txt ->
                         'a_uident -> Locf.t -> 'constructor_declarations ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))];
                annot = "(s :>or_ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                        ((s :>or_ctyp) : 'constructor_declarations ) : 
                     'a_uident -> Locf.t -> 'constructor_declarations ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (constructor_declaration : 'constructor_declaration Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.of_ctyp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.of_ctyp) s : 
                           'constructor_declaration ) : Tokenf.ant ->
                                                          Locf.t ->
                                                            'constructor_declaration ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.of_ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.of_ctyp) s : 
                          'constructor_declaration ) : Tokenf.ant ->
                                                         Locf.t ->
                                                           'constructor_declaration ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A "of"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm
                    (Gramf.obj
                       (constructor_arg_list : 'constructor_arg_list Gramf.t ))];
                annot = "`Of (_loc, (s :>vid), t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'constructor_arg_list)  _  (s : 'a_uident) 
                        (_loc : Locf.t)  ->
                        (`Of (_loc, (s :>vid), t) : 'constructor_declaration ) : 
                     'constructor_arg_list ->
                       Tokenf.txt ->
                         'a_uident -> Locf.t -> 'constructor_declaration ))
              };
              {
                symbols = [Nterm (Gramf.obj (a_uident : 'a_uident Gramf.t ))];
                annot = "(s :>of_ctyp)\n";
                fn =
                  (Gramf.mk_action
                     (fun (s : 'a_uident)  (_loc : Locf.t)  ->
                        ((s :>of_ctyp) : 'constructor_declaration ) : 
                     'a_uident -> Locf.t -> 'constructor_declaration ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (constructor_arg_list : 'constructor_arg_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A "*"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sta (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'constructor_arg_list)  _ 
                         (t1 : 'constructor_arg_list)  (_loc : Locf.t)  ->
                         (`Sta (_loc, t1, t2) : 'constructor_arg_list ) : 
                      'constructor_arg_list ->
                        Tokenf.txt ->
                          'constructor_arg_list ->
                            Locf.t -> 'constructor_arg_list ))
               };
              {
                symbols = [Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "t\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  (_loc : Locf.t)  ->
                        (t : 'constructor_arg_list ) : 'ctyp ->
                                                         Locf.t ->
                                                           'constructor_arg_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_declaration_list : 'label_declaration_list Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Nterm
                      (Gramf.obj
                         (label_declaration : 'label_declaration Gramf.t ));
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ";"); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Sem (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'label_declaration_list)  _ 
                         (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                         (`Sem (_loc, t1, t2) : 'label_declaration_list ) : 
                      'label_declaration_list ->
                        Tokenf.txt ->
                          'label_declaration ->
                            Locf.t -> 'label_declaration_list ))
               };
              {
                symbols =
                  [Nterm
                     (Gramf.obj
                        (label_declaration : 'label_declaration Gramf.t ))];
                annot = "t1\n";
                fn =
                  (Gramf.mk_action
                     (fun (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                        (t1 : 'label_declaration_list ) : 'label_declaration
                                                            ->
                                                            Locf.t ->
                                                              'label_declaration_list ))
              };
              {
                symbols =
                  [Nterm
                     (Gramf.obj
                        (label_declaration : 'label_declaration Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ";"); tag_name = "Key" }
                     } : Tokenf.pattern )];
                annot = "t1\n";
                fn =
                  (Gramf.mk_action
                     (fun _  (t1 : 'label_declaration)  (_loc : Locf.t)  ->
                        (t1 : 'label_declaration_list ) : Tokenf.txt ->
                                                            'label_declaration
                                                              ->
                                                              Locf.t ->
                                                                'label_declaration_list ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (label_declaration : 'label_declaration Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Token
                      ({
                         descr =
                           { tag = `Ant; word = (Kind ""); tag_name = "Ant" }
                       } : Tokenf.pattern )];
                 annot =
                   "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.name_ctyp) s\n";
                 fn =
                   (Gramf.mk_action
                      (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                         let s = __fan_0 in
                         (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.name_ctyp) s : 
                           'label_declaration ) : Tokenf.ant ->
                                                    Locf.t ->
                                                      'label_declaration ))
               };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          { tag = `Ant; word = (Kind "typ"); tag_name = "Ant"
                          }
                      } : Tokenf.pattern )];
                annot = "mk_ant ~c:(Dyn_tag.to_string Dyn_tag.name_ctyp) s\n";
                fn =
                  (Gramf.mk_action
                     (fun (__fan_0 : Tokenf.ant)  (_loc : Locf.t)  ->
                        let s = __fan_0 in
                        (mk_ant ~c:(Dyn_tag.to_string Dyn_tag.name_ctyp) s : 
                          'label_declaration ) : Tokenf.ant ->
                                                   Locf.t ->
                                                     'label_declaration ))
              };
              {
                symbols =
                  [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyCol (_loc, s, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (s : 'a_lident)  (_loc : Locf.t) 
                        -> (`TyCol (_loc, s, t) : 'label_declaration ) : 
                     'ctyp ->
                       Tokenf.txt ->
                         'a_lident -> Locf.t -> 'label_declaration ))
              };
              {
                symbols =
                  [Token
                     ({
                        descr =
                          {
                            tag = `Key;
                            word = (A "mutable");
                            tag_name = "Key"
                          }
                      } : Tokenf.pattern );
                  Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ));
                  Token
                    ({
                       descr =
                         { tag = `Key; word = (A ":"); tag_name = "Key" }
                     } : Tokenf.pattern );
                  Nterm (Gramf.obj (ctyp : 'ctyp Gramf.t ))];
                annot = "`TyColMut (_loc, s, t)\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'ctyp)  _  (s : 'a_lident)  _  (_loc : Locf.t)
                         -> (`TyColMut (_loc, s, t) : 'label_declaration ) : 
                     'ctyp ->
                       Tokenf.txt ->
                         'a_lident ->
                           Tokenf.txt -> Locf.t -> 'label_declaration ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement );
  Gramf.extend_single
    ({
       entry = (comma_type_parameter : 'comma_type_parameter Gramf.t );
       olevel =
         ({
            label = None;
            lassoc = true;
            productions =
              [{
                 symbols =
                   [Self;
                   Token
                     ({
                        descr =
                          { tag = `Key; word = (A ","); tag_name = "Key" }
                      } : Tokenf.pattern );
                   Self];
                 annot = "`Com (_loc, t1, t2)\n";
                 fn =
                   (Gramf.mk_action
                      (fun (t2 : 'comma_type_parameter)  _ 
                         (t1 : 'comma_type_parameter)  (_loc : Locf.t)  ->
                         (`Com (_loc, t1, t2) : 'comma_type_parameter ) : 
                      'comma_type_parameter ->
                        Tokenf.txt ->
                          'comma_type_parameter ->
                            Locf.t -> 'comma_type_parameter ))
               };
              {
                symbols =
                  [Nterm
                     (Gramf.obj (type_parameter : 'type_parameter Gramf.t ))];
                annot = "`Ctyp (_loc, (t :>ctyp))\n";
                fn =
                  (Gramf.mk_action
                     (fun (t : 'type_parameter)  (_loc : Locf.t)  ->
                        (`Ctyp (_loc, (t :>ctyp)) : 'comma_type_parameter ) : 
                     'type_parameter -> Locf.t -> 'comma_type_parameter ))
              }]
          } : Gramf.olevel )
     } : _ Gramf.single_extend_statement )
let fill_parsers =
  let applied = ref false in
  fun ()  ->
    if not (!applied) then (applied := true; apply (); apply_ctyp ())
let () = Ast_parsers.register_parser ("fan", fill_parsers)
