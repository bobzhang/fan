let cparser = Compile_stream.cparser
let cstream = Compile_stream.cstream
let exp = Syntaxf.exp
let a_lident = Syntaxf.a_lident
let pat = Syntaxf.pat
open FAst
let parser_ipat = Gramf.mk "parser_ipat"
let parser_exp = Gramf.mk "parser_exp"
let stream_pat_comp = Gramf.mk "stream_pat_comp"
let stream_pat_comp_err = Gramf.mk "stream_pat_comp_err"
let stream_pat_comp_err_list = Gramf.mk "stream_pat_comp_err_list"
let stream_pat = Gramf.mk "stream_pat"
let parser_case = Gramf.mk "parser_case"
let parser_case_list = Gramf.mk "parser_case_list"
let _ =
  Gramf.extend_single (parser_exp : 'parser_exp Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm
                 (Gramf.obj (parser_case_list : 'parser_case_list Gramf.t ))];
            annot =
              "match n with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc pcl)\n| None  -> cparser _loc pcl\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(pcl : 'parser_case_list)  (_loc : Locf.t)  ->
                    let n = None in
                    (match n with
                     | Some o ->
                         Ref.protect Compile_stream.grammar_module_name o
                           (fun _  -> cparser _loc pcl)
                     | None  -> cparser _loc pcl : 'parser_exp )))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Uid; word = Any; tag_name = "Uid" } } : 
                Tokenf.pattern );
             Nterm
               (Gramf.obj (parser_case_list : 'parser_case_list Gramf.t ))];
           annot =
             "match n with\n| Some o ->\n    Ref.protect Compile_stream.grammar_module_name o\n      (fun _  -> cparser _loc pcl)\n| None  -> cparser _loc pcl\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(pcl : 'parser_case_list) 
                   ~__fan_0:(__fan_0 : Tokenf.txt)  (_loc : Locf.t)  ->
                   let n = __fan_0.txt in
                   let n = Some n in
                   (match n with
                    | Some o ->
                        Ref.protect Compile_stream.grammar_module_name o
                          (fun _  -> cparser _loc pcl)
                    | None  -> cparser _loc pcl : 'parser_exp )))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (parser_ipat : 'parser_ipat Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols = [Nterm (Gramf.obj (a_lident : 'a_lident Gramf.t ))];
            annot = "(i : alident  :>pat)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(i : 'a_lident)  (_loc : Locf.t)  ->
                    ((i : alident  :>pat) : 'parser_ipat )))
          };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "_"); tag_name = "Key" } } : 
                Tokenf.pattern )];
           annot = "(`Any _loc : FAst.pat )\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:_  (_loc : Locf.t)  ->
                   ((`Any _loc : FAst.pat ) : 'parser_ipat )))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (parser_case_list : 'parser_case_list Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Token
                 ({ descr = { tag = `Key; word = (A "|"); tag_name = "Key" }
                  } : Tokenf.pattern );
              List0sep
                ((Nterm (Gramf.obj (parser_case : 'parser_case Gramf.t ))),
                  (Token
                     ({
                        descr =
                          { tag = `Key; word = (A "|"); tag_name = "Key" }
                      } : Tokenf.pattern )))];
            annot = "pcl\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_1:(pcl : 'parser_case list)  ~__fan_0:_ 
                    (_loc : Locf.t)  -> (pcl : 'parser_case_list )))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (parser_case : 'parser_case Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (stream_pat : 'stream_pat Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A "->"); tag_name = "Key" }
                 } : Tokenf.pattern );
              Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
            annot = "(sp, None, e)\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(sp : 'stream_pat)  (_loc : Locf.t)  ->
                    ((sp, None, e) : 'parser_case )))
          }]
     } : Gramf.olevel );
  Gramf.extend_single (stream_pat : 'stream_pat Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm
                 (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ))];
            annot = "[(spc, None)]\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                    ([(spc, None)] : 'stream_pat )))
          };
         {
           symbols =
             [Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm
               (Gramf.obj
                  (stream_pat_comp_err_list : 'stream_pat_comp_err_list
                                                Gramf.t ))];
           annot = "(spc, None) :: sp\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_2:(sp : 'stream_pat_comp_err_list)  ~__fan_1:_ 
                   ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                   ((spc, None) :: sp : 'stream_pat )))
         };
         {
           symbols = [];
           annot = "[]\n";
           fn =
             (Gramf.mk_action (fun (_loc : Locf.t)  -> ([] : 'stream_pat )))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (stream_pat_comp : 'stream_pat_comp Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
              Token
                ({
                   descr =
                     { tag = `Key; word = (A "when"); tag_name = "Key" }
                 } : Tokenf.pattern );
              Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
            annot = "When (_loc, p, (Some e))\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                    (_loc : Locf.t)  ->
                    (When (_loc, p, (Some e)) : 'stream_pat_comp )))
          };
         {
           symbols = [Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
           annot = "When (_loc, p, None)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(p : 'pat)  (_loc : Locf.t)  ->
                   (When (_loc, p, None) : 'stream_pat_comp )))
         };
         {
           symbols =
             [Nterm (Gramf.obj (pat : 'pat Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A "="); tag_name = "Key" } } : 
               Tokenf.pattern );
             Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
           annot = "Match (_loc, p, e)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_2:(e : 'exp)  ~__fan_1:_  ~__fan_0:(p : 'pat) 
                   (_loc : Locf.t)  ->
                   (Match (_loc, p, e) : 'stream_pat_comp )))
         };
         {
           symbols =
             [Token
                ({ descr = { tag = `Key; word = (A "'"); tag_name = "Key" } } : 
                Tokenf.pattern );
             Nterm (Gramf.obj (pat : 'pat Gramf.t ))];
           annot = "Str (_loc, p)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:(p : 'pat)  ~__fan_0:_  (_loc : Locf.t)  ->
                   (Str (_loc, p) : 'stream_pat_comp )))
         }]
     } : Gramf.olevel );
  Gramf.extend_single (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm
                 (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ));
              Token
                ({ descr = { tag = `Key; word = (A "??"); tag_name = "Key" }
                 } : Tokenf.pattern );
              Nterm (Gramf.obj (exp : 'exp Gramf.t ))];
            annot = "(spc, (Some e))\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_2:(e : 'exp)  ~__fan_1:_ 
                    ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                    ((spc, (Some e)) : 'stream_pat_comp_err )))
          };
         {
           symbols =
             [Nterm (Gramf.obj (stream_pat_comp : 'stream_pat_comp Gramf.t ))];
           annot = "(spc, None)\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_0:(spc : 'stream_pat_comp)  (_loc : Locf.t)  ->
                   ((spc, None) : 'stream_pat_comp_err )))
         }]
     } : Gramf.olevel );
  Gramf.extend_single
    (stream_pat_comp_err_list : 'stream_pat_comp_err_list Gramf.t )
    ({
       label = None;
       assoc = None;
       productions =
         [{
            symbols =
              [Nterm
                 (Gramf.obj
                    (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ))];
            annot = "[spc]\n";
            fn =
              (Gramf.mk_action
                 (fun ~__fan_0:(spc : 'stream_pat_comp_err)  (_loc : Locf.t) 
                    -> ([spc] : 'stream_pat_comp_err_list )))
          };
         {
           symbols =
             [Nterm
                (Gramf.obj
                   (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern )];
           annot = "[spc]\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_1:_  ~__fan_0:(spc : 'stream_pat_comp_err) 
                   (_loc : Locf.t)  -> ([spc] : 'stream_pat_comp_err_list )))
         };
         {
           symbols =
             [Nterm
                (Gramf.obj
                   (stream_pat_comp_err : 'stream_pat_comp_err Gramf.t ));
             Token
               ({ descr = { tag = `Key; word = (A ";"); tag_name = "Key" } } : 
               Tokenf.pattern );
             Self];
           annot = "spc :: sp\n";
           fn =
             (Gramf.mk_action
                (fun ~__fan_2:(sp : 'stream_pat_comp_err_list)  ~__fan_1:_ 
                   ~__fan_0:(spc : 'stream_pat_comp_err)  (_loc : Locf.t)  ->
                   (spc :: sp : 'stream_pat_comp_err_list )))
         }]
     } : Gramf.olevel )
let () = Ast_quotation.of_exp ~name:(Ns.lang, "parser") ~entry:parser_exp ()
