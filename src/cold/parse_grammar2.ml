let gm = Gram_gen.gm
let grammar_module_name = Gram_gen.grammar_module_name
let text_of_functorial_extend = Gram_gen.text_of_functorial_extend
let mk_name = Gram_gen.mk_name
let mk_entry = Gram_gen.mk_entry
let mk_level = Gram_gen.mk_level
let retype_rule_list_without_patterns =
  Gram_gen.retype_rule_list_without_patterns
let mk_rule = Gram_gen.mk_rule
let check_not_tok = Gram_gen.check_not_tok
let mk_slist = Gram_gen.mk_slist
let mk_symbol = Gram_gen.mk_symbol
let token_of_simple_pat = Gram_gen.token_of_simple_pat
let sem_of_list = Ast_gen.sem_of_list
let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
let tuple_com = Ast_gen.tuple_com
open FAst
open Util
let g =
  Gramf.create_lexer ~annot:"Grammar's lexer"
    ~keywords:["`";
              "(";
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
              "Flo"] ()
type words =  
  | A of string list
  | Any 
type data =  {
  tag: string;
  words: words} 
let simple: data Gramf.t = Gramf.mk_dynamic g "simple"
let _ =
  let grammar_entry_create x = Gramf.mk_dynamic g x in
  let or_words: 'or_words Gramf.t = grammar_entry_create "or_words"
  and str: 'str Gramf.t = grammar_entry_create "str"
  and lid: 'lid Gramf.t = grammar_entry_create "lid" in
  Gramf.extend_single (simple : 'simple Gramf.t )
    (None,
      (None, None,
        [([`Skeyword "`"; `Skeyword "EOI"],
           ("{ tag = \"EOI\"; words = (A []) }\n",
             (Gramf.mk_action
                (fun _  _  (_loc : Locf.t)  ->
                   ({ tag = "EOI"; words = (A []) } : 'simple )))));
        ([`Skeyword "`";
         `Skeyword "Lid";
         `Stoken
           (((function | `Str _ -> true | _ -> false)),
             (`App ((`Vrn "Str"), `Any)), "`Str _")],
          ("{ tag = \"Lid\"; words = (A [v]) }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Str v -> ({ tag = "Lid"; words = (A [v]) } : 'simple )
                  | _ -> failwith "{ tag = \"Lid\"; words = (A [v]) }\n"))));
        ([`Skeyword "`";
         `Skeyword "Uid";
         `Stoken
           (((function | `Str _ -> true | _ -> false)),
             (`App ((`Vrn "Str"), `Any)), "`Str _")],
          ("{ tag = \"Uid\"; words = (A [v]) }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Str v -> ({ tag = "Uid"; words = (A [v]) } : 'simple )
                  | _ -> failwith "{ tag = \"Uid\"; words = (A [v]) }\n"))));
        ([`Skeyword "`";
         `Skeyword "Lid";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Lid\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Lid"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Lid\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Uid";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Uid\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Uid"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Uid\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Quot";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Quot\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Quot"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Quot\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Label";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Label\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Label"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Label\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "DirQuotation";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"DirQuotation\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x ->
                      ({ tag = "DirQuotation"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"DirQuotation\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Optlabel";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Optlabel\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Optlabel"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Optlabel\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Str";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Str\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Str"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Str\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Chr";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Chr\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Chr"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Chr\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Int";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Int\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Int"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Int\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Int32";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Int32\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Int32"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Int32\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Int64";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Int64\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Int64"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Int64\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Nativeint";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Nativeint\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Nativeint"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Nativeint\"; words = Any }\n"))));
        ([`Skeyword "`";
         `Skeyword "Flo";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("{ tag = \"Flo\"; words = Any }\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  _  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid x -> ({ tag = "Flo"; words = Any } : 'simple )
                  | _ -> failwith "{ tag = \"Flo\"; words = Any }\n"))));
        ([`Skeyword "`"; `Skeyword "Lid"; `Skeyword "_"],
          ("{ tag = \"Lid\"; words = Any }\n",
            (Gramf.mk_action
               (fun _  _  _  (_loc : Locf.t)  ->
                  ({ tag = "Lid"; words = Any } : 'simple )))));
        ([`Skeyword "`"; `Skeyword "Uid"; `Skeyword "_"],
          ("{ tag = \"Uid\"; words = Any }\n",
            (Gramf.mk_action
               (fun _  _  _  (_loc : Locf.t)  ->
                  ({ tag = "Uid"; words = Any } : 'simple )))));
        ([`Skeyword "`";
         `Skeyword "Ant";
         `Skeyword "(";
         `Snterm (Gramf.obj (or_words : 'or_words Gramf.t ));
         `Skeyword ",";
         `Snterm (Gramf.obj (lid : 'lid Gramf.t ));
         `Skeyword ")"],
          ("{ tag = \"Ant\"; words = p }\n",
            (Gramf.mk_action
               (fun _  _  _  (p : 'or_words)  _  _  _  (_loc : Locf.t)  ->
                  ({ tag = "Ant"; words = p } : 'simple )))));
        ([`Skeyword "`";
         `Skeyword "Uid";
         `Skeyword "(";
         `Snterm (Gramf.obj (or_words : 'or_words Gramf.t ));
         `Skeyword ")"],
          ("{ tag = \"Uid\"; words = p }\n",
            (Gramf.mk_action
               (fun _  (p : 'or_words)  _  _  _  (_loc : Locf.t)  ->
                  ({ tag = "Uid"; words = p } : 'simple )))))]));
  Gramf.extend_single (or_words : 'or_words Gramf.t )
    (None,
      (None, None,
        [([`Slist1sep
             ((`Snterm (Gramf.obj (str : 'str Gramf.t ))), (`Skeyword "|"))],
           ("A v\n",
             (Gramf.mk_action
                (fun (v : 'str list)  (_loc : Locf.t)  -> (A v : 'or_words )))));
        ([`Slist1sep
            ((`Snterm (Gramf.obj (str : 'str Gramf.t ))), (`Skeyword "|"));
         `Skeyword "as";
         `Stoken
           (((function | `Lid _ -> true | _ -> false)),
             (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
          ("A v\n",
            (Gramf.mk_action
               (fun (__fan_2 : [> Tokenf.t])  _  (v : 'str list) 
                  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid s -> (A v : 'or_words )
                  | _ -> failwith "A v\n"))))]));
  Gramf.extend_single (str : 'str Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)),
               (`App ((`Vrn "Str"), `Any)), "`Str _")],
           ("s\n",
             (Gramf.mk_action
                (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str s -> (s : 'str )
                   | _ -> failwith "s\n"))))]));
  Gramf.extend_single (lid : 'lid Gramf.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid _ -> true | _ -> false)),
               (`App ((`Vrn "Lid"), `Any)), "`Lid _")],
           ("`Lid (_loc, s)\n",
             (Gramf.mk_action
                (fun (__fan_0 : [> Tokenf.t])  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid s -> (`Lid (_loc, s) : 'lid )
                   | _ -> failwith "`Lid (_loc, s)\n"))))]))
