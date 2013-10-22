let gm = Compile_gram.gm
let module_name = Compile_gram.module_name
let mk_entry = Compile_gram.mk_entry
let mk_level = Compile_gram.mk_level
let mk_rule = Compile_gram.mk_rule
let mk_slist = Compile_gram.mk_slist
let mk_symbol = Compile_gram.mk_symbol
let make = Compile_gram.make
let is_irrefut_pat = Fan_ops.is_irrefut_pat
let sem_of_list = Ast_gen.sem_of_list
let loc_of = Ast_gen.loc_of
let seq_sem = Ast_gen.seq_sem
let tuple_com = Ast_gen.tuple_com
let mk_name _loc (i : FAst.vid) =
  (let rec aux x =
     match (x : FAst.vid ) with
     | `Lid (_,x)|`Uid (_,x) -> x
     | `Dot (_,`Uid (_,x),xs) -> x ^ ("__" ^ (aux xs))
     | _ -> failwith "internal error in the Grammar extension" in
   { exp = (i :>FAst.exp); tvar = (aux i); loc = _loc } : Gram_def.name )
open FAst
open Util
let g =
  Fgram.create_lexer ~annot:"Grammar's lexer"
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
              "OPT";
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
let normalize (x : Gram_pat.t) =
  (match x with
   | `Vrn (_loc,x) -> (x, `Empty)
   | `App (_loc,`Vrn (_,x),`Str (_,s))
     |`App (_loc,`Vrn (_,x),`Alias (_,`Str (_,s),_)) -> (x, (`A s))
   | `App (_loc,`Vrn (_,x),`Lid (_,_))|`App (_loc,`Vrn (_,x),`Any _) ->
       (x, `Any)
   | `App (_loc,`App (_,`Vrn (_,x),`Lid (_,_)),_) -> (x, `Any)
   | `App (_loc,`App (_,`Vrn (_,x),`Alias (_,`Str (_,s),_)),_)
     |`App (_loc,`App (_,`Vrn (_,x),`Str (_,s)),_) -> (x, (`A s))
   | _ -> (failwithf "normalize %s") @@ (Gram_pat.to_string x) : Gram_def.data )
let token_of_simple_pat (p : Gram_pat.t) =
  (let _loc = loc_of p in
   let p_pat = (p : Gram_pat.t  :>pat) in
   let (po,ls) = Compile_gram.filter_pat_with_captured_variables p_pat in
   let mdescr = (Gram_def.meta_data#data _loc (normalize p) :>exp) in
   let no_variable = Gram_pat.wildcarder#t p in
   let mstr = Gram_pat.to_string no_variable in
   match ls with
   | [] ->
       let match_fun =
         let v = (no_variable :>pat) in
         if is_irrefut_pat v
         then
           (`Fun (_loc, (`Case (_loc, v, (`Lid (_loc, "true"))))) : FAst.exp )
         else
           (`Fun
              (_loc,
                (`Bar
                   (_loc, (`Case (_loc, v, (`Lid (_loc, "true")))),
                     (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) : 
           FAst.exp ) in
       {
         text = (`Stoken (_loc, match_fun, mdescr, mstr));
         styp = (`Tok _loc);
         pattern = (Some p_pat)
       }
   | (x,y)::ys ->
       let guard =
         List.fold_left
           (fun acc  (x,y)  ->
              (`App
                 (_loc, (`App (_loc, (`Lid (_loc, "&&")), acc)),
                   (`App (_loc, (`App (_loc, (`Lid (_loc, "=")), x)), y))) : 
              FAst.exp ))
           (`App (_loc, (`App (_loc, (`Lid (_loc, "=")), x)), y) : FAst.exp )
           ys in
       let match_fun: FAst.exp =
         `Fun
           (_loc,
             (`Bar
                (_loc, (`CaseWhen (_loc, po, guard, (`Lid (_loc, "true")))),
                  (`Case (_loc, (`Any _loc), (`Lid (_loc, "false"))))))) in
       {
         text = (`Stoken (_loc, match_fun, mdescr, mstr));
         styp = (`Tok _loc);
         pattern = (Some (Objs.wildcarder#pat po))
       } : Gram_def.symbol )
let extend_header = Fgram.mk_dynamic g "extend_header"
let qualuid: vid Fgram.t = Fgram.mk_dynamic g "qualuid"
let qualid: vid Fgram.t = Fgram.mk_dynamic g "qualid"
let t_qualid: vid Fgram.t = Fgram.mk_dynamic g "t_qualid"
let entry_name:
  ([ `name of Ftoken.name option | `non]* Gram_def.name) Fgram.t =
  Fgram.mk_dynamic g "entry_name"
let position = Fgram.mk_dynamic g "position"
let assoc = Fgram.mk_dynamic g "assoc"
let name = Fgram.mk_dynamic g "name"
let string = Fgram.mk_dynamic g "string"
let rules = Fgram.mk_dynamic g "rules"
let symbol = Fgram.mk_dynamic g "symbol"
let rule = Fgram.mk_dynamic g "rule"
let meta_rule = Fgram.mk_dynamic g "meta_rule"
let rule_list = Fgram.mk_dynamic g "rule_list"
let psymbol = Fgram.mk_dynamic g "psymbol"
let level = Fgram.mk_dynamic g "level"
let level_list = Fgram.mk_dynamic g "level_list"
let entry: Gram_def.entry option Fgram.t = Fgram.mk_dynamic g "entry"
let extend_body = Fgram.mk_dynamic g "extend_body"
let unsafe_extend_body = Fgram.mk_dynamic g "unsafe_extend_body"
let simple: Gram_def.symbol list Fgram.t = Fgram.mk_dynamic g "simple"
let _ =
  let grammar_entry_create x = Fgram.mk_dynamic g x in
  let or_strs: 'or_strs Fgram.t = grammar_entry_create "or_strs"
  and str0: 'str0 Fgram.t = grammar_entry_create "str0"
  and or_words: 'or_words Fgram.t = grammar_entry_create "or_words"
  and level_str: 'level_str Fgram.t = grammar_entry_create "level_str"
  and str: 'str Fgram.t = grammar_entry_create "str"
  and lid: 'lid Fgram.t = grammar_entry_create "lid"
  and sep_symbol: 'sep_symbol Fgram.t = grammar_entry_create "sep_symbol"
  and brace_pattern: 'brace_pattern Fgram.t =
    grammar_entry_create "brace_pattern" in
  Fgram.extend_single (simple : 'simple Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "EOI"],
           ("[token_of_simple_pat (`Vrn (_loc, \"EOI\"))]\n",
             (Fgram.mk_action
                (fun _  (_loc : Locf.t)  ->
                   ([token_of_simple_pat (`Vrn (_loc, "EOI"))] : 'simple )))));
        ([`Skeyword "Lid";
         `Stoken
           (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
             "`Str _")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Str (_loc, x)))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com\n         (_loc, (`Str (_loc, v)),\n           (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))), (`Str (_loc, x))) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Str x,`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))),
                                            (`Str (_loc, x)))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`App
                                     (_loc, (`Vrn (_loc, "A")),
                                       (`Str (_loc, x))))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (_loc,
                                (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),
                                (`Str (_loc, x))) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Uid";
         `Stoken
           (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
             "`Str _")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Str (_loc, x)))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par\n    (_loc,\n      (`Com\n         (_loc, (`Str (_loc, v)),\n           (`App (_loc, (`Vrn (_loc, \"A\")), (`Str (_loc, x))))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))), (`Str (_loc, x))) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Str x,`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))),
                                            (`Str (_loc, x)))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`App
                                     (_loc, (`Vrn (_loc, "A")),
                                       (`Str (_loc, x))))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Str (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (_loc,
                                (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),
                                (`Str (_loc, x))) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Lid";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (xloc, (`App (xloc, (`Vrn (xloc, v)), (`Any xloc))), (`Lid (xloc, x))) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (xloc,x),`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (xloc,
                                (`App (xloc, (`Vrn (xloc, v)), (`Any xloc))),
                                (`Lid (xloc, x))) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Uid";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (xloc, (`App (xloc, (`Vrn (xloc, v)), (`Any xloc))), (`Lid (xloc, x))) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (xloc,x),`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (xloc,
                                (`App (xloc, (`Vrn (xloc, v)), (`Any xloc))),
                                (`Lid (xloc, x))) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Lid";
         `Skeyword "@";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid loc");
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (xloc, (`App (xloc, (`Vrn (xloc, v)), (`Lid (xloc, loc)))),\n         (`Lid (xloc, x))) : FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_3 : Ftoken.t)  (__fan_2 : Ftoken.t)  _ 
                  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match (__fan_3, __fan_2, __fan_0) with
                  | (`Lid (xloc,x),`Lid (_,loc),`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (xloc,
                                (`App
                                   (xloc, (`Vrn (xloc, v)),
                                     (`Lid (xloc, loc)))), (`Lid (xloc, x))) : 
                           FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s %s"
                           (Ftoken.token_to_string __fan_3)
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Uid";
         `Skeyword "@";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid loc");
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str =\n  Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in\nlet pattern =\n  Some\n    (`App\n       (xloc, (`App (xloc, (`Vrn (xloc, v)), (`Lid (xloc, loc)))),\n         (`Lid (xloc, x))) : FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun (__fan_3 : Ftoken.t)  (__fan_2 : Ftoken.t)  _ 
                  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match (__fan_3, __fan_2, __fan_0) with
                  | (`Lid (xloc,x),`Lid (_,loc),`Key v) ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x)))) in
                       let pattern =
                         Some
                           (`App
                              (xloc,
                                (`App
                                   (xloc, (`Vrn (xloc, v)),
                                     (`Lid (xloc, loc)))), (`Lid (xloc, x))) : 
                           FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s %s"
                           (Ftoken.token_to_string __fan_3)
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Lid"; `Skeyword "_"],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\nlet pattern =\n  Some\n    (`App (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))), (`Any _loc)) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key v ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                       let pattern =
                         Some
                           (`App
                              (_loc,
                                (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),
                                (`Any _loc)) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Uid"; `Skeyword "_"],
          ("let pred: FAst.exp =\n  `Fun\n    (_loc,\n      (`Bar\n         (_loc,\n           (`Case\n              (_loc,\n                (`App\n                   (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),\n                     (`Any _loc))), (`Lid (_loc, \"true\")))),\n           (`Case (_loc, (`Any _loc), (`Lid (_loc, \"false\"))))))) in\nlet des: FAst.exp =\n  `Par (_loc, (`Com (_loc, (`Str (_loc, v)), (`Vrn (_loc, \"Any\"))))) in\nlet des_str = Gram_pat.to_string (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in\nlet pattern =\n  Some\n    (`App (_loc, (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))), (`Any _loc)) : \n    FAst.pat ) in\n[{\n   Gram_def.text = (`Stoken (_loc, pred, des, des_str));\n   styp = (`Tok _loc);\n   pattern\n }]\n",
            (Fgram.mk_action
               (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key v ->
                      (let pred: FAst.exp =
                         `Fun
                           (_loc,
                             (`Bar
                                (_loc,
                                  (`Case
                                     (_loc,
                                       (`App
                                          (_loc,
                                            (`App
                                               (_loc, (`Vrn (_loc, v)),
                                                 (`Any _loc))), (`Any _loc))),
                                       (`Lid (_loc, "true")))),
                                  (`Case
                                     (_loc, (`Any _loc),
                                       (`Lid (_loc, "false"))))))) in
                       let des: FAst.exp =
                         `Par
                           (_loc,
                             (`Com
                                (_loc, (`Str (_loc, v)),
                                  (`Vrn (_loc, "Any"))))) in
                       let des_str =
                         Gram_pat.to_string
                           (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))) in
                       let pattern =
                         Some
                           (`App
                              (_loc,
                                (`App (_loc, (`Vrn (_loc, v)), (`Any _loc))),
                                (`Any _loc)) : FAst.pat ) in
                       [{
                          Gram_def.text =
                            (`Stoken (_loc, pred, des, des_str));
                          styp = (`Tok _loc);
                          pattern
                        }] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Quot";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Label";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "DirQuotation";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Optlabel";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Str";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Chr";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Int";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Int32";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Int64";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Nativeint";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Flo";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))]\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_1, __fan_0) with
                  | (`Lid (_,x),`Key v) ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Lid (_loc, x))))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_1)
                           (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Str"; `Skeyword "_"],
          ("[token_of_simple_pat (`App (_loc, (`Vrn (_loc, v)), (`Any _loc)))]\n",
            (Fgram.mk_action
               (fun _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key v ->
                      ([token_of_simple_pat
                          (`App (_loc, (`Vrn (_loc, v)), (`Any _loc)))] : 
                      'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Ant";
         `Skeyword "(";
         `Snterm (Fgram.obj (or_words : 'or_words Fgram.t ));
         `Skeyword ",";
         `Snterm (Fgram.obj (lid : 'lid Fgram.t ));
         `Skeyword ")"],
          ("match p with\n| (v,None ) ->\n    List.map\n      (fun x  ->\n         token_of_simple_pat\n           (`App (_loc, (`App (_loc, (`Vrn (_loc, \"Ant\")), x)), p1))) v\n| (v,Some u) ->\n    List.map\n      (fun x  ->\n         token_of_simple_pat\n           (`App\n              (_loc,\n                (`App\n                   (_loc, (`Vrn (_loc, \"Ant\")),\n                     (`Alias (_loc, x, (`Lid (_loc, u)))))), p1))) v\n",
            (Fgram.mk_action
               (fun _  (p1 : 'lid)  _  (p : 'or_words)  _  _  (_loc : Locf.t)
                   ->
                  (match p with
                   | (v,None ) ->
                       List.map
                         (fun x  ->
                            token_of_simple_pat
                              (`App
                                 (_loc,
                                   (`App (_loc, (`Vrn (_loc, "Ant")), x)),
                                   p1))) v
                   | (v,Some u) ->
                       List.map
                         (fun x  ->
                            token_of_simple_pat
                              (`App
                                 (_loc,
                                   (`App
                                      (_loc, (`Vrn (_loc, "Ant")),
                                        (`Alias (_loc, x, (`Lid (_loc, u)))))),
                                   p1))) v : 'simple )))));
        ([`Stoken
            (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
              "`Str _")],
          ("[mk_symbol ~text:(`Skeyword (_loc, s)) ~styp:(`Tok _loc) ~pattern:None]\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Str s ->
                      ([mk_symbol ~text:(`Skeyword (_loc, s))
                          ~styp:(`Tok _loc) ~pattern:None] : 'simple )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "(";
         `Snterm (Fgram.obj (or_strs : 'or_strs Fgram.t ));
         `Skeyword ")"],
          ("match v with\n| (vs,None ) ->\n    vs |>\n      (List.map\n         (fun x  ->\n            mk_symbol ~text:(`Skeyword (_loc, x)) ~styp:(`Tok _loc)\n              ~pattern:None))\n| (vs,Some b) ->\n    vs |>\n      (List.map\n         (fun x  ->\n            mk_symbol ~text:(`Skeyword (_loc, x)) ~styp:(`Tok _loc)\n              ~pattern:(Some\n                          (`App\n                             (_loc, (`Vrn (_loc, \"Key\")), (`Lid (_loc, b))) : \n                          FAst.pat ))))\n",
            (Fgram.mk_action
               (fun _  (v : 'or_strs)  _  (_loc : Locf.t)  ->
                  (match v with
                   | (vs,None ) ->
                       vs |>
                         (List.map
                            (fun x  ->
                               mk_symbol ~text:(`Skeyword (_loc, x))
                                 ~styp:(`Tok _loc) ~pattern:None))
                   | (vs,Some b) ->
                       vs |>
                         (List.map
                            (fun x  ->
                               mk_symbol ~text:(`Skeyword (_loc, x))
                                 ~styp:(`Tok _loc)
                                 ~pattern:(Some
                                             (`App
                                                (_loc, (`Vrn (_loc, "Key")),
                                                  (`Lid (_loc, b))) : 
                                             FAst.pat )))) : 'simple )))));
        ([`Skeyword "Uid";
         `Skeyword "(";
         `Snterm (Fgram.obj (or_words : 'or_words Fgram.t ));
         `Skeyword ")"],
          ("match p with\n| (v,None ) ->\n    List.map\n      (fun x  -> token_of_simple_pat (`App (_loc, (`Vrn (_loc, \"Uid\")), x)))\n      v\n| (v,Some x) ->\n    List.map\n      (fun a  ->\n         token_of_simple_pat\n           (`App\n              (_loc, (`Vrn (_loc, \"Uid\")),\n                (`Alias (_loc, a, (`Lid (_loc, x))))))) v\n",
            (Fgram.mk_action
               (fun _  (p : 'or_words)  _  _  (_loc : Locf.t)  ->
                  (match p with
                   | (v,None ) ->
                       List.map
                         (fun x  ->
                            token_of_simple_pat
                              (`App (_loc, (`Vrn (_loc, "Uid")), x))) v
                   | (v,Some x) ->
                       List.map
                         (fun a  ->
                            token_of_simple_pat
                              (`App
                                 (_loc, (`Vrn (_loc, "Uid")),
                                   (`Alias (_loc, a, (`Lid (_loc, x))))))) v : 
                  'simple )))));
        ([`Skeyword "S"],
          ("[mk_symbol ~text:(`Sself _loc) ~styp:(`Self _loc) ~pattern:None]\n",
            (Fgram.mk_action
               (fun _  (_loc : Locf.t)  ->
                  ([mk_symbol ~text:(`Sself _loc) ~styp:(`Self _loc)
                      ~pattern:None] : 'simple )))));
        ([`Snterm (Fgram.obj (name : 'name Fgram.t ));
         `Sopt (`Snterm (Fgram.obj (level_str : 'level_str Fgram.t )))],
          ("[mk_symbol ~text:(`Snterm (_loc, n, lev))\n   ~styp:(`Quote (_loc, (`Normal _loc), (`Lid (_loc, (n.tvar)))))\n   ~pattern:None]\n",
            (Fgram.mk_action
               (fun (lev : 'level_str option)  (n : 'name)  (_loc : Locf.t) 
                  ->
                  ([mk_symbol ~text:(`Snterm (_loc, n, lev))
                      ~styp:(`Quote
                               (_loc, (`Normal _loc),
                                 (`Lid (_loc, (n.tvar))))) ~pattern:None] : 
                  'simple )))))]));
  Fgram.extend_single (or_strs : 'or_strs Fgram.t )
    (None,
      (None, None,
        [([`Slist1sep
             ((`Snterm (Fgram.obj (str0 : 'str0 Fgram.t ))), (`Skeyword "|"))],
           ("(xs, None)\n",
             (Fgram.mk_action
                (fun (xs : 'str0 list)  (_loc : Locf.t)  ->
                   ((xs, None) : 'or_strs )))));
        ([`Slist1sep
            ((`Snterm (Fgram.obj (str0 : 'str0 Fgram.t ))), (`Skeyword "|"));
         `Skeyword "as";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid s")],
          ("(xs, (Some s))\n",
            (Fgram.mk_action
               (fun (__fan_2 : Ftoken.t)  _  (xs : 'str0 list) 
                  (_loc : Locf.t)  ->
                  match __fan_2 with
                  | `Lid (_,s) -> ((xs, (Some s)) : 'or_strs )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_2))))))]));
  Fgram.extend_single (str0 : 'str0 Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
               "`Str _")],
           ("s\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str s -> (s : 'str0 )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (or_words : 'or_words Fgram.t )
    (None,
      (None, None,
        [([`Slist1sep
             ((`Snterm (Fgram.obj (str : 'str Fgram.t ))), (`Skeyword "|"))],
           ("(v, None)\n",
             (Fgram.mk_action
                (fun (v : 'str list)  (_loc : Locf.t)  ->
                   ((v, None) : 'or_words )))));
        ([`Slist1sep
            ((`Snterm (Fgram.obj (str : 'str Fgram.t ))), (`Skeyword "|"));
         `Skeyword "as";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid s")],
          ("(v, (Some s))\n",
            (Fgram.mk_action
               (fun (__fan_2 : Ftoken.t)  _  (v : 'str list)  (_loc : Locf.t)
                   ->
                  match __fan_2 with
                  | `Lid (_,s) -> ((v, (Some s)) : 'or_words )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_2))))))]));
  Fgram.extend_single (level_str : 'level_str Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "Level";
          `Stoken
            (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
              "`Str _")],
           ("s\n",
             (Fgram.mk_action
                (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Str s -> (s : 'level_str )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (str : 'str Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
               "`Str _")],
           ("`Str (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str s -> (`Str (_loc, s) : 'str )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (lid : 'lid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
               "`Lid s")],
           ("`Lid (_loc, s)\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Lid (_,s) -> (`Lid (_loc, s) : 'lid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (sep_symbol : 'sep_symbol Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "SEP"; `Snterm (Fgram.obj (simple : 'simple Fgram.t ))],
           ("let t::[] = t in t\n",
             (Fgram.mk_action
                (fun (t : 'simple)  _  (_loc : Locf.t)  ->
                   (let t::[] = t in t : 'sep_symbol )))))]));
  Fgram.extend_single (symbol : 'symbol Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "L0";
          `Snterm (Fgram.obj (simple : 'simple Fgram.t ));
          `Sopt (`Snterm (Fgram.obj (sep_symbol : 'sep_symbol Fgram.t )))],
           ("let s::[] = s in\nlet styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = mk_slist _loc (if l = \"L0\" then false else true) sep s in\n[mk_symbol ~text ~styp ~pattern:None]\n",
             (Fgram.mk_action
                (fun (sep : 'sep_symbol option)  (s : 'simple) 
                   (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key l ->
                       (let s::[] = s in
                        let styp =
                          `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                        let text =
                          mk_slist _loc (if l = "L0" then false else true)
                            sep s in
                        [mk_symbol ~text ~styp ~pattern:None] : 'symbol )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "L1";
         `Snterm (Fgram.obj (simple : 'simple Fgram.t ));
         `Sopt (`Snterm (Fgram.obj (sep_symbol : 'sep_symbol Fgram.t )))],
          ("let s::[] = s in\nlet styp = `App (_loc, (`Lid (_loc, \"list\")), (s.styp)) in\nlet text = mk_slist _loc (if l = \"L0\" then false else true) sep s in\n[mk_symbol ~text ~styp ~pattern:None]\n",
            (Fgram.mk_action
               (fun (sep : 'sep_symbol option)  (s : 'simple) 
                  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key l ->
                      (let s::[] = s in
                       let styp =
                         `App (_loc, (`Lid (_loc, "list")), (s.styp)) in
                       let text =
                         mk_slist _loc (if l = "L0" then false else true) sep
                           s in
                       [mk_symbol ~text ~styp ~pattern:None] : 'symbol )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "OPT"; `Snterm (Fgram.obj (simple : 'simple Fgram.t ))],
          ("let s::[] = s in\nlet styp = `App (_loc, (`Lid (_loc, \"option\")), (s.styp)) in\nlet text = `Sopt (_loc, (s.text)) in [mk_symbol ~text ~styp ~pattern:None]\n",
            (Fgram.mk_action
               (fun (s : 'simple)  _  (_loc : Locf.t)  ->
                  (let s::[] = s in
                   let styp = `App (_loc, (`Lid (_loc, "option")), (s.styp)) in
                   let text = `Sopt (_loc, (s.text)) in
                   [mk_symbol ~text ~styp ~pattern:None] : 'symbol )))));
        ([`Skeyword "TRY"; `Snterm (Fgram.obj (simple : 'simple Fgram.t ))],
          ("let s::[] = s in\nlet v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Stry v else `Speek v in\n[mk_symbol ~text ~styp:(s.styp) ~pattern:None]\n",
            (Fgram.mk_action
               (fun (s : 'simple)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key p ->
                      (let s::[] = s in
                       let v = (_loc, (s.text)) in
                       let text = if p = "TRY" then `Stry v else `Speek v in
                       [mk_symbol ~text ~styp:(s.styp) ~pattern:None] : 
                      'symbol )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "PEEK"; `Snterm (Fgram.obj (simple : 'simple Fgram.t ))],
          ("let s::[] = s in\nlet v = (_loc, (s.text)) in\nlet text = if p = \"TRY\" then `Stry v else `Speek v in\n[mk_symbol ~text ~styp:(s.styp) ~pattern:None]\n",
            (Fgram.mk_action
               (fun (s : 'simple)  (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key p ->
                      (let s::[] = s in
                       let v = (_loc, (s.text)) in
                       let text = if p = "TRY" then `Stry v else `Speek v in
                       [mk_symbol ~text ~styp:(s.styp) ~pattern:None] : 
                      'symbol )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Snterm (Fgram.obj (simple : 'simple Fgram.t ))],
          ("p\n",
            (Fgram.mk_action
               (fun (p : 'simple)  (_loc : Locf.t)  -> (p : 'symbol )))))]));
  Fgram.extend_single (brace_pattern : 'brace_pattern Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid i");
          `Skeyword "}"],
           ("`Lid (loc, i)\n",
             (Fgram.mk_action
                (fun _  (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                   match __fan_1 with
                   | `Lid (loc,i) -> (`Lid (loc, i) : 'brace_pattern )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (psymbol : 'psymbol Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (symbol : 'symbol Fgram.t ));
          `Sopt
            (`Snterm (Fgram.obj (brace_pattern : 'brace_pattern Fgram.t )))],
           ("List.map\n  (fun (s : Gram_def.symbol)  ->\n     match p with\n     | Some _ -> { s with pattern = (p : pat option ) }\n     | None  -> s) ss\n",
             (Fgram.mk_action
                (fun (p : 'brace_pattern option)  (ss : 'symbol) 
                   (_loc : Locf.t)  ->
                   (List.map
                      (fun (s : Gram_def.symbol)  ->
                         match p with
                         | Some _ -> { s with pattern = (p : pat option ) }
                         | None  -> s) ss : 'psymbol )))))]))
let _ =
  let grammar_entry_create x = Fgram.mk_dynamic g x in
  let str: 'str Fgram.t = grammar_entry_create "str"
  and left_rule: 'left_rule Fgram.t = grammar_entry_create "left_rule"
  and opt_action: 'opt_action Fgram.t = grammar_entry_create "opt_action" in
  Fgram.extend_single (str : 'str Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
               "`Str _")],
           ("y\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str y -> (y : 'str )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (extend_header : 'extend_header Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "(";
          `Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Skeyword ":";
          `Snterm (Fgram.obj (t_qualid : 't_qualid Fgram.t ));
          `Skeyword ")"],
           ("let old = gm () in let () = module_name := t in ((Some i), old)\n",
             (Fgram.mk_action
                (fun _  (t : 't_qualid)  _  (i : 'qualid)  _  (_loc : Locf.t)
                    ->
                   (let old = gm () in
                    let () = module_name := t in ((Some i), old) : 'extend_header )))));
        ([`Snterm (Fgram.obj (qualuid : 'qualuid Fgram.t ))],
          ("let old = gm () in let () = module_name := t in (None, old)\n",
            (Fgram.mk_action
               (fun (t : 'qualuid)  (_loc : Locf.t)  ->
                  (let old = gm () in
                   let () = module_name := t in (None, old) : 'extend_header )))));
        ([],
          ("(None, (gm ()))\n",
            (Fgram.mk_action
               (fun (_loc : Locf.t)  -> ((None, (gm ())) : 'extend_header )))))]));
  Fgram.extend_single (extend_body : 'extend_body Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
           ("let (gram,old) = rest in\nlet items = Listf.filter_map (fun x  -> x) el in\nlet res = make _loc { items; gram; safe = true } in\nlet () = module_name := old in res\n",
             (Fgram.mk_action
                (fun (el : 'entry list)  (rest : 'extend_header) 
                   (_loc : Locf.t)  ->
                   (let (gram,old) = rest in
                    let items = Listf.filter_map (fun x  -> x) el in
                    let res = make _loc { items; gram; safe = true } in
                    let () = module_name := old in res : 'extend_body )))))]));
  Fgram.extend_single (unsafe_extend_body : 'unsafe_extend_body Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (extend_header : 'extend_header Fgram.t ));
          `Slist1 (`Snterm (Fgram.obj (entry : 'entry Fgram.t )))],
           ("let (gram,old) = rest in\nlet items = Listf.filter_map (fun x  -> x) el in\nlet res = make _loc { items; gram; safe = false } in\nlet () = module_name := old in res\n",
             (Fgram.mk_action
                (fun (el : 'entry list)  (rest : 'extend_header) 
                   (_loc : Locf.t)  ->
                   (let (gram,old) = rest in
                    let items = Listf.filter_map (fun x  -> x) el in
                    let res = make _loc { items; gram; safe = false } in
                    let () = module_name := old in res : 'unsafe_extend_body )))))]));
  Fgram.extend_single (qualuid : 'qualuid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualuid)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualuid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
              "`Uid x")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Uid (_,x) -> (`Uid (_loc, x) : 'qualuid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (qualid : 'qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 'qualid)  _  (__fan_0 : Ftoken.t)  (_loc : Locf.t)
                    ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 'qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
              "`Lid i")],
          ("`Lid (_loc, i)\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Lid (_,i) -> (`Lid (_loc, i) : 'qualid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (t_qualid : 't_qualid Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
               "`Uid x");
          `Skeyword ".";
          `Sself],
           ("`Dot (_loc, (`Uid (_loc, x)), xs)\n",
             (Fgram.mk_action
                (fun (xs : 't_qualid)  _  (__fan_0 : Ftoken.t) 
                   (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Uid (_,x) ->
                       (`Dot (_loc, (`Uid (_loc, x)), xs) : 't_qualid )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Uid (_,_) -> true | _ -> false)), ("Uid", `Any),
              "`Uid x");
         `Skeyword ".";
         `Stoken
           (((function | `Lid (_,"t") -> true | _ -> false)),
             ("Lid", (`A "t")), "`Lid \"t\"")],
          ("`Uid (_loc, x)\n",
            (Fgram.mk_action
               (fun (__fan_2 : Ftoken.t)  _  (__fan_0 : Ftoken.t) 
                  (_loc : Locf.t)  ->
                  match (__fan_2, __fan_0) with
                  | (`Lid (_,"t"),`Uid (_,x)) ->
                      (`Uid (_loc, x) : 't_qualid )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s %s"
                           (Ftoken.token_to_string __fan_2)
                           (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (name : 'name Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ))],
           ("mk_name _loc il\n",
             (Fgram.mk_action
                (fun (il : 'qualid)  (_loc : Locf.t)  ->
                   (mk_name _loc il : 'name )))))]));
  Fgram.extend_single (entry_name : 'entry_name Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (qualid : 'qualid Fgram.t ));
          `Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )))],
           ("let x =\n  match name with\n  | Some x ->\n      let old = Ast_quotation.default.contents in\n      (match Ast_quotation.resolve_name ((`Sub []), x) with\n       | None  -> Locf.failf _loc \"DDSL `%s' not resolved\" x\n       | Some x -> (Ast_quotation.default := (Some x); `name old))\n  | None  -> `non in\n(x, (mk_name _loc il))\n",
             (Fgram.mk_action
                (fun (name : 'str option)  (il : 'qualid)  (_loc : Locf.t) 
                   ->
                   (let x =
                      match name with
                      | Some x ->
                          let old = Ast_quotation.default.contents in
                          (match Ast_quotation.resolve_name ((`Sub []), x)
                           with
                           | None  ->
                               Locf.failf _loc "DDSL `%s' not resolved" x
                           | Some x ->
                               (Ast_quotation.default := (Some x); `name old))
                      | None  -> `non in
                    (x, (mk_name _loc il)) : 'entry_name )))))]));
  Fgram.extend_single (entry : 'entry Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (entry_name : 'entry_name Fgram.t ));
          `Skeyword ":";
          `Sopt (`Snterm (Fgram.obj (position : 'position Fgram.t )));
          `Snterm (Fgram.obj (level_list : 'level_list Fgram.t ))],
           ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some (mk_entry ~local:false ~name:p ~pos ~levels))\n",
             (Fgram.mk_action
                (fun (levels : 'level_list)  (pos : 'position option)  _ 
                   (rest : 'entry_name)  (_loc : Locf.t)  ->
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
                     | _ -> Some (mk_entry ~local:false ~name:p ~pos ~levels)) : 
                   'entry )))));
        ([`Skeyword "let";
         `Snterm (Fgram.obj (entry_name : 'entry_name Fgram.t ));
         `Skeyword ":";
         `Sopt (`Snterm (Fgram.obj (position : 'position Fgram.t )));
         `Snterm (Fgram.obj (level_list : 'level_list Fgram.t ))],
          ("let (n,p) = rest in\n(match n with | `name old -> Ast_quotation.default := old | _ -> ());\n(match (pos, levels) with\n | (Some (`App (_loc,`Vrn (_,\"Level\"),_) : FAst.exp),`Group _) ->\n     failwithf \"For Group levels the position can not be applied to Level\"\n | _ -> Some (mk_entry ~local:true ~name:p ~pos ~levels))\n",
            (Fgram.mk_action
               (fun (levels : 'level_list)  (pos : 'position option)  _ 
                  (rest : 'entry_name)  _  (_loc : Locf.t)  ->
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
                    | _ -> Some (mk_entry ~local:true ~name:p ~pos ~levels)) : 
                  'entry )))));
        ([`Skeyword "Inline";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x");
         `Skeyword ":";
         `Snterm (Fgram.obj (rule_list : 'rule_list Fgram.t ))],
          ("Hashtbl.add inline_rules x rules; None\n",
            (Fgram.mk_action
               (fun (rules : 'rule_list)  _  (__fan_1 : Ftoken.t)  _ 
                  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Lid (_,x) ->
                      ((Hashtbl.add inline_rules x rules; None) : 'entry )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (position : 'position Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "First"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Last"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Before"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "After"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "Level"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'position )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (level_list : 'level_list Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "{";
          `Slist1 (`Snterm (Fgram.obj (level : 'level Fgram.t )));
          `Skeyword "}"],
           ("`Group ll\n",
             (Fgram.mk_action
                (fun _  (ll : 'level list)  _  (_loc : Locf.t)  ->
                   (`Group ll : 'level_list )))));
        ([`Snterm (Fgram.obj (level : 'level Fgram.t ))],
          ("`Single l\n",
            (Fgram.mk_action
               (fun (l : 'level)  (_loc : Locf.t)  ->
                  (`Single l : 'level_list )))))]));
  Fgram.extend_single (level : 'level Fgram.t )
    (None,
      (None, None,
        [([`Sopt (`Snterm (Fgram.obj (str : 'str Fgram.t )));
          `Sopt (`Snterm (Fgram.obj (assoc : 'assoc Fgram.t )));
          `Snterm (Fgram.obj (rule_list : 'rule_list Fgram.t ))],
           ("mk_level ~label ~assoc ~rules\n",
             (Fgram.mk_action
                (fun (rules : 'rule_list)  (assoc : 'assoc option) 
                   (label : 'str option)  (_loc : Locf.t)  ->
                   (mk_level ~label ~assoc ~rules : 'level )))))]));
  Fgram.extend_single (assoc : 'assoc Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "LA"],
           ("(`Vrn (_loc, x) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "RA"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))));
        ([`Skeyword "NA"],
          ("(`Vrn (_loc, x) : FAst.exp )\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Key x -> ((`Vrn (_loc, x) : FAst.exp ) : 'assoc )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (rule_list : 'rule_list Fgram.t )
    (None,
      (None, None,
        [([`Skeyword "["; `Skeyword "]"],
           ("[]\n",
             (Fgram.mk_action
                (fun _  _  (_loc : Locf.t)  -> ([] : 'rule_list )))));
        ([`Skeyword "[";
         `Slist1sep
           ((`Snterm (Fgram.obj (rule : 'rule Fgram.t ))), (`Skeyword "|"));
         `Skeyword "]"],
          ("Listf.concat ruless\n",
            (Fgram.mk_action
               (fun _  (ruless : 'rule list)  _  (_loc : Locf.t)  ->
                  (Listf.concat ruless : 'rule_list )))))]));
  Fgram.extend_single (rule : 'rule Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (left_rule : 'left_rule Fgram.t ));
          `Sopt (`Snterm (Fgram.obj (opt_action : 'opt_action Fgram.t )))],
           ("let prods = Listf.cross prod in\nList.map (fun prod  -> mk_rule ~prod ~action) prods\n",
             (Fgram.mk_action
                (fun (action : 'opt_action option)  (prod : 'left_rule) 
                   (_loc : Locf.t)  ->
                   (let prods = Listf.cross prod in
                    List.map (fun prod  -> mk_rule ~prod ~action) prods : 
                   'rule )))));
        ([`Skeyword "@";
         `Stoken
           (((function | `Lid (_,_) -> true | _ -> false)), ("Lid", `Any),
             "`Lid x")],
          ("match query_inline x with\n| Some x -> x\n| None  -> Locf.failf _loc \"inline rules %s not found\" x\n",
            (Fgram.mk_action
               (fun (__fan_1 : Ftoken.t)  _  (_loc : Locf.t)  ->
                  match __fan_1 with
                  | `Lid (_,x) ->
                      ((match query_inline x with
                        | Some x -> x
                        | None  ->
                            Locf.failf _loc "inline rules %s not found" x) : 
                      'rule )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_1))))))]));
  Fgram.extend_single (left_rule : 'left_rule Fgram.t )
    (None,
      (None, None,
        [([`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ))],
           ("[x]\n",
             (Fgram.mk_action
                (fun (x : 'psymbol)  (_loc : Locf.t)  -> ([x] : 'left_rule )))));
        ([`Snterm (Fgram.obj (psymbol : 'psymbol Fgram.t ));
         `Skeyword ";";
         `Sself],
          ("x :: xs\n",
            (Fgram.mk_action
               (fun (xs : 'left_rule)  _  (x : 'psymbol)  (_loc : Locf.t)  ->
                  (x :: xs : 'left_rule )))));
        ([],
          ("[]\n",
            (Fgram.mk_action (fun (_loc : Locf.t)  -> ([] : 'left_rule )))))]));
  Fgram.extend_single (opt_action : 'opt_action Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Quot _ -> true | _ -> false)), ("Quot", `Any),
               "`Quot _")],
           ("if x.name = Ftoken.empty_name\nthen\n  let expander loc _ s = Fgram.parse_string ~loc Syntaxf.exp s in\n  Ftoken.quot_expand expander x\nelse Ast_quotation.expand x Dyn_tag.exp\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Quot x ->
                       (if x.name = Ftoken.empty_name
                        then
                          let expander loc _ s =
                            Fgram.parse_string ~loc Syntaxf.exp s in
                          Ftoken.quot_expand expander x
                        else Ast_quotation.expand x Dyn_tag.exp : 'opt_action )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))))]));
  Fgram.extend_single (string : 'string Fgram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `Str _ -> true | _ -> false)), ("Str", `Any),
               "`Str _")],
           ("(`Str (_loc, s) : FAst.exp )\n",
             (Fgram.mk_action
                (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                   match __fan_0 with
                   | `Str s -> ((`Str (_loc, s) : FAst.exp ) : 'string )
                   | _ ->
                       failwith
                         (Printf.sprintf "%s"
                            (Ftoken.token_to_string __fan_0))))));
        ([`Stoken
            (((function | `Ant ("",_) -> true | _ -> false)),
              ("Ant", (`A "")), "`Ant (\"\",_)")],
          ("Parsef.exp _loc s\n",
            (Fgram.mk_action
               (fun (__fan_0 : Ftoken.t)  (_loc : Locf.t)  ->
                  match __fan_0 with
                  | `Ant ("",s) -> (Parsef.exp _loc s : 'string )
                  | _ ->
                      failwith
                        (Printf.sprintf "%s" (Ftoken.token_to_string __fan_0))))))]))
let _ =
  let d = Ns.lang in
  Ast_quotation.of_exp ~name:(d, "extend") ~entry:extend_body ();
  Ast_quotation.of_exp ~name:(d, "unsafe_extend") ~entry:unsafe_extend_body
    ()