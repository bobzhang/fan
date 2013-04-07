open LibUtil

open AstLoc

open FSig

open Format

let apply_filter f (m : module_types) =
  (let f =
     function
     | `Single (s,_) as x -> if f s then Some x else None
     | `Mutual ls ->
         let x =
           List.filter_map
             (fun ((s,_) as x)  -> if f s then Some x else None) ls in
         (match x with
          | [] -> None
          | x::[] -> Some (`Single x)
          | y -> Some (`Mutual y)) in
   List.filter_map f m : module_types )

let filters: (plugin_name,plugin) Hashtbl.t = Hashtbl.create 30

let show_code = ref false

let print_collect_module_types = ref false

let register ?filter  ?position  (name,f) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else Hashtbl.add filters name { transform = f; position; filter }

let show_modules () =
  Hashtbl.iter (fun key  _  -> Format.printf "%s@ " key) filters;
  print_newline ()

let plugin_add plugin =
  (try
     let v = Hashtbl.find filters plugin in
     fun ()  ->
       if
         not
           (List.exists (fun (n,_)  -> n = plugin)
              FanState.current_filters.contents)
       then
         Ref.modify FanState.current_filters (fun x  -> cons (plugin, v) x)
       else eprintf "<Warning> plugin %s has already been loaded" plugin
   with
   | Not_found  ->
       (fun ()  -> show_modules (); failwithf "plugins %s not found " plugin))
    ()

let plugin_remove plugin =
  Ref.modify FanState.current_filters (fun x  -> List.remove plugin x)

class type traversal
  =
  object 
    inherit Objs.map
    method get_cur_module_types : FSig.module_types
    method get_cur_and_types : FSig.and_types
    method update_cur_and_types : (FSig.and_types -> FSig.and_types) -> unit
    method update_cur_module_types :
      (FSig.module_types -> FSig.module_types) -> unit
  end

let traversal () =
  (object (self : 'self_type)
     inherit  Objs.map as super
     val module_types_stack = (Stack.create () : module_types Stack.t )
     val mutable cur_and_types = ([] : and_types )
     val mutable and_group = false
     method get_cur_module_types : module_types= Stack.top module_types_stack
     method update_cur_module_types f =
       let open Stack in push (f (pop module_types_stack)) module_types_stack
     method private in_module = Stack.push [] module_types_stack
     method private out_module = (Stack.pop module_types_stack) |> ignore
     method private in_and_types = and_group <- true; cur_and_types <- []
     method private out_and_types = and_group <- false; cur_and_types <- []
     method private is_in_and_types = and_group
     method get_cur_and_types = cur_and_types
     method update_cur_and_types f = cur_and_types <- f cur_and_types
     method! module_exp =
       function
       | `Struct (_loc,u) ->
           (self#in_module;
            (let res = self#stru u in
             let module_types = List.rev self#get_cur_module_types in
             if print_collect_module_types.contents
             then eprintf "@[%a@]@." FSig.pp_print_module_types module_types;
             (let result =
                List.fold_right
                  (fun (_,{ position; transform; filter })  acc  ->
                     let module_types =
                       match filter with
                       | Some x -> apply_filter x module_types
                       | None  -> module_types in
                     let code = transform module_types in
                     match position with
                     | Some x ->
                         let (name,f) = Filters.make_filter (x, code) in
                         (AstFilters.register_stru_filter (name, f);
                          AstFilters.use_implem_filter name;
                          acc)
                     | None  -> `Sem (_loc, acc, code))
                  FanState.current_filters.contents
                  (if FanState.keep.contents
                   then res
                   else `StExp (_loc, (`Id (_loc, (`Uid (_loc, "()")))))) in
              self#out_module; `Struct (_loc, result))))
       | x -> super#module_exp x
     method! stru =
       function
       | `Type (_loc,`And (_,_,_)) as x ->
           (self#in_and_types;
            (let _ = super#stru x in
             self#update_cur_module_types
               (fun lst  -> (`Mutual (List.rev self#get_cur_and_types)) ::
                  lst);
             self#out_and_types;
             if FanState.keep.contents
             then x
             else `StExp (_loc, (`Id (_loc, (`Uid (_loc, "()")))))))
       | `Type (_loc,(`TyDcl (_,`Lid (_,name),_,_,_) as t)) as x ->
           let item = `Single (name, t) in
           (if print_collect_module_types.contents
            then eprintf "Came across @[%a@]@." FSig.pp_print_types item;
            self#update_cur_module_types (fun lst  -> item :: lst);
            x)
       | `Value (_loc,`ReNil _,_)|`ModuleType (_loc,_,_)|`Include (_loc,_)
         |`External (_loc,_,_,_)|`StExp (_loc,_)|`Exception (_loc,_)
         |`Directive (_loc,_,_) as x -> x
       | x -> super#stru x
     method! typedecl =
       function
       | `TyDcl (_,`Lid (_,name),_,_,_) as t ->
           (if self#is_in_and_types
            then self#update_cur_and_types (fun lst  -> (name, t) :: lst)
            else ();
            t)
       | t -> super#typedecl t
   end : traversal )

let g =
  Gram.create_lexer
    ~keywords:["derive";
              "unload";
              "clear";
              "keep";
              "on";
              "keep";
              "off";
              "show_code";
              "(";
              ")";
              ",";
              ";"] ~annot:"derive" ()

let fan_quot = Gram.mk_dynamic g "fan_quot"

let fan_quots = Gram.mk_dynamic g "fan_quots"

let _ =
  Gram.extend_single (fan_quot : 'fan_quot Gram.t )
    (None,
      (None, None,
        [([`Skeyword "derive";
          `Skeyword "(";
          `Slist1
            (Gram.srules
               [([`Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"))],
                  ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Lid x -> (x : 'e__1 ) | _ -> failwith \"x\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Lid x -> (x : 'e__1 )
                          | _ -> failwith "x\n"))));
               ([`Stoken
                   (((function | `Uid _ -> true | _ -> false)),
                     (`Normal, "`Uid _"))],
                 ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid x -> (x : 'e__1 ) | _ -> failwith \"x\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid x -> (x : 'e__1 )
                         | _ -> failwith "x\n"))))]);
          `Skeyword ")"],
           ("Gram.mk_action\n  (fun _  (plugins : 'e__1 list)  _  _  (_loc : FanLoc.t)  ->\n     (List.iter plugin_add plugins; `Id (_loc, (`Uid (_loc, \"()\"))) : \n     'fan_quot ))\n",
             (Gram.mk_action
                (fun _  (plugins : 'e__1 list)  _  _  (_loc : FanLoc.t)  ->
                   (List.iter plugin_add plugins;
                    `Id (_loc, (`Uid (_loc, "()"))) : 'fan_quot )))));
        ([`Skeyword "unload";
         `Slist1sep
           ((Gram.srules
               [([`Stoken
                    (((function | `Lid _ -> true | _ -> false)),
                      (`Normal, "`Lid _"))],
                  ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Lid x -> (x : 'e__2 ) | _ -> failwith \"x\n\")\n",
                    (Gram.mk_action
                       (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                          match __fan_0 with
                          | `Lid x -> (x : 'e__2 )
                          | _ -> failwith "x\n"))));
               ([`Stoken
                   (((function | `Uid _ -> true | _ -> false)),
                     (`Normal, "`Uid _"))],
                 ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Uid x -> (x : 'e__2 ) | _ -> failwith \"x\n\")\n",
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `Uid x -> (x : 'e__2 )
                         | _ -> failwith "x\n"))))]), (`Skeyword ","))],
          ("Gram.mk_action\n  (fun (plugins : 'e__2 list)  _  (_loc : FanLoc.t)  ->\n     (List.iter plugin_remove plugins; `Id (_loc, (`Uid (_loc, \"()\"))) : \n     'fan_quot ))\n",
            (Gram.mk_action
               (fun (plugins : 'e__2 list)  _  (_loc : FanLoc.t)  ->
                  (List.iter plugin_remove plugins;
                   `Id (_loc, (`Uid (_loc, "()"))) : 'fan_quot )))));
        ([`Skeyword "clear"],
          ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  ->\n     (FanState.reset_current_filters (); `Id (_loc, (`Uid (_loc, \"()\"))) : \n     'fan_quot ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (FanState.reset_current_filters ();
                   `Id (_loc, (`Uid (_loc, "()"))) : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "on"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (FanState.keep := true; `Id (_loc, (`Uid (_loc, \"()\"))) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (FanState.keep := true; `Id (_loc, (`Uid (_loc, "()"))) : 
                  'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "off"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (FanState.keep := false; `Id (_loc, (`Uid (_loc, \"()\"))) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (FanState.keep := false; `Id (_loc, (`Uid (_loc, "()"))) : 
                  'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "on"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (show_code := true; `Id (_loc, (`Uid (_loc, \"()\"))) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (show_code := true; `Id (_loc, (`Uid (_loc, "()"))) : 
                  'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "off"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (show_code := false; `Id (_loc, (`Uid (_loc, \"()\"))) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (show_code := false; `Id (_loc, (`Uid (_loc, "()"))) : 
                  'fan_quot )))))]));
  Gram.extend_single (fan_quots : 'fan_quots Gram.t )
    (None,
      (None, None,
        [([`Slist1
             (Gram.srules
                [([`Snterm (Gram.obj (fan_quot : 'fan_quot Gram.t ));
                  `Skeyword ";"],
                   ("Gram.mk_action (fun _  (x : 'fan_quot)  (_loc : FanLoc.t)  -> (x : 'e__3 ))\n",
                     (Gram.mk_action
                        (fun _  (x : 'fan_quot)  (_loc : FanLoc.t)  ->
                           (x : 'e__3 )))))])],
           ("Gram.mk_action\n  (fun (xs : 'e__3 list)  (_loc : FanLoc.t)  -> (seq_sem xs : 'fan_quots ))\n",
             (Gram.mk_action
                (fun (xs : 'e__3 list)  (_loc : FanLoc.t)  ->
                   (seq_sem xs : 'fan_quots )))))]))

let g = Gram.create_lexer ~annot:"include" ~keywords:[] ()

let include_quot = Gram.mk_dynamic g "include_quot"

let _ =
  Gram.extend_single (include_quot : 'include_quot Gram.t )
    (None,
      (None, None,
        [([`Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
           ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with\n     | `STR (_,s) ->\n         (let keep = FanState.keep and cf = FanState.current_filters in\n          let fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\n          (try\n             let fan_res__2 =\n               FanState.reset ();\n               FanBasic.parse_include_file PreCast.Syntax.strus s in\n             let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\n           with\n           | fan_e__3 ->\n               ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)) : \n         'include_quot )\n     | _ ->\n         failwith\n           \"let keep = FanState.keep and cf = FanState.current_filters in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    FanState.reset (); FanBasic.parse_include_file PreCast.Syntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n\")\n",
             (Gram.mk_action
                (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                   match __fan_0 with
                   | `STR (_,s) ->
                       (let keep = FanState.keep
                        and cf = FanState.current_filters in
                        let fan_keep__0 = keep.contents
                        and fan_cf__1 = cf.contents in
                        (try
                           let fan_res__2 =
                             FanState.reset ();
                             FanBasic.parse_include_file PreCast.Syntax.strus
                               s in
                           let _ = keep := fan_keep__0; cf := fan_cf__1 in
                           fan_res__2
                         with
                         | fan_e__3 ->
                             ((keep := fan_keep__0; cf := fan_cf__1);
                              raise fan_e__3)) : 'include_quot )
                   | _ ->
                       failwith
                         "let keep = FanState.keep and cf = FanState.current_filters in\nlet fan_keep__0 = keep.contents and fan_cf__1 = cf.contents in\ntry\n  let fan_res__2 =\n    FanState.reset (); FanBasic.parse_include_file PreCast.Syntax.strus s in\n  let _ = keep := fan_keep__0; cf := fan_cf__1 in fan_res__2\nwith | fan_e__3 -> ((keep := fan_keep__0; cf := fan_cf__1); raise fan_e__3)\n"))))]))

let save_quot = Gram.mk "save_quot"

let _ =
  Gram.extend_single (save_quot : 'save_quot Gram.t )
    (None,
      (None, None,
        [([`Slist1
             (Gram.srules
                [([`Stoken
                     (((function | `Lid _ -> true | _ -> false)),
                       (`Normal, "`Lid _"))],
                   ("Gram.mk_action\n  (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->\n     match __fan_0 with | `Lid x -> (x : 'e__4 ) | _ -> failwith \"x\n\")\n",
                     (Gram.mk_action
                        (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t) 
                           ->
                           match __fan_0 with
                           | `Lid x -> (x : 'e__4 )
                           | _ -> failwith "x\n"))))]);
          `Skeyword "->";
          `Snterm (Gram.obj (Syntax.exp : 'Syntax__exp Gram.t ))],
           ("Gram.mk_action\n  (fun (b : 'Syntax__exp)  _  (ls : 'e__4 list)  (_loc : FanLoc.t)  ->\n     (let symbs = List.map (fun x  -> FanState.gensym x) ls in\n      let res = FanState.gensym \"res\" in\n      let exc = FanState.gensym \"e\" in\n      let binds =\n        and_of_list\n          (List.map2\n             (fun x  y  ->\n                `Bind\n                  (_loc, (`Id (_loc, (`Lid (_loc, x)))),\n                    (`Dot\n                       (_loc, (`Id (_loc, (`Lid (_loc, y)))),\n                         (`Id (_loc, (`Lid (_loc, \"contents\")))))))) symbs ls) in\n      let restore =\n        seq_sem\n          (List.map2\n             (fun x  y  ->\n                `Assign\n                  (_loc,\n                    (`Dot\n                       (_loc, (`Id (_loc, (`Lid (_loc, x)))),\n                         (`Id (_loc, (`Lid (_loc, \"contents\")))))),\n                    (`Id (_loc, (`Lid (_loc, y)))))) ls symbs) in\n      `LetIn\n        (_loc, (`ReNil _loc), binds,\n          (`Try\n             (_loc,\n               (`LetIn\n                  (_loc, (`ReNil _loc),\n                    (`Bind (_loc, (`Id (_loc, (`Lid (_loc, res)))), b)),\n                    (`LetIn\n                       (_loc, (`ReNil _loc),\n                         (`Bind (_loc, (`Any _loc), restore)),\n                         (`Id (_loc, (`Lid (_loc, res)))))))),\n               (`Case\n                  (_loc, (`Id (_loc, (`Lid (_loc, exc)))),\n                    (`Seq\n                       (_loc,\n                         (`Sem\n                            (_loc, restore,\n                              (`App\n                                 (_loc, (`Id (_loc, (`Lid (_loc, \"raise\")))),\n                                   (`Id (_loc, (`Lid (_loc, exc))))))))))))))) : \n     'save_quot ))\n",
             (Gram.mk_action
                (fun (b : 'Syntax__exp)  _  (ls : 'e__4 list) 
                   (_loc : FanLoc.t)  ->
                   (let symbs = List.map (fun x  -> FanState.gensym x) ls in
                    let res = FanState.gensym "res" in
                    let exc = FanState.gensym "e" in
                    let binds =
                      and_of_list
                        (List.map2
                           (fun x  y  ->
                              `Bind
                                (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                  (`Dot
                                     (_loc, (`Id (_loc, (`Lid (_loc, y)))),
                                       (`Id (_loc, (`Lid (_loc, "contents"))))))))
                           symbs ls) in
                    let restore =
                      seq_sem
                        (List.map2
                           (fun x  y  ->
                              `Assign
                                (_loc,
                                  (`Dot
                                     (_loc, (`Id (_loc, (`Lid (_loc, x)))),
                                       (`Id (_loc, (`Lid (_loc, "contents")))))),
                                  (`Id (_loc, (`Lid (_loc, y)))))) ls symbs) in
                    `LetIn
                      (_loc, (`ReNil _loc), binds,
                        (`Try
                           (_loc,
                             (`LetIn
                                (_loc, (`ReNil _loc),
                                  (`Bind
                                     (_loc, (`Id (_loc, (`Lid (_loc, res)))),
                                       b)),
                                  (`LetIn
                                     (_loc, (`ReNil _loc),
                                       (`Bind (_loc, (`Any _loc), restore)),
                                       (`Id (_loc, (`Lid (_loc, res)))))))),
                             (`Case
                                (_loc, (`Id (_loc, (`Lid (_loc, exc)))),
                                  (`Seq
                                     (_loc,
                                       (`Sem
                                          (_loc, restore,
                                            (`App
                                               (_loc,
                                                 (`Id
                                                    (_loc,
                                                      (`Lid (_loc, "raise")))),
                                                 (`Id
                                                    (_loc,
                                                      (`Lid (_loc, exc))))))))))))))) : 
                   'save_quot )))))]))

let _ =
  PreCast.Syntax.Options.add
    ("-keep", (FanArg.Set FanState.keep),
      "Keep the included type definitions");
  PreCast.Syntax.Options.add
    ("-loaded-plugins", (FanArg.Unit show_modules), "Show plugins")