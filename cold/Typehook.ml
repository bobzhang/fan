open LibUtil

open AstLib

open Format

let apply_filter f (m : FSig.mtyps) =
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
   List.filter_map f m : FSig.mtyps )

let filters: (FSig.plugin_name,FSig.plugin) Hashtbl.t = Hashtbl.create 30

let show_code = ref false

let print_collect_mtyps = ref false

let register ?filter  ?position  (name,transform) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else
    Hashtbl.add filters name { FSig.transform = transform; position; filter }

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
    method get_cur_mtyps : FSig.mtyps
    method get_cur_and_types : FSig.and_types
    method update_cur_and_types : (FSig.and_types -> FSig.and_types) -> unit
    method update_cur_mtyps : (FSig.mtyps -> FSig.mtyps) -> unit
  end

let traversal () =
  (object (self : 'self_type)
     inherit  Objs.map as super
     val mtyps_stack = (Stack.create () : FSig.mtyps Stack.t )
     val mutable cur_and_types = ([] : FSig.and_types )
     val mutable and_group = false
     method get_cur_mtyps : FSig.mtyps= Stack.top mtyps_stack
     method update_cur_mtyps f =
       let open Stack in push (f (pop mtyps_stack)) mtyps_stack
     method private in_module = Stack.push [] mtyps_stack
     method private out_module = ignore (Stack.pop mtyps_stack)
     method private in_and_types = and_group <- true; cur_and_types <- []
     method private out_and_types = and_group <- false; cur_and_types <- []
     method private is_in_and_types = and_group
     method get_cur_and_types = cur_and_types
     method update_cur_and_types f = cur_and_types <- f cur_and_types
     method! mexp =
       function
       | (`Struct (_loc,u) : Ast.mexp) ->
           (self#in_module;
            (let res = self#stru u in
             let mtyps = List.rev self#get_cur_mtyps in
             let () =
               if print_collect_mtyps.contents
               then eprintf "@[%a@]@." FSig.pp_print_mtyps mtyps in
             let result =
               List.fold_right
                 (fun (_,{ FSig.position = position; transform; filter }) 
                    acc  ->
                    let mtyps =
                      match filter with
                      | Some x -> apply_filter x mtyps
                      | None  -> mtyps in
                    let code = transform mtyps in
                    match (position, code) with
                    | (Some x,Some code) ->
                        let (name,f) = Filters.make_filter (x, code) in
                        (AstFilters.register_stru_filter (name, f);
                         AstFilters.use_implem_filter name;
                         acc)
                    | (None ,Some code) ->
                        (`Sem (_loc, acc, code) : Ast.stru )
                    | (_,None ) -> acc) FanState.current_filters.contents
                 (if FanState.keep.contents
                  then res
                  else (`StExp (_loc, (`Uid (_loc, "()"))) : Ast.stru )) in
             self#out_module; (`Struct (_loc, result) : Ast.mexp )))
       | x -> super#mexp x
     method! stru =
       function
       | (`Type (_loc,`And (_,_,_)) : Ast.stru) as x ->
           (self#in_and_types;
            (let _ = super#stru x in
             self#update_cur_mtyps
               (fun lst  -> (`Mutual (List.rev self#get_cur_and_types)) ::
                  lst);
             self#out_and_types;
             if FanState.keep.contents
             then x
             else (`StExp (_loc, (`Uid (_loc, "()"))) : Ast.stru )))
       | (`Type (_loc,(`TyDcl (_,`Lid (_,name),_,_,_) as t)) : Ast.stru) as x
           ->
           let item = `Single (name, t) in
           let () =
             if print_collect_mtyps.contents
             then eprintf "Came across @[%a@]@." FSig.pp_print_types item in
           (self#update_cur_mtyps (fun lst  -> item :: lst); x)
       | (`Value (_loc,`Negative _,_) : Ast.stru)
         |(`ModuleType (_loc,_,_) : Ast.stru)|(`Include (_loc,_) : Ast.stru)
         |(`External (_loc,_,_,_) : Ast.stru)|(`StExp (_loc,_) : Ast.stru)
         |`Exception (_loc,_)|(`Directive (_loc,_,_) : Ast.stru) as x -> x
       | x -> super#stru x
     method! typedecl =
       function
       | `TyDcl (_,`Lid (_,name),_,_,_) as t ->
           (if self#is_in_and_types
            then self#update_cur_and_types (fun lst  -> (name, t) :: lst);
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
           ("Gram.mk_action\n  (fun _  (plugins : 'e__1 list)  _  _  (_loc : FanLoc.t)  ->\n     (List.iter plugin_add plugins; (`Uid (_loc, \"()\") : Ast.exp ) : \n     'fan_quot ))\n",
             (Gram.mk_action
                (fun _  (plugins : 'e__1 list)  _  _  (_loc : FanLoc.t)  ->
                   (List.iter plugin_add plugins;
                    (`Uid (_loc, "()") : Ast.exp ) : 'fan_quot )))));
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
          ("Gram.mk_action\n  (fun (plugins : 'e__2 list)  _  (_loc : FanLoc.t)  ->\n     (List.iter plugin_remove plugins; (`Uid (_loc, \"()\") : Ast.exp ) : \n     'fan_quot ))\n",
            (Gram.mk_action
               (fun (plugins : 'e__2 list)  _  (_loc : FanLoc.t)  ->
                  (List.iter plugin_remove plugins;
                   (`Uid (_loc, "()") : Ast.exp ) : 'fan_quot )))));
        ([`Skeyword "clear"],
          ("Gram.mk_action\n  (fun _  (_loc : FanLoc.t)  ->\n     (FanState.reset_current_filters (); (`Uid (_loc, \"()\") : Ast.exp ) : \n     'fan_quot ))\n",
            (Gram.mk_action
               (fun _  (_loc : FanLoc.t)  ->
                  (FanState.reset_current_filters ();
                   (`Uid (_loc, "()") : Ast.exp ) : 'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "on"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (FanState.keep := true; (`Uid (_loc, \"()\") : Ast.exp ) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (FanState.keep := true; (`Uid (_loc, "()") : Ast.exp ) : 
                  'fan_quot )))));
        ([`Skeyword "keep"; `Skeyword "off"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (FanState.keep := false; (`Uid (_loc, \"()\") : Ast.exp ) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (FanState.keep := false; (`Uid (_loc, "()") : Ast.exp ) : 
                  'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "on"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (show_code := true; (`Uid (_loc, \"()\") : Ast.exp ) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (show_code := true; (`Uid (_loc, "()") : Ast.exp ) : 
                  'fan_quot )))));
        ([`Skeyword "show_code"; `Skeyword "off"],
          ("Gram.mk_action\n  (fun _  _  (_loc : FanLoc.t)  ->\n     (show_code := false; (`Uid (_loc, \"()\") : Ast.exp ) : 'fan_quot ))\n",
            (Gram.mk_action
               (fun _  _  (_loc : FanLoc.t)  ->
                  (show_code := false; (`Uid (_loc, "()") : Ast.exp ) : 
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

let _ =
  Syntax.Options.add
    ("-keep", (FanArg.Set FanState.keep),
      "Keep the included type definitions");
  Syntax.Options.add
    ("-loaded-plugins", (FanArg.Unit show_modules), "Show plugins")