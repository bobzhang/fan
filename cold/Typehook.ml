open LibUtil

open Format

open FSigUtil

let filters: (plugin_name,plugin) Hashtbl.t = Hashtbl.create 30

let show_code = ref false

let print_collect_mtyps = ref false

let register ?filter  ?position  (name,transform) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else Hashtbl.add filters name { transform; position; filter }

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
    method get_cur_mtyps : mtyps
    method get_cur_and_types : and_types
    method update_cur_and_types : (and_types -> and_types) -> unit
    method update_cur_mtyps : (mtyps -> mtyps) -> unit
  end

let iterate_code sloc mtyps (_,{ position; transform; filter }) acc =
  let mtyps =
    match filter with
    | Some x -> FSigUtil.apply_filter x mtyps
    | None  -> mtyps in
  let code = transform mtyps in
  match (position, code) with
  | (Some x,Some code) ->
      let (name,f) = Filters.make_filter (x, code) in
      (AstFilters.register_stru_filter (name, f);
       AstFilters.use_implem_filter name;
       acc)
  | (None ,Some code) ->
      let code = FanAstN.fill_loc_stru sloc code in
      (`Sem (sloc, acc, code) : Ast.stru )
  | (_,None ) -> acc

let traversal () =
  (object (self : 'self_type)
     inherit  Objs.map as super
     val mtyps_stack = (Stack.create () : mtyps Stack.t )
     val mutable cur_and_types = ([] : and_types )
     val mutable and_group = false
     method get_cur_mtyps : mtyps= Stack.top mtyps_stack
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
       | (`Struct (sloc,u) : Ast.mexp) ->
           (self#in_module;
            (let res = self#stru u in
             let mtyps = List.rev self#get_cur_mtyps in
             let () =
               if print_collect_mtyps.contents
               then eprintf "@[%a@]@." pp_print_mtyps mtyps in
             let result =
               List.fold_right (iterate_code sloc mtyps)
                 FanState.current_filters.contents
                 (if FanState.keep.contents
                  then res
                  else (`StExp (sloc, (`Uid (sloc, "()"))) : Ast.stru )) in
             self#out_module; (`Struct (sloc, result) : Ast.mexp )))
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
       | `TypeWith (_loc,typedecl,_) -> self#stru (`Type (_loc, typedecl))
       | (`Type (_loc,(`TyDcl (_,`Lid (_,name),_,_,_) as t)) : Ast.stru) as x
           ->
           let item = `Single (name, (Objs.strip_loc_typedecl t)) in
           let () =
             if print_collect_mtyps.contents
             then eprintf "Came across @[%a@]@." pp_print_types item in
           (self#update_cur_mtyps (fun lst  -> item :: lst); x)
       | (`Value (_loc,`Negative _,_) : Ast.stru)
         |(`ModuleType (_loc,_,_) : Ast.stru)|(`Include (_loc,_) : Ast.stru)
         |(`External (_loc,_,_,_) : Ast.stru)|(`StExp (_loc,_) : Ast.stru)
         |(`Exception (_loc,_) : Ast.stru)|(`Directive (_loc,_,_) : Ast.stru)
           as x -> x
       | x -> super#stru x
     method! typedecl =
       function
       | `TyDcl (_,`Lid (_,name),_,_,_) as t ->
           (if self#is_in_and_types
            then
              self#update_cur_and_types
                (fun lst  -> (name, (Objs.strip_loc_typedecl t)) :: lst);
            t)
       | t -> super#typedecl t
   end : traversal )