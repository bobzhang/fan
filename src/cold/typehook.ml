let pp_print_mtyps = Sigs_util.pp_print_mtyps
let pp_print_types = Sigs_util.pp_print_types
let eprintf = Format.eprintf
open Util
open Ast_basic
let filters: (Sigs_util.plugin_name,Sigs_util.plugin) Hashtbl.t =
  Hashtbl.create 30
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
         not @@
           (List.exists (fun (n,_)  -> n = plugin) (!State.current_filters))
       then Ref.modify State.current_filters (fun x  -> cons (plugin, v) x)
       else eprintf "<Warning> plugin %s has already been loaded" plugin
   with
   | Not_found  ->
       (fun ()  -> show_modules (); failwithf "plugins %s not found " plugin))
    ()
let plugin_remove plugin =
  Ref.modify State.current_filters (fun x  -> Listf.remove plugin x)
class type traversal
  =
  object 
    inherit Objs.map
    method get_cur_mtyps : Sigs_util.mtyps
    method get_cur_and_types : Sigs_util.and_types
    method update_cur_and_types :
      (Sigs_util.and_types -> Sigs_util.and_types) -> unit
    method update_cur_mtyps : (Sigs_util.mtyps -> Sigs_util.mtyps) -> unit
  end
let make_filter (s,code) =
  let f =
    function
    | (`StExp (_loc,`Lid (_,s')) : Astf.stru) when s = s' ->
        FanAstN.fill_stru _loc code
    | e -> e in
  (("filter_" ^ s), ((Objs.map_stru f)#stru))
let iterate_code sloc mtyps (_,(x : Sigs_util.plugin)) acc =
  let mtyps =
    match x.filter with
    | Some x -> Sigs_util.apply_filter x mtyps
    | None  -> mtyps in
  let code = x.transform mtyps in
  match ((x.position), code) with
  | (Some x,Some code) ->
      let (name,f) = make_filter (x, code) in
      (Ast_filters.register_stru_filter (name, f);
       Ast_filters.use_implem_filter name;
       acc)
  | (None ,Some code) ->
      let code = FanAstN.fill_stru sloc code in
      (`Sem (sloc, acc, code) : Astf.stru )
  | (_,None ) -> acc
let traversal () =
  (object (self : 'self_type)
     inherit  Objs.map as super
     val mtyps_stack = (Stack.create () : Sigs_util.mtyps Stack.t )
     val mutable cur_and_types = ([] : Sigs_util.and_types )
     val mutable and_group = false
     method get_cur_mtyps : Sigs_util.mtyps= Stack.top mtyps_stack
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
       | (`Struct (sloc,u) : Astf.mexp) ->
           (self#in_module;
            (let res = self#stru u in
             let mtyps = List.rev self#get_cur_mtyps in
             let () =
               if !print_collect_mtyps
               then eprintf "@[%a@]@." pp_print_mtyps mtyps in
             let result =
               List.fold_right (iterate_code sloc mtyps)
                 (!State.current_filters)
                 (if !State.keep
                  then res
                  else (`StExp (sloc, (`Unit sloc)) : Astf.stru )) in
             self#out_module; (`Struct (sloc, result) : Astf.mexp )))
       | x -> super#mexp x
     method! stru =
       function
       | (`Type (_loc,`And (_,_,_)) : Astf.stru) as x ->
           (self#in_and_types;
            (let _ = super#stru x in
             self#update_cur_mtyps
               (fun lst  -> (`Mutual (List.rev self#get_cur_and_types)) ::
                  lst);
             self#out_and_types;
             if !State.keep
             then x
             else (`StExp (_loc, (`Unit _loc)) : Astf.stru )))
       | `TypeWith (_loc,typedecl,_) -> self#stru (`Type (_loc, typedecl))
       | (`Type (_loc,(`TyDcl (_,`Lid (_,name),_,_,_) as t)) : Astf.stru) as
           x ->
           let item = `Single (name, (Objs.strip_typedecl t)) in
           let () =
             if !print_collect_mtyps
             then eprintf "Came across @[%a@]@." pp_print_types item in
           (self#update_cur_mtyps (fun lst  -> item :: lst); x)
       | (`Value (_loc,`Negative _,_) : Astf.stru)
         |(`ModuleType (_loc,_,_) : Astf.stru)
         |(`Include (_loc,_) : Astf.stru)
         |(`External (_loc,_,_,_) : Astf.stru)|(`StExp (_loc,_) : Astf.stru)
         |(`Exception (_loc,_) : Astf.stru)
         |(`Directive (_loc,_,_) : Astf.stru) as x -> x
       | x -> super#stru x
     method! typedecl =
       function
       | `TyDcl (_,`Lid (_,name),_,_,_) as t ->
           (if self#is_in_and_types
            then
              self#update_cur_and_types
                (fun lst  -> (name, (Objs.strip_typedecl t)) :: lst);
            t)
       | t -> super#typedecl t
   end : traversal )
let genenrate_type_code _loc tdl (ns : Astf.strings) =
  (let x: Astf.stru = `Type (_loc, tdl) in
   let ns = list_of_app ns [] in
   let filters =
     List.map
       (function
        | `Str (sloc,n) ->
            (match Hashtblf.find_opt filters n with
             | None  -> Locf.failf sloc "%s not found" n
             | Some p -> (n, p))
        | `Ant _ ->
            Locf.raise _loc (Failure "antiquotation not expected here")
        | _ -> assert false) ns in
   let code =
     Ref.protect2 (State.current_filters, filters) (State.keep, false)
       (fun _  ->
          match (traversal ())#mexp (`Struct (_loc, x) : Astf.mexp ) with
          | (`Struct (_loc,s) : Astf.mexp) -> s
          | _ -> assert false) in
   `Sem (_loc, x, code) : Astf.stru )
let () = Ast2pt.generate_type_code := genenrate_type_code
