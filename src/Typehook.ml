open LibUtil
open Format

open FSigUtil  


(** A Hook To Ast Filters *)
(* type plugin_name = string  *)

let filters : (plugin_name, plugin) Hashtbl.t  = Hashtbl.create 30;;

let show_code =  ref false
let print_collect_mtyps = ref false
  
let register  ?filter ?position (name,transform) =
  if Hashtbl.mem filters name then
    eprintf "Warning:%s filter already exists!@." name
  else 
   Hashtbl.add filters name {transform; position;filter} 


let show_modules () =
  begin
    Hashtbl.iter
      (fun key _  ->
        Format.printf  "%s@ " key ) filters;
    print_newline()
  end
  
(* Get all definitions from mli file
  Entrance: sigi *)


let plugin_add plugin =
  let try v = Hashtbl.find filters plugin in 
    if not
        (List.exists (fun (n,_) -> n=plugin) !FanState.current_filters)
    then
      Ref.modify FanState.current_filters (fun x -> cons (plugin,v) x) 
    else
      eprintf "<Warning> plugin %s has already been loaded" plugin

  with
  |Not_found -> begin
    show_modules ();
    failwithf "plugins %s not found " plugin ;
  end

    
let plugin_remove plugin =
    Ref.modify FanState.current_filters (fun x -> List.remove plugin x) 
  




class type traversal = object
  inherit Objs.map
  method get_cur_mtyps: mtyps
  method get_cur_and_types: and_types
  (* method in_and_types: *)
  method update_cur_and_types:
      (and_types -> and_types) -> unit
  method update_cur_mtyps:
      (mtyps -> mtyps) -> unit

end

let iterate_code sloc mtyps = 
  (fun (_, {position;transform;filter}) acc ->
    let mtyps =
      match filter with
      |Some x -> FSigUtil.apply_filter x mtyps
      |None -> mtyps in
    let code = transform mtyps in 
    match (position,code) with
    |(Some x,Some code) ->
        let (name,f) = Filters.make_filter (x,code) in 
        (AstFilters.register_stru_filter (name,f);
         AstFilters.use_implem_filter name ;
         acc)
    |(None,Some code) ->
        let code = FanAstN.fill_loc_stru sloc code in
        ({:stru@sloc| $acc;; $code |};)
    |(_,None) -> acc);;
 
let traversal () : traversal  = object (self:'self_type)
  inherit Objs.map as super
  val mtyps_stack : mtyps Stack.t  = Stack.create ()
  val mutable cur_and_types : and_types= []
  val mutable and_group = false
  method get_cur_mtyps : mtyps =
    Stack.top mtyps_stack
  method update_cur_mtyps f =
    Stack.(push (f (pop mtyps_stack)) mtyps_stack)
  method private in_module =  Stack.push [] mtyps_stack 
  method private out_module = ignore (Stack.pop mtyps_stack)
      
  method private in_and_types =  (and_group <- true; cur_and_types <- [])
  method private out_and_types = (and_group <- false; cur_and_types <- [])
  method private is_in_and_types = and_group
  method get_cur_and_types = cur_and_types
  method update_cur_and_types f = 
    cur_and_types <-  f cur_and_types
        (* entrance *)  
  method! mexp = with stru function
    | {:mexp@sloc| struct $u end |}  ->
        (self#in_module ;
         (* extracted code *)
         let res = self#stru u in
         let mtyps = List.rev (self#get_cur_mtyps) in
         let () = if !print_collect_mtyps then
           eprintf "@[%a@]@." pp_print_mtyps mtyps in
         let result =
         List.fold_right (iterate_code sloc mtyps)
             !FanState.current_filters 
             (if !FanState.keep then res else {@sloc| let _ = () |}) in
            (self#out_module ; {:mexp@sloc| struct $result end |} ))

    | x -> super#mexp x 
  method! stru  = with stru function
    | {| type $_ and $_ |} as x -> begin
      self#in_and_types;
      let _ = super#stru x in
      (self#update_cur_mtyps
          (fun lst -> `Mutual (List.rev self#get_cur_and_types) :: lst );
       self#out_and_types;
       (if !FanState.keep then x else {| let _ = () |} (* FIXME *) ))
    end
    | `TypeWith(_loc,typedecl,_) ->
        self#stru (`Type(_loc,typedecl))
    | {| type $((`TyDcl (_,`Lid(_, name), _, _, _) as t)) |} as x -> 
        let item =  `Single (name,Objs.strip_loc_typedecl t) in
        let () =
          if !print_collect_mtyps then eprintf "Came across @[%a@]@."
              pp_print_types  item in
        (self#update_cur_mtyps (fun lst -> item :: lst);
       (* if !keep then x else {| |} *) (* always keep *)
       x )

    | ( {| let $_ |}  | {| module type $_ = $_ |}  | {| include $_ |}
    | {| external $_ : $_ = $_ |} | {| $exp:_ |}
    | {| exception $_ |} 
    | {| # $_ $_ |}  as x)  ->  x (* always keep *)
    |  x ->  super#stru x  
  method! typedecl = function
    | `TyDcl (_, `Lid(_,name), _, _, _) as t -> 
      ((if self#is_in_and_types then
        self#update_cur_and_types (fun lst ->
          (name,Objs.strip_loc_typedecl t) :: lst ));
        t)
    | t -> super#typedecl t 
end











