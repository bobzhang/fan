open LibUtil
open Basic
open FSig
open Format
module Ast = Camlp4Ast
let keep = ref false
type plugin = 
  {
  plugin_transform: module_types -> Ast.str_item;
  mutable plugin_activate: bool} 
type plugin_name = string 
let filters: (plugin_name,plugin) Hashtbl.t = Hashtbl.create 30
let show_code = ref false
let register (name,filter) =
  if Hashtbl.mem filters name
  then eprintf "Warning:%s filter already exists!@." name
  else
    (eprintf "%s filter registered@." name;
     Hashtbl.add filters name
       { plugin_transform = filter; plugin_activate = false })
let show_modules () =
  Hashtbl.iter (fun key  _  -> Format.printf "%s@ " key) filters;
  print_newline ()
let plugin_add plugin =
  try let v = Hashtbl.find filters plugin in v.plugin_activate <- true
  with
  | Not_found  -> (show_modules (); failwithf "plugins %s not found " plugin)
let plugin_remove plugin =
  try let v = Hashtbl.find filters plugin in v.plugin_activate <- false
  with
  | Not_found  ->
      (show_modules ();
       eprintf "plugin %s not found, removing operation ignored" plugin)
let filter_type_defs ?qualified  () =
  object 
    inherit  Ast.map as super
    val mutable type_defs = Ast.StNil _loc
    method! sig_item =
      function
      | Ast.SgVal (_loc,_,_)|Ast.SgInc (_loc,_)|Ast.SgExt
          (_loc,_,_,_)|Ast.SgExc (_loc,_)|Ast.SgCls (_loc,_)|Ast.SgClt
          (_loc,_)|Ast.SgDir (_loc,_,Ast.ExNil _)|Ast.SgMod
          (_loc,_,_)|Ast.SgMty (_loc,_,_)|Ast.SgRecMod (_loc,_)|Ast.SgOpn
          (_loc,_) -> Ast.SgNil _loc
      | Ast.SgTyp (_,(Ast.TyDcl (_loc,name,vars,ctyp,constraints) as x)) ->
          let x =
            match ((Ctyp.qualified_app_list ctyp), qualified) with
            | (Some (Ast.IdAcc (_loc,i,_),ls),Some q) when
                (Ident.eq i q) && (Ctyp.eq_list ls vars) ->
                Ast.TyDcl (_loc, name, vars, (Ast.TyNil _loc), constraints)
            | (_,_) -> super#ctyp x in
          let y = Ast.StTyp (_loc, x) in
          let () = type_defs <- Ast.StSem (_loc, type_defs, y) in
          Ast.SgTyp (_loc, x)
      | Ast.SgTyp (_loc,ty) ->
          let x = super#ctyp ty in
          let () =
            type_defs <- Ast.StSem (_loc, type_defs, (Ast.StTyp (_loc, x))) in
          Ast.SgTyp (_loc, x)
      | x -> super#sig_item x
    method! ident =
      function
      | Ast.IdAcc (_loc,x,y) as i ->
          (match qualified with
           | Some q when Ident.eq q x -> super#ident y
           | _ -> super#ident i)
      | i -> super#ident i
    method! ctyp =
      function
      | Ast.TyMan (_loc,_,ctyp) -> super#ctyp ctyp
      | ty -> super#ctyp ty
    method get_type_defs = type_defs
  end
let traversal () =
  object (self : 'self_type)
    inherit  Ast.map as super
    val module_types_stack = (Stack.create () : module_types Stack.t )
    method get_cur_module_types : module_types= Stack.top module_types_stack
    method update_cur_module_types f =
      let open Stack in push (f (pop module_types_stack)) module_types_stack
    method in_module = Stack.push [] module_types_stack
    method out_module = (Stack.pop module_types_stack) |> ignore
    val mutable cur_and_types = ([] : and_types )
    val mutable and_group = false
    method in_and_types = and_group <- true; cur_and_types <- []
    method out_and_types = and_group <- false; cur_and_types <- []
    method is_in_and_types = and_group
    method get_cur_and_types = cur_and_types
    method update_cur_and_types f = cur_and_types <- f cur_and_types
    method! module_expr =
      function
      | Ast.MeStr (_loc,u) ->
          let () = self#in_module in
          let res = self#str_item u in
          let module_types = List.rev self#get_cur_module_types in
          let result =
            Hashtbl.fold
              (fun _  v  acc  ->
                 if v.plugin_activate
                 then
                   Ast.StSem (_loc, acc, (v.plugin_transform module_types))
                 else acc) filters
              (if keep.contents then res else Ast.StNil _loc) in
          let () = self#out_module in Ast.MeStr (_loc, result)
      | x -> super#module_expr x
    method! str_item =
      function
      | Ast.StTyp (_loc,Ast.TyAnd (_,_,_)) as x ->
          (self#in_and_types;
           (let _ = super#str_item x in
            self#update_cur_module_types
              (fun lst  -> (Mutual (List.rev self#get_cur_and_types)) :: lst);
            self#out_and_types;
            if keep.contents then x else Ast.StNil _loc))
      | Ast.StTyp (_loc,(Ast.TyDcl (_,name,_,_,_) as t)) as x ->
          (self#update_cur_module_types
             (fun lst  -> (Single (name, t)) :: lst);
           x)
      | Ast.StVal (_loc,Ast.ReNil ,_)|Ast.StMty (_loc,_,_)|Ast.StInc
          (_loc,_)|Ast.StExt (_loc,_,_,_)|Ast.StExp (_loc,_)|Ast.StExc
          (_loc,_,Ast.ONone )|Ast.StDir (_loc,_,_) as x -> x
      | x -> super#str_item x
    method! ctyp =
      function
      | Ast.TyDcl (_,name,_,_,_) as t ->
          (if self#is_in_and_types
           then self#update_cur_and_types (fun lst  -> (name, t) :: lst)
           else ();
           t)
      | t -> super#ctyp t
  end
let g = Gram.create_gram ()
let fan_quot = Gram.mk_dynamic g "fan_quot"
let fan_quots = Gram.mk_dynamic g "fan_quots"
let _ =
  Gram.extend (fan_quot : 'fan_quot Gram.t )
    (None,
      [(None, None,
         [([`Skeyword "plugin_add";
           `Stoken
             (((function | `STR (_,_) -> true | _ -> false)),
               (`Normal, "`STR (_,_)"))],
            (Gram.mk_action
               (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                  match __fan_1 with
                  | `STR (_,plugin) ->
                      ((plugin_add plugin; Ast.ExNil _loc) : 'fan_quot )
                  | _ -> assert false)));
         ([`Skeyword "plugins_add";
          `Slist1sep
            ((Gram.srules fan_quot
                [([`Stoken
                     (((function | `STR (_,_) -> true | _ -> false)),
                       (`Normal, "`STR (_,_)"))],
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `STR (_,x) -> (x : 'e__1 )
                         | _ -> assert false)))]), (`Skeyword ","))],
           (Gram.mk_action
              (fun (plugins : 'e__1 list)  _  (_loc : FanLoc.t)  ->
                 (List.iter plugin_add plugins; Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "plugins_clear"],
           (Gram.mk_action
              (fun _  (_loc : FanLoc.t)  ->
                 (Hashtbl.iter (fun _  v  -> v.plugin_activate <- false)
                    filters;
                  Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "plugin_remove";
          `Stoken
            (((function | `STR (_,_) -> true | _ -> false)),
              (`Normal, "`STR (_,_)"))],
           (Gram.mk_action
              (fun (__fan_1 : [> FanToken.t])  _  (_loc : FanLoc.t)  ->
                 match __fan_1 with
                 | `STR (_,plugin) ->
                     ((plugin_remove plugin; Ast.ExNil _loc) : 'fan_quot )
                 | _ -> assert false)));
         ([`Skeyword "plugins_remove";
          `Slist1sep
            ((Gram.srules fan_quot
                [([`Stoken
                     (((function | `STR (_,_) -> true | _ -> false)),
                       (`Normal, "`STR (_,_)"))],
                   (Gram.mk_action
                      (fun (__fan_0 : [> FanToken.t])  (_loc : FanLoc.t)  ->
                         match __fan_0 with
                         | `STR (_,x) -> (x : 'e__2 )
                         | _ -> assert false)))]), (`Skeyword ","))],
           (Gram.mk_action
              (fun (plugins : 'e__2 list)  _  (_loc : FanLoc.t)  ->
                 (List.iter plugin_remove plugins; Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "keep"; `Skeyword "on"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (keep := true; Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "keep"; `Skeyword "off"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (keep := false; Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "show_code"; `Skeyword "on"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (show_code := true; Ast.ExNil _loc : 'fan_quot ))));
         ([`Skeyword "show_code"; `Skeyword "off"],
           (Gram.mk_action
              (fun _  _  (_loc : FanLoc.t)  ->
                 (show_code := false; Ast.ExNil _loc : 'fan_quot ))))])]);
  Gram.extend (fan_quots : 'fan_quots Gram.t )
    (None,
      [(None, None,
         [([`Slist0
              (Gram.srules fan_quots
                 [([`Snterm (Gram.obj (fan_quot : 'fan_quot Gram.t ));
                   `Skeyword ";"],
                    (Gram.mk_action
                       (fun _  (x : 'fan_quot)  (_loc : FanLoc.t)  ->
                          (x : 'e__3 ))))])],
            (Gram.mk_action
               (fun (xs : 'e__3 list)  (_loc : FanLoc.t)  ->
                  (Ast.ExSeq (_loc, (Ast.exSem_of_list xs)) : 'fan_quots ))))])])