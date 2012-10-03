open Format


type evaluation_outcome = Result of Obj.t | Exception of exn

let toplevel_value_bindings =
  (Hashtbl.create 37 : (string, Obj.t) Hashtbl.t)
let getvalue name =
  try
    Hashtbl.find toplevel_value_bindings name
  with Not_found ->
    failwith (name ^ " unbound at toplevel")
let setvalue name v =
  Hashtbl.replace toplevel_value_bindings name v
let max_printer_depth = ref 100
let max_printer_steps = ref 300
let print_out_value = Oprint.out_value
let print_out_type = Oprint.out_type
let print_out_class_type = Oprint.out_class_type
let print_out_module_type = Oprint.out_module_type
let print_out_sig_item = Oprint.out_sig_item
let print_out_signature = Oprint.out_signature
let print_out_phrase = Oprint.out_phrase

(* Return the value referred to by a path *)
let rec eval_path = let open Path in function
  | Pident id ->
      if Ident.persistent id || Ident.global id then
        Symtable.get_global_value id
      else begin
        let name = Translmod.toplevel_name id in
        try
          Hashtbl.find toplevel_value_bindings name
        with Not_found ->
          raise (Symtable.Error(Symtable.Undefined_global name))
      end
  | Pdot(p, s, pos) ->
      Obj.field (eval_path p) pos
  | Papply(p1, p2) ->
      failwith "Toploop.eval_path"
(* To print values *)
module EvalPath = struct
  type value = Obj.t
  exception Error
  let eval_path p = try eval_path p with Symtable.Error _ -> raise Error
  let same_value v1 v2 = (v1 == v2)
end
    
module Printer =  Genprintval.Make(Obj)(EvalPath)
let print_untyped_exception ppf obj =
  !print_out_value ppf (Printer.outval_of_untyped_exception obj)
let outval_of_value env obj ty =
  Printer.outval_of_value !max_printer_steps !max_printer_depth
    (fun _ _ _ -> None) env obj ty
let print_value env obj ppf ty =
  !print_out_value ppf (outval_of_value env obj ty)
let rec pr_item env = let open Types in function
  | Sig_value(id, decl) :: rem ->
      let tree = Printtyp.tree_of_value_description id decl in
      let valopt =
        match decl.val_kind with
        | Val_prim _ -> None
        | _ -> let v = outval_of_value env (getvalue (Translmod.toplevel_name id))
              decl.val_type in
            Some v in
      Some (tree, valopt, rem)
  | Sig_type(id, _, _) :: rem when Btype.is_row_name (Ident.name id) ->
      pr_item env rem
  | Sig_type(id, decl, rs) :: rem ->
      let tree = Printtyp.tree_of_type_declaration id decl rs in
      Some (tree, None, rem)
  | Sig_exception(id, decl) :: rem ->
      let tree = Printtyp.tree_of_exception_declaration id decl in
      Some (tree, None, rem)
  | Sig_module(id, mty, rs) :: rem ->
      let tree = Printtyp.tree_of_module id mty rs in
      Some (tree, None, rem)
  | Sig_modtype(id, decl) :: rem ->
      let tree = Printtyp.tree_of_modtype_declaration id decl in
      Some (tree, None, rem)
  | Sig_class(id, decl, rs) :: cltydecl :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_class_declaration id decl rs in
      Some (tree, None, rem)
  | Sig_class_type(id, decl, rs) :: tydecl1 :: tydecl2 :: rem ->
      let tree = Printtyp.tree_of_cltype_declaration id decl rs in
      Some (tree, None, rem)
  | _ -> None

let rec item_list env = function
  | [] -> []
  | items ->
     match pr_item env items with
     | None -> []
     | Some (tree, valopt, items) -> (tree, valopt) :: item_list env items
    
    
(* The current typing environment for the toplevel *)
let toplevel_env = ref Env.empty
let may_trace = ref false (*Global lock on tracing*)    
let load_lambda ppf lam =
  let slam = Simplif.simplify_lambda lam in
  let (init_code, fun_code) = Bytegen.compile_phrase slam in
  let (code, code_size, reloc) = Emitcode.to_memory init_code fun_code in
  let can_free = (fun_code = []) in
  let initial_symtable = Symtable.current_state() in
  Symtable.patch_object code reloc;
  Symtable.check_global_initialized reloc;
  Symtable.update_global_table();
  try
    may_trace := true;
    let retval = (Meta.reify_bytecode code code_size) () in
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Result retval
  with x ->
    may_trace := false;
    if can_free then begin
      Meta.static_release_bytecode code code_size;
      Meta.static_free code;
    end;
    Symtable.restore_state initial_symtable;
    Exception x
      
let execute_phrase print_outcome ppf phr =
  let open Parsetree in
  let open Typedtree in
  let open Outcometree in 
  match phr with
  | Ptop_def sstr ->
      let oldenv = !toplevel_env in
      Typecore.reset_delayed_checks ();
      let (str, sg, newenv) = Typemod.type_toplevel_phrase oldenv sstr in
      let sg' = Typemod.simplify_signature sg in
      ignore (Includemod.signatures oldenv sg sg');
      Typecore.force_delayed_checks ();
      let lam = Translmod.transl_toplevel_definition str in
      Warnings.check_fatal ();
      begin try
        toplevel_env := newenv;
        let res = load_lambda ppf lam in
        let out_phr =
          match res with
          | Result v ->
              if print_outcome then
                match str.str_items with
                | [ { str_desc = Tstr_eval exp }] ->
                    (* printer for type *)
                    let outv = outval_of_value newenv v exp.exp_type in
                    (* printer for env *)
                    let ty = Printtyp.tree_of_type_scheme exp.exp_type in
                    Ophr_eval (outv, ty)
                | [] -> Ophr_signature []
                | _ -> Ophr_signature (item_list newenv sg')
              else Ophr_signature []
          | Exception exn ->
              toplevel_env := oldenv;
              if exn = Out_of_memory then Gc.full_major();
              let outv =
                outval_of_value !toplevel_env (Obj.repr exn) Predef.type_exn
              in
              Ophr_exception (exn, outv)
        in
        !print_out_phrase ppf out_phr;
        begin match out_phr with
        | Ophr_eval (_, _) | Ophr_signature _ -> true
        | Ophr_exception _ -> false
        end
      with x ->
        toplevel_env := oldenv; raise x
      end
  | Ptop_dir(dir_name, dir_arg) ->
      fprintf ppf "Directive `%s' not supported .@." dir_name;
      false
        
        



















