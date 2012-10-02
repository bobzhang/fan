open Format;
open Camlp4.PreCast;
open Lib_common;
open Fan_basic;
(**
   a module to dynamiclly inject the code you want to
   execute.

   Its main contribution is that it's you have first class
   Ast to manipulate and dynamically load to pipeline to
   the compiler.

   Its defect is that you can not interact with the environment
 *)

module type STR_ITEM = sig value code: Ast.str_item; end ;
module type CLASS_STR_ITEM = sig value code: Ast.class_str_item; end ;
module type EXPR = sig value code: Ast.expr; end ;
module type CTYP = sig  value code: Ast.ctyp;end ;
  
value load_and_query_plugins (file,component) plugins =
  try begin
    Dynlink.loadfile file ;
    let key = Filename.(chop_extension  file) in
    try
      let components = Hashtbl.find plugins key  in
      try   Hashtbl.find components component
      with
        [Not_found -> begin
          prerr_endlinef "component %s not found in file %s"
            component file ;
          exit 2 
        end]
    with
      [Not_found -> begin
        prerr_endlinef "plugin %s not found in Hashtbl" key;
        exit 2
      end]
  end
  with
    [Dynlink.Error s -> begin
      prerr_endline & Dynlink.error_message s;
      exit 2;
    end]
;
value str_items = Hashtbl.create 50 ;
value class_str_items = Hashtbl.create 50 ;
value exprs = Hashtbl.create 50;
value ctyps = Hashtbl.create 50;
  
value str_item_of_file (file,component) =     
  let (module M:STR_ITEM) = load_and_query_plugins (file,component)
      str_items in  M.code;
value class_str_item_of_file (file,component) =
  let (module M:CLASS_STR_ITEM) = load_and_query_plugins (file,component)
      class_str_items  in  M.code;
value expr_of_file (file,component) =
  let (module M:EXPR) = load_and_query_plugins (file,component) exprs in
  M.code;
value ctyp_of_file (file,component) =
  let (module M:CTYP) = load_and_query_plugins (file,component) ctyps in
  M.code;
  
value update (file,component) tbl code_module = begin 
  if Fan_config.debug.val then prerr_endlinef "update (%s,%s)\n" file component
  else ();
  if not (Hashtbl.mem tbl file ) then do{
    let t1 = Hashtbl.create 3;
    Hashtbl.add t1 component code_module;
    Hashtbl.add tbl file t1;
   }
  else do{
    let t1 = Hashtbl.find tbl file;
    if not (Hashtbl.mem t1 component) then 
      Hashtbl.add t1 component code_module
    else do{
      if Fan_config.debug.val then
        prerr_endlinef "Warning: %s already exist in %s before\n"
          component file
      else ();
      Hashtbl.replace t1 component code_module
     }
    }
end 
  ;
