open Format
  
open Lib_common
  
module C2O = Camlp4.Struct.Camlp4Ast2OCamlAst.Make(Camlp4.PreCast.Ast)
  
let s2s s : Parsetree.toplevel_phrase = Obj.magic (C2O.phrase s)
let preload_objects = ref []
let prepare ppf =
  (Toploop.set_paths ();
   try
     let res =
       List.for_all (Topdirs.load_file ppf) (List.rev !preload_objects) in
     let () = !Toploop.toplevel_startup_hook () in res
   with
   | x ->
       (try let () = Errors.report_error ppf x in false
        with
        | x ->
            (Format.fprintf ppf "Uncaught exception: %s\n"
               (Printexc.to_string x);
             false)))
  
(*
  FIXME do the type checking at compile time,
  only load lambda at the runtime?
  If that environment does not work,
  so we can only do parsing to the ast at
  compile time??
 *)
let eval_ast fmt ast =
  let _snap = Btype.snapshot ()
  in
    try
      (Env.reset_missing_cmis ();
       ignore (Toploop.execute_phrase true fmt (s2s ast)))
    with | x -> (Errors.report_error fmt x; exit 2)
  
open Camlp4.PreCast
  
(* module Lexer conflict withs Lexer *)
let _ =
  (Toploop.initialize_toplevel_env ();
   let camlp4 = Filename.concat Config.standard_library "camlp4" in
   let toplevel = Filename.concat Config.standard_library "compiler-libs"
   in Ref.modify Config.load_path (fun x -> camlp4 :: toplevel :: x))
  


