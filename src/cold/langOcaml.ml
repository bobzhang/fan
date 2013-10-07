open Ast_gen
open Util
let filter s =
  let _loc = loc_of s in
  let v: FAst.mexp = `Struct (_loc, s) in
  let mexp = (Typehook.traversal ())#mexp v in
  let code =
    match mexp with
    | (`Struct (_loc,s) : FAst.mexp) -> s
    | _ -> failwith "can not find items back " in
  if Typehook.show_code.contents
  then
    (try Ast2pt.print_stru Format.std_formatter code
     with
     | _ ->
         prerr_endlinef
           "There is a printer bugOur code generator may still work when Printer is brokenPlz send bug report to %s"
           FConfig.bug_main_address);
  code