open Ast_gen
open Util
let filter =
  (fun s  ->
    let _loc = loc_of s in
    let v = %mexp{ struct $s end } in
    let mexp = (Typehook.traversal ())#mexp v in
    let code =
      match mexp with
      | %mexp{ struct $s end } -> s
      | _ -> failwith "can not find items back " in
    (if !Typehook.show_code then
      (try Ast2pt.print_stru Format.std_formatter code
      with
      | _ ->
          prerr_endlinef 
          "There is a printer bug\
           Our code generator may still work when Printer is broken\
           Plz send bug report to %s" FConfig.bug_main_address)
  ; code))
