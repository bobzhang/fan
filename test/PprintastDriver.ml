open Format

(* open Pprintast *)

let _ =
  let chin = open_in Sys.argv.(1) in begin
    (AstPrint.structure std_formatter (Parse.implementation (Lexing.from_channel chin)));
    close_in chin  
  end


















