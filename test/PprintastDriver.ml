(* open Format *)
(* open Pprintast *)

let _ =
  let chin = open_in Sys.argv.(1) in begin
    let ast = (Parse.implementation (Lexing.from_channel chin)) in
    let str = AstPrint.string_of_structure ast in
    let new_ast = Parse.implementation (Lexing.from_string str) in
    prerr_endline str;
    (if AstEq.eq_structure (ast, new_ast) then
      prerr_endline "passed."
    else begin
      prerr_endline "failed!!";
    end);
    close_in chin  
  end


















