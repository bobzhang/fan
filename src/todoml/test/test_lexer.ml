open Format;;
open FanUtil;;
(* module MToken = FanToken.Make(FanLoc) *)
(* module MLexer = FanLexer.Make(MToken) (\* avoid conflicts with Lexer in compiler *\) *)

#load_rec "FanLexer.cmo";;
(* test_from_string "2 + 3 (\* (\* (\* \" *\) \" *\) *\) *\) 3 + 4 ";; *)

(* test_from_string "(\* )";; *)
(* Exception: FanLoc.Exc_located (<abstr>, FanMLexer.Make(MToken).Error.E 0). *)
(* test_from_string "( * )";; *)
(* ESCAPED_IDENT "*" *)
(* File "<string>", line 1, characters 0-5 *)
FanConfig.antiquotations := true ;;
module FanLexer=FanLexer.Make(FanToken);;
open FanLexer;;
(*
  test_from_string "$lid";;
ANTIQUOT : "lid"
File "<string>", line 1, characters 0-4
- : unit = ()
test_from_string "$lid:go";;
ANTIQUOT lid: "go"
File "<string>", line 1, characters 0-7
- : unit = ()
test_from_string "$`lid:go";;
ANTIQUOT `lid: "go"
File "<string>", line 1, characters 0-8
- : unit = ()
test_from_string "$(`lid:go)";;
ANTIQUOT `lid: "go"
File "<string>", line 1, characters 1-10
- : unit = ()

test_from_string "$(`lid:(go:\")\"))";;
ANTIQUOT `lid: "(go:\")\")"

 *)


















