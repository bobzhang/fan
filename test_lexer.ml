open Format;;
open FanUtil;;
module MToken = FanToken.Make(FanLoc)
module MLexer = FanLexer.Make(MToken) (* avoid conflicts with Lexer in compiler *)

let test_from_string str =
  let loc = MToken.Loc.mk "<string>" in
  let stream = MLexer.from_string loc str  in
  try
    Stream.iter (fun (t,loc) ->
    match t with
    |FanSig.EOI -> raise (Stream.Error "end")
    | _ ->  fprintf std_formatter "%a@ %a@." MToken.print t MToken.Loc.print loc) stream
  with
    Stream.Error _ -> ();;

let _ = begin
  test_from_string "3 2 32 + (* (* *) *) 32"
end
;;

let test_from_file file =
  let loc = MToken.Loc.mk file in
  let chan = (open_in file) in
  let stream = Stream.of_channel  chan in 
  let stream = MLexer.from_stream loc stream in begin 
  try
    Stream.iter (fun (t,loc) ->
    match t with
    |FanSig.EOI -> begin
        raise (Stream.Error "end");
        close_in chan
    end 
    | _ ->  fprintf std_formatter "%a@ %a@." MToken.print t MToken.Loc.print loc) stream
  with
    Stream.Error _ -> close_in chan;
  end ;;

(* test_from_string "2 + 3 (\* (\* (\* \" *\) \" *\) *\) *\) 3 + 4 ";; *)

(* test_from_string "(\* )";; *)
(* Exception: FanLoc.Exc_located (<abstr>, FanMLexer.Make(MToken).Error.E 0). *)
(* test_from_string "( * )";; *)
(* ESCAPED_IDENT "*" *)
(* File "<string>", line 1, characters 0-5 *)



















