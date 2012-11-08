open Grammar;
open Structure;
open LibUtil;

(* module P = PreCast.Make (struct end); (\* FIXME here Gram contains a global varialbe*\) *)
(* let open FanParsers in  begin *)
(*    pa_r (module P); *)
(*    pa_rp (module P); *)
(*    pa_q (module P); *)
(*    pa_g (module P); *)
(*    pa_l (module P); *)
(*    pa_m (module P); *)
(* end; *)



let pp = Format.fprintf;
let f =Format.std_formatter;
(* Parser.parser_of_terminals *)
(* LL.test *)
let u app = app
    
  (* [`Skeyword "let"; `Skeyword "i"; `Skeyword "begin"; `Skeyword "match"] *)
    [`Skeyword "b"; `Skeyword "c"; `Skeyword "a";`Skeyword "d"]
  (fun loc act _strm ->

    begin
      pp f "%a@." FanLoc.print loc;
      Action.mk
        (fun  x y z w-> begin 
          pp f "%s@." (BatPervasives.dump act);
          match (x,y,z,w) with
          [(`KEYWORD x,`KEYWORD y,`KEYWORD z,`KEYWORD w) ->
            pp f "@[<2>%s@;%s@;%s@;%s@]@." x y z w]
        end) end)

    (* (Gram.token_stream_of_string "let in begin match" |> FanLexUtil.strict_clean) *)
    
    (Stream.map (fun x -> (x,Gram.ghost_token_info))
       [<  `KEYWORD "ab"; `KEYWORD "d"; `KEYWORD "a" ; `KEYWORD "d" >]);
(* pp f "#hel"; *)
u LL.test;
(* u Parser.parser_of_terminals; *)
