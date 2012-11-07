open Grammar;
open Structure;
open LibUtil;


let pp = Format.fprintf;
let f =Format.std_formatter;
Parser.parser_of_terminals
  [`Skeyword "b"; `Skeyword "c"; `Skeyword "a"]
  (fun _loc act _strm ->
    begin Action.mk
        (fun  x y z -> begin 
          pp f "%s@." (BatPervasives.dump act);
          match (x,y,z) with
          [(`KEYWORD x,`KEYWORD y,`KEYWORD z) ->
            pp f "@[<2>%s@;%s@;%s@]@." x y z]
        end) end)
  (Stream.map (fun x -> (x,Gram.ghost_token_info)) [<  `KEYWORD "b"; `KEYWORD "c"; `KEYWORD "a" >]);
