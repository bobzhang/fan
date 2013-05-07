open LibUtil

let sfold0 f e _entry _symbl psymb =
  let rec fold accu =  parser
      [ [< a = psymb; 's >] -> fold (f a accu) s
      | [< >] -> accu ] in parser [< a = fold e >] -> a
            
let sfold1 f e _entry _symbl psymb =
  let rec fold accu = parser
      [ [< a = psymb; 's >] -> fold (f a accu) s
      | [< >] -> accu ] in parser
      [< a = psymb; a = fold (f a e) >] -> a

let sfold0sep f e entry symbl psymb psep =
  let failed = fun
    [ [symb; sep] -> Failed.symb_failed_txt entry sep symb
      | _ -> assert false ] in
  let rec kont accu = parser
    [ [< () = psep; a = psymb ?? failed symbl; 's >] -> kont (f a accu) s
    | [< >] -> accu ] in
  parser
    [ [< a = psymb; 's >] -> kont (f a e) s
    | [< >] -> e ] 

let sfold1sep f e entry symbl psymb psep =  (* FIXME this function was never used*)
  let failed = function
    | [symb; sep] -> Failed.symb_failed_txt entry sep symb
    | _ -> assert false  in
  let parse_top =  fun
    [ [symb; _] -> FanParser.parser_of_symbol entry symb 0 (* FIXME context *)
    | _ -> raise XStream.Failure ] in
  let rec kont accu =
    parser
    [ [< () = psep;
        a =
          parser
          [ [< a = psymb >] -> a
          | [< a = parse_top symbl >] -> Obj.magic a
          | [< >] -> raise (XStream.Error (failed symbl)) ];
        's >] ->
          kont (f a accu) s
    | [< >] -> accu ] in
  parser [ [< a = psymb; 's >] -> kont (f a e) s]

