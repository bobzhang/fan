open LibUtil;
open Structure;
(* open Format; *)
let f = Format.std_formatter;
  
let parser_of_terminals
    (terminals:list terminal ) (cont:cont_parse Action.t) (strm:token_stream) =
  let bp = Tools.get_cur_loc strm in (* FIXME more precise Location *)
  let rec p ?(first=true) (acc:list FanSig.token) (ts:list terminal) =
    match ts with
    [ [] -> acc
    | [x::xs] ->
        match strm with parser
        [[< (t,_) when
          match x with [`Stoken(f,_) -> f t | `Skeyword kwd -> FanToken.match_keyword kwd t] >] ->   
            p  ~first:false [t::acc] xs
        |[<>] -> 
           if first then raise Stream.Failure
           else raise (Stream.Error "")]]in

  let (ts:list FanSig.token) = p [] terminals in
      match ts with
      [ [] -> invalid_arg "parser_of_terminals"
      | [x::_] ->
          let action = Obj.magic cont bp (Action.mk x) strm in
          List.fold_left
            (fun a arg -> Action.getf a arg)
            action ts ]  ;
