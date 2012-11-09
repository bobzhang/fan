open LibUtil
open Structure
let f = Format.std_formatter
let parser_of_terminals (terminals : terminal list)
  (cont : Action.t cont_parse) (strm : token_stream) =
  let bp = Tools.get_cur_loc strm in
  let rec p ?(first= true)  (acc : FanSig.token list) (ts : terminal list) =
    match ts with
    | [] -> acc
    | x::xs ->
        let (__strm :_ Stream.t)= strm in
        (match Stream.peek __strm with
         | Some (t,_) when
             match x with
             | `Stoken (f,_) -> f t
             | `Skeyword kwd -> FanToken.match_keyword kwd t ->
             (Stream.junk __strm; p ~first:false (t :: acc) xs)
         | _ ->
             if first then raise Stream.Failure else raise (Stream.Error "")) in
  let (ts :FanSig.token list)= p [] terminals in
  match ts with
  | [] -> invalid_arg "parser_of_terminals"
  | x::_ ->
      let action = Obj.magic cont bp (Action.mk x) strm in
      List.fold_left (fun a  arg  -> Action.getf a arg) action ts