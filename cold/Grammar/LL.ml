open LibUtil
open Structure
let f = Format.std_formatter
let parser_of_terminals (terminals : terminal list)
  (cont : Action.t cont_parse) (strm : token_stream) =
  let bp = Tools.get_cur_loc strm in
  let n = List.length terminals in
  let acc = ref [] in
  (try
     List.iteri
       (fun i  terminal  ->
          let t =
            match Stream.peek_nth strm i with
            | Some (tok,_) -> tok
            | None  -> invalid_arg "parser_of_terminals" in
          acc := (t :: (acc.contents));
          if
            not
              (match terminal with
               | `Stoken (f,_) -> f t
               | `Skeyword kwd -> FanToken.match_keyword kwd t)
          then invalid_arg "parser_of_terminals"
          else ()) terminals
   with | Invalid_argument _ -> raise Stream.Failure);
  Stream.njunk n strm;
  (match acc.contents with
   | [] -> invalid_arg "parser_of_terminals"
   | x::_ ->
       let action = Obj.magic cont bp (Action.mk x) strm in
       List.fold_left (fun a  arg  -> Action.getf a arg) action acc.contents)