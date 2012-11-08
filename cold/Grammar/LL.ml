open LibUtil
open Structure
let test (terminals : terminal list) (cont : Action.t cont_parse)
  (strm : token_stream) =
  let bp = Tools.get_cur_loc strm in
  let rec p ?(first= true)  (acc : FanSig.token list) (ts : terminal list) =
    match ts with
    | [] -> acc
    | x::xs ->
        let (__strm :_ Stream.t)= strm in
        (match Stream.peek __strm with
         | Some (t,_) ->
             (Stream.junk __strm;
              (let ok =
                 match x with
                 | `Stoken (f,_) -> f t
                 | `Skeyword kwd -> FanToken.match_keyword kwd t in
               if ok
               then p ~first:false (t :: acc) xs
               else
                 if first
                 then raise Stream.Failure
                 else raise (Stream.Error "")))
         | _ -> raise Stream.Failure) in
  let (ts :FanSig.token list)= p [] terminals in
  match ts with
  | [] -> invalid_arg "test"
  | x::_ ->
      let action = Obj.magic cont bp (Action.mk x) strm in
      List.fold_left (fun a  arg  -> Action.getf a arg) action ts