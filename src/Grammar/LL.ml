open LibUtil;
open Structure;
(* open Format; *)
let f = Format.std_formatter;

(* do the backtrakcing with limited LL(k) *)  
let parser_of_terminals
    (terminals:list terminal ) (cont:cont_parse Action.t) (strm:token_stream) =
  let bp = Tools.get_cur_loc strm in (* FIXME more precise Location *)
  let n = List.length terminals in
  let acc = ref [] in begin
    try
      List.iteri
          (fun i terminal  -> 
            let t =
              match XStream.peek_nth strm i with
              [Some (tok,_) -> tok
              |None -> invalid_arg "parser_of_terminals"] in begin
                  acc:= [t::!acc];
                  if not (match terminal with
                    [`Stoken(f,_) -> f t
                    |`Skeyword kwd -> FanToken.match_keyword kwd t])
                  then
                    invalid_arg "parser_of_terminals"
                  else ()
              end) terminals (* tokens *)
    with [Invalid_argument _ -> raise XStream.Failure];

    XStream.njunk n strm;
    match !acc with
    [[] -> invalid_arg "parser_of_terminals"
    |[x::_] ->
        let action = Obj.magic cont bp (Action.mk x) strm in
        List.fold_left (fun a arg -> Action.getf a arg) action !acc]
  end;
    
  (* let rec p ?(first=true) (acc:list FanSig.token) (ts:list terminal) = *)
  (*   match ts with *)
  (*   [ [] -> acc *)
  (*   | [x::xs] -> *)
  (*       match strm with parser *)
  (*       [[< (t,_) when *)
  (*         match x with [`Stoken(f,_) -> f t | `Skeyword kwd -> FanToken.match_keyword kwd t] >] ->    *)
  (*           p  ~first:false [t::acc] xs *)
  (*       |[<>] ->  *)
  (*          if first then raise XStream.Failure *)
  (*          else raise (XStream.Error "")]]in *)

  (* let (ts:list FanSig.token) = p [] terminals in *)
  (*     match ts with *)
  (*     [ [] -> invalid_arg "parser_of_terminals" *)
  (*     | [x::_] -> *)
  (*         let action = Obj.magic cont bp (Action.mk x) strm in *)
  (*         List.fold_left *)
  (*           (fun a arg -> Action.getf a arg) *)
  (*           action ts ]  ; *)
