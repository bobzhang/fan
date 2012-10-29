let expand_quotation loc expander pos_tag quot =
  let open FanSig in
  let loc_name_opt =
    if quot.q_loc  = "" then begin
      None
    end else begin
      Some quot.q_loc
    end in
  begin try expander loc loc_name_opt  quot.q_contents  with
  | (FanLoc.Exc_located(_,Quotation(_)) as exc) ->   (raise exc)
  | FanLoc.Exc_located(iloc,exc) ->
      let exc1 =
        Quotation ( quot.q_name ,pos_tag,Expanding ,exc) in
      raise ( FanLoc.Exc_located (iloc,exc1) )
  | exc ->
      let exc1 =
        Quotation ( quot.q_name ,pos_tag,Expanding ,exc) in
      raise FanLoc.Exc_located (loc,exc1) 
  end

let mkvirtual =
  (function
  | Ast.ViVirtual  -> Virtual | Ast.ViNil  -> Concrete | _ -> assert false)
let ident_tag ?(conv_lid=(fun (x) -> x))  (i) =
  let rec  self (i) (acc) =
    begin match i with
    | Ast.IdAcc(_,Ast.IdLid(_,"*predef*"),Ast.IdLid(_,"option")) ->
        Some ((( (ldot ( (lident "*predef*") ) "option") ),`lident))
    | Ast.IdAcc(_,i1,i2) -> (self i2 ( (self i1 acc) ))
    | Ast.IdApp(_,i1,i2) ->
        begin match (( (self i1 None ) ),( (self i2 None ) ),acc) with
        | (Some(l,_),Some(r,_),None ) -> Some ((( Lapply ((l,r)) ),`app))
        | _ ->
            (error ( (Camlp4Ast.loc_of_ident i) ) "invalid long identifer") end
    | Ast.IdUid(_,s) ->
        begin match (acc,s) with
        | (None ,"") -> None | (None ,s) -> Some ((( (lident s) ),`uident))
        | (Some(_,(`uident |`app)),"") -> acc
        | (Some(x,(`uident |`app)),s) -> Some ((( (ldot x s) ),`uident))
        | _ -> (error ( (Camlp4Ast.loc_of_ident i) ) "invalid long identifier") end
    | Ast.IdLid(_,s) ->
        let  x =
          begin match acc with
          | None  -> (lident ( (conv_lid s) ))
          | Some(acc,(`uident |`app)) -> (ldot acc ( (conv_lid s) ))
          | _ -> (error ( (loc_of_ident i) ) "invalid long identifier") end in
        Some ((x,`lident))
    | _ -> (error ( (loc_of_ident i) ) "invalid long identifier") end in
  begin match (self i None ) with
  | Some(x) -> x
  | None  -> (error ( (loc_of_ident i) ) "invalid long identifier ") end 
  
let mode = begin try
  let str = (Sys.getenv "CAMLP4_DEBUG") in
  let rec loop (acc) (i) = begin try
    let pos = (String.index_from str i ':') in
    (loop ( (SSet.add ( (String.sub str i ( (pos - i) )) ) acc) ) ( (pos + 1)
      ))
    with
    | Not_found  ->
      (SSet.add ( (String.sub str i ( (( (String.length str) ) - i) )) ) acc)
    end in
  let sections = (loop SSet.empty 0) in
  if (SSet.mem "*" sections) then begin (fun (_) -> true)
  end else begin (fun (x) -> (SSet.mem x sections))
  end with | Not_found  ->   (fun (_) -> false) end
    
let formatter =
  let header = "camlp4-debug: " in
  let at_bol = (ref true ) in
  (make_formatter (
    (fun (buf) ->
      (fun (pos) ->
        (fun (len) ->
          for i = pos to  (( (pos + len) ) - 1) do
            begin
            if at_bol.contents then begin
            (output_string out_channel header)
            end else begin ()
            end;
            let ch = buf.[i] in
            begin
              (output_char out_channel ch);
              (( at_bol.contents ) := ( (ch = '\n') ))
              end
            end done))) ) ( (fun (() ) -> (flush out_channel)) ))
let simple_try =try List.find x lst with Not_found  -> 3 

let u = function
    (Ast.PaVrn(_,_) |Ast.PaStr(_,_) |Ast.PaRng(_,_,_)
     |Ast.PaFlo(_,_) |Ast.PaNativeInt(_,_) |Ast.PaInt64(_,_)
     |Ast.PaInt32(_,_) |Ast.PaInt(_,_) |Ast.PaChr(_,_)
     |Ast.PaTyp(_,_) |Ast.PaArr(_,_) |Ast.PaAnt(_,_)) -> 1
  | 3 -> 2 

let v =
  (fun (ppf) ->
    (function
        | (`Smeta (n,sl,_)) ->   (self#meta ppf n sl)
        | (`Slist0 s) ->   (fprintf ppf "LIST0 %a" ( self#symbol1 ) s)
        | (`Slist0sep (s,t)) ->
            (fprintf ppf "LIST0 %a SEP %a" ( self#symbol1 ) s ( self#symbol1
              ) t)
        | (`Slist1 s) ->   (fprintf ppf "LIST1 %a" ( self#symbol1 ) s)
        | (`Slist1sep (s,t)) ->
            (fprintf ppf "LIST1 %a SEP %a" ( self#symbol1 ) s ( self#symbol1
              ) t)
        | (`Sopt s) ->   (fprintf ppf "OPT %a" ( self#symbol1 ) s)
        | (`Stry s) ->   (fprintf ppf "TRY %a" ( self#symbol1 ) s)
        | (`Snterml (e,l)) ->   (fprintf ppf "%s@ Level@ %S" ( e.ename ) l)
        | ((`Snterm _|`Snext|`Sself|(`Stree _)|(`Stoken _)|(`Skeyword _)) as s) ->
            (self#symbol1 ppf s)))

let short_match x = match x with Some x-> x | Nnoe -> 0 | A -> 1 
let short_match x = match x with Some x-> x | Nnoe -> 0 | A -> 1 | B -> 2 | C -> 3
let short_match x = match x with Some x-> x | Nnoe -> 0 | A -> 1 | B -> 2 | C -> 3 | D -> 4 | E -> 5
let f = function (Some x as y as z) -> z;;
let f = fun (`a (Some _ | None)) -> 3;;
let f = fun `a (Some _ | None) -> 3;;
let f = fun (`a (Some x) | x) -> 3;;
let f = fun (`a `b `c) -> 3;;

let u =
  for i = 0 to 11 do
    print_int a long line;
    print_int i;
    print_string;
  done
let u =
  while true do
    this is ;
    a test;
    ha statement;
  done
let g =
  f a  g b b c d  gh g   g g  g  b
let f () =
  if f a g then
    3
  else 4




let check x msg =
  if ((start_line x) > (stop_line x) ||
      (start_bol x) > (stop_bol x) ||
      (start_off x) > (stop_off x) ||
      (start_line x) < 0 || (stop_line x) < 0 ||
      (start_bol x) < 0 || (stop_bol x) < 0 ||
      (start_off x) < 0 ||  (stop_off x) < 0)
      (* Here, we don't check
        (start_off x) < (start_bol x) || (stop_off x) < (start_bol x)
        since the lexer is called on antiquotations, with off=0, but line and bolpos
        have "correct" lets *)
  then begin
    eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
    false
  end
  else true
let a = 3 + 4 -2

let filter x =
  let f (tok,loc) =
    let tok = keyword_conversion tok (x.is_kwd) in (tok,loc) in
  fun strm -> (x.filter) (Stream.map f strm)

let rec action_arg s sl = function
  | Arg.Unit f -> if s = "" then (f (); Some sl) else None
  | Arg.Bool f ->
      if s = ""
      then
        (match sl with
        | s::sl ->
            (try f (bool_of_string s); Some sl
            with | Invalid_argument "bool_of_string" -> None)
        | [] -> None)
        else
          (try f (bool_of_string s); Some sl
          with | Invalid_argument "bool_of_string" -> None)
  | Arg.Int f ->
      if s = ""
      then
        (match sl with
          | s::sl ->
              try f (int_of_string s); Some sl
              with | Failure "int_of_string" -> None
            | [] -> None)
      else
        (try f (int_of_string s); Some sl
        with | Failure "int_of_string" -> None)
let u = {
  v =3;
  u = 32;
}

let f = {
  u={a=3;
     v=b;
   };
  g={
  a=4;
  b=3;
  };
  g={
  a=4;
  b=3;
  };
  g={
  a=4;
  b=3;
  };
  g={
  a=4;
  b=3;
  }
}    
    
let test_labels = begin
  List.find ~f:ff (+) g ;
  List.find ~f (+) g;
  List.find ~f (+) g;
  f ?g:(Some 32)   3;
  f ?g 3 ;
  f ?g:g 3 ;
end
;;
