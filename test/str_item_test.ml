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
  
