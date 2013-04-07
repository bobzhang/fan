open AstLoc

open LibUtil

let rec tvar_of_ident =
  function
  | (`Lid (_loc,x) : Ast.ident)|(`Uid (_loc,x) : Ast.ident) -> x
  | (`Dot (_loc,`Uid (_,x),xs) : Ast.ident) ->
      x ^ ("__" ^ (tvar_of_ident xs))
  | _ -> failwith "internal error in the Grammar extension"

let map_to_string ident =
  let rec aux i acc =
    match i with
    | (`Dot (_loc,a,b) : Ast.ident) -> aux a ("_" ^ (aux b acc))
    | (`App (_loc,a,b) : Ast.ident) ->
        "app_" ^ ((aux a ("_to_" ^ (aux b acc))) ^ "_end")
    | (`Lid (_loc,x) : Ast.ident) -> x ^ acc
    | (`Uid (_loc,x) : Ast.ident) -> (String.lowercase x) ^ acc
    | t -> FanLoc.errorf (loc_of t) "map_to_string: %s" (Objs.dump_ident t) in
  aux ident ""

let ident_map f x =
  let lst = list_of_dot x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`Lid (_loc,y) : Ast.ident)::[] -> (`Lid (_loc, (f y)) : Ast.ident )
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`Lid (_loc,y) : Ast.ident)::[] ->
           (`Dot (_loc, q, (`Lid (_loc, (f y)))) : Ast.ident )
       | _ -> FanLoc.errorf (loc_of x) "ident_map: %s" (Objs.dump_ident x))

let ident_map_of_ident f x =
  let lst = list_of_dot x [] in
  match lst with
  | [] -> invalid_arg "ident_map identifier [] "
  | (`Lid (_loc,y) : Ast.ident)::[] -> f y
  | ls ->
      let l = List.length ls in
      (match List.drop (l - 2) ls with
       | q::(`Lid (_loc,y) : Ast.ident)::[] ->
           (`Dot (_loc, q, (f y)) : Ast.ident )
       | _ ->
           FanLoc.errorf (loc_of x) "ident_map_of_ident: %s"
             (Objs.dump_ident x))