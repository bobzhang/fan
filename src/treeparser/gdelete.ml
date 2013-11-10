

(* Deleting a rule *)

(* [delete_rule_in_tree] returns
     [Some (dsl, t)] if success
        [dsl] =
           Some (symbols) if branch deleted
           None if [action] replaced by previous version of action
        [t] = remaining tree
     [None] if failure *)

let delete_rule_in_tree entry =
  let rec delete_in_tree (symbols:Gdefs.symbol list) (tree:Gdefs.tree)
      : (Gdefs.symbol list option * Gdefs.tree) option=
    match (symbols, tree) with
    | (s :: sl, Node ({node;brother;son} as n)) ->
        if Gtools.logically_eq_symbols entry s node then
            match delete_in_tree sl son with
            |Some (Some dsl,DeadEnd) -> Some (Some (node::dsl),brother)
            |Some (Some dsl, t) -> Some (Some (node::dsl),Node {(n) with son=t})
            |Some (None,t) -> Some (None,Node {(n) with son=t})
            |None -> None
        else
          (match delete_in_tree symbols brother with
          | Some (dsl, t) ->
              Some (dsl, Node {(n) with brother=t} )
          | None -> None )
    | (_ :: _, _) -> None
    | ([], Node n) ->
        (match delete_in_tree [] n.brother with
        | Some (dsl, t) -> Some (dsl, Node {(n) with brother =t  })
        | None -> None )
    | ([], DeadEnd) -> None
    | ([], LocAct _) -> Some (Some [], DeadEnd) in
  delete_in_tree


(* deprecated *)  
let removing _gram _kwd =  ()
  
(* FIXME
   there's a bug
   the revised syntax could parse
   Snterml _ _ => Snterml(_,_)
 *)
let rec decr_keyw_use gram (x:Gdefs.symbol) =
  match x with 
  | Token ({descr = {tag = `Key ; word = A kwd; _}}:Tokenf.pattern) -> removing gram kwd
  | List0 s | List1 s  | Try s | Peek s -> decr_keyw_use gram s
  | List0sep (s1, s2) -> begin  decr_keyw_use gram s1; decr_keyw_use gram s2  end
  | List1sep (s1, s2) -> begin  decr_keyw_use gram s1; decr_keyw_use gram s2  end
  | Self | Nterm _ | Snterml (_, _) | Token _ -> () 
and decr_keyw_use_in_tree gram (x:Gdefs.tree) =
  match x with 
  | DeadEnd | LocAct _ -> ()
  | Node n -> begin
        decr_keyw_use gram n.node;
        decr_keyw_use_in_tree gram n.son;
        decr_keyw_use_in_tree gram n.brother
  end 

let rec delete_rule_in_suffix entry symbols (xs:Gdefs.level list) =
  match xs with
  | lev :: levs ->
      (match delete_rule_in_tree entry symbols lev.lsuffix with
      | Some (dsl, t) ->begin 
        (match dsl with
        | Some dsl -> List.iter (decr_keyw_use entry.gram) dsl
        | None -> () );
        match t with
        | DeadEnd when lev.lprefix == DeadEnd -> levs
        | _ ->
            {(lev) with lsuffix=t} :: levs
      end
      | None ->
          let levs = delete_rule_in_suffix entry symbols levs in
          lev :: levs )
  | [] -> raise Not_found 


let rec delete_rule_in_prefix entry symbols (xs:Gdefs.level list) =
  match xs with 
  | lev :: levs ->
      (match delete_rule_in_tree entry symbols lev.lprefix with
      | Some (dsl, t) -> begin 
          (match dsl with
          | Some dsl -> List.iter (decr_keyw_use entry.gram) dsl
          | None -> ()) ;
          match t with
          | DeadEnd when lev.lsuffix == DeadEnd -> levs
          | _ -> {(lev) with lprefix=t}::levs
      end
      | None -> let levs = delete_rule_in_prefix entry symbols levs in
        lev :: levs)
  | [] -> raise Not_found 
        

let  delete_rule_in_level_list entry (symbols:Gdefs.symbol list) levs =
  match symbols with
  | Self :: symbols -> delete_rule_in_suffix entry symbols levs
  | Nterm e :: symbols when e == entry ->
      delete_rule_in_suffix entry symbols levs
  | _ -> delete_rule_in_prefix entry symbols levs 



let delete_rule entry sl =
  let levs = delete_rule_in_level_list entry sl entry.levels  in begin
    entry.levels <-  levs;
    let start = Gparser.start_parser_of_entry entry in
    let continue = Gparser.continue_parser_of_entry entry in begin 
      entry.start <- start;
      entry.continue <- continue
    end
  end
  


