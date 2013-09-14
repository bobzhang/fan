open Gstructure
  
open LibUtil
  

  
(* open Format; *)
(* [bp] means begining position, [ep] means ending position
   apply the [parse_fun] and get the result and the location of
   consumed areas
 *)
let with_loc (parse_fun: 'b FToken.parse ) strm =
  let bp = Gtools.get_cur_loc strm in
  let x = parse_fun strm in
  let ep = Gtools.get_prev_loc strm in
  let loc =
    let start_off_bp = FLoc.start_off bp in
    let stop_off_ep = FLoc.stop_off ep in 
    if start_off_bp > stop_off_ep then 
      Location_util.join bp
    else FLoc.merge bp ep in
  (x, loc)


(* given a level string, return a number from 0
   {[
   Gparser.level_number (Obj.magic expr) "top";
   - : int = 0
   Gparser.level_number (Obj.magic expr) "simple";
   - : int = 16
   ]}
 *)  
let level_number entry lab =
  let rec lookup levn = function
    | [] -> failwithf "unknown level %s"  lab
    | lev :: levs ->
        if Gtools.is_level_labelled lab lev then levn else lookup (1 + levn) levs  in
  match entry.edesc with
  | Dlevels elev -> lookup 0 elev
  | Dparser _ -> raise Not_found 
        

(* in case of syntax error, the system attempts to recover the error by applying
   the [continue] function of the previous symbol(if the symbol is a call to an entry),
   so there's no behavior difference between [LA] and [NA]
 *)
        
module ArgContainer= Stack
  
(*
  It outputs a stateful parser, but it is functional itself
 *)    
let rec parser_of_tree entry (lev,assoc) (q: (Gaction.t * FLoc.t) ArgContainer.t ) x =
  let alevn =
    match assoc with
    | `LA|`NA -> lev + 1 | `RA -> lev  in
  (*
    Given a tree, return a parser which has the type
    [parse Gaction.t]. Think about [node son], only son owns the action,
    so the action returned by son is a function,
    it is to be applied by the value returned by node.
   *)
  let rec from_tree tree =
    match tree  with
    |DeadEnd -> raise XStream.NotConsumed (* FIXME be more preicse *)
    | LocAct (act, _) -> fun _ -> act
          (* | LocActAppend(act,_,n) -> *)
          (* rules ending with [SELF] , for this last symbol there's a call to the [start] function:
             of the current level if the level is [`RA] or of the next level otherwise. (This can be
             verified by [start_parser_of_levels]) *)      
    | Node {node = `Sself; son = LocAct (act, _); brother = bro} ->  fun strm ->
        (try
          let a = with_loc (entry.estart alevn) strm in
          fun ()  -> ArgContainer.push a q; act
        with  XStream.NotConsumed  -> (fun ()  -> from_tree bro strm)) ()

          (* [son] will never be [DeadEnd] *)        
    | Node ({ node ; son; brother } as y) ->
        (*
          Handle the problem
          {[
          `-OPT assoc---rule_list---.
          `-OPT [ `STR (_,_)]---OPT assoc---rule_list---.
          ]}
         *)
        match Gtools.get_terminals  y with
        | None ->
            (* [paser_of_symbol] given a stream should always return a value  *) 
            (let ps = parser_of_symbol entry node  in fun strm ->
              let bp = Gtools.get_cur_loc strm in
              (try
                let a = ps strm in
                fun ()  ->
                  ArgContainer.push a q;
                  (let pson = from_tree son in
                  try pson strm
                  with
                  | e ->
                      (ignore (ArgContainer.pop q);
                       (match e with
                       | XStream.NotConsumed  ->
                           if (Gtools.get_cur_loc strm) = bp
                           then raise XStream.NotConsumed
                           else
                             raise @@
                               XStream.Error
                               (Gfailed.tree_failed  entry a node son)
                       | _ -> raise e)))
              with | XStream.NotConsumed  -> (fun ()  -> from_tree brother strm)) ())

        | Some (tokl, node, son) -> fun strm ->
            (try
              let args = List.rev (parser_of_terminals tokl strm) in
              fun ()  ->
                List.iter (fun a  -> ArgContainer.push a q) args;
                (let len = List.length args in
                let p = from_tree son in
                try p strm
                with
                | e ->
                    (for _i = 1 to len do ignore (ArgContainer.pop q) done;
                     (match e with
                     | XStream.NotConsumed  ->
                         raise
                           (XStream.Error
                              (Gfailed.tree_failed  entry e (node :>symbol) son))
                     | _ -> raise e)))
            with | XStream.NotConsumed  -> (fun ()  -> from_tree brother strm)) () in
  let parse = from_tree x in
  fun strm -> 
    let ((arity,_symbols,_,parse),loc) =  with_loc parse strm in 
    let ans = ref parse in
    (for _i = 1 to arity do
      let (v,_) = ArgContainer.pop q in
      ans:=Gaction.getf !ans v;   
    done;
     (!ans,loc))


(*
  {[
  let a : FToken.t = Obj.magic & Gparser.parser_of_terminals
  [`Skeyword "a";`Skeyword "b"; `Skeyword "c"]
  (fun _ v _  -> Gaction.mk (fun  c b a ->  v))
  [< (`KEYWORD "a",_loc) ; (`KEYWORD "b", _loc); (`KEYWORD "c",_loc) >];
  val a : FToken.t = `KEYWORD "c"
  ]}
 *)    
and parser_of_terminals (terminals: terminal list) strm =
  let n = List.length terminals in
  let acc = ref [] in begin
    (try
      List.iteri
        (fun i terminal  -> 
          let (t,loc) =
            match XStream.peek_nth strm i with
            | Some (t,loc) (* (tok,_) *) -> (t,loc)
            | None -> invalid_arg "parser_of_terminals" in
          begin
            acc:= (Gaction.mk t,loc)::!acc;
            if not
                (match terminal with
                |`Stoken(f,_,_) -> f t
                |`Skeyword kwd ->
                    begin match t with
                    |`KEYWORD kwd' when kwd = kwd' -> true
                    | _ -> false
                    end)
            then
              invalid_arg "parser_of_terminals"
          end) terminals
    with Invalid_argument _ -> raise XStream.NotConsumed);
    XStream.njunk n strm;
    !acc
  end          
(* only for [Smeta] it might not be functional *)
and parser_of_symbol (entry:Gstructure.entry) s  =
  let rec aux s = 
    match s with 
    | `Slist0 s ->
        let ps = aux s in  Gcomb.slist0 ps ~f:(fun l -> Gaction.mk (List.rev l))
    | `Slist0sep (symb, sep) ->
        let ps = aux symb and pt =  aux sep  in
        Gcomb.slist0sep ps pt ~err:(fun v -> Gfailed.symb_failed entry v sep symb)
          ~f:(fun l -> Gaction.mk (List.rev l))
    | `Slist1 s -> let ps =  aux s  in
      Gcomb.slist1 ps ~f:(fun l -> Gaction.mk (List.rev l))
    | `Slist1sep (symb, sep) ->
        let ps = aux symb and pt = aux sep  in
        Gcomb.slist1sep ps pt ~err:(fun v -> Gfailed.symb_failed entry v sep symb)
          ~f:(fun l -> Gaction.mk (List.rev l))
    | `Sopt s -> let ps = aux s  in Gcomb.opt ps ~f:Gaction.mk
    | `Stry s -> let ps = aux s in Gcomb.tryp ps
    | `Speek s -> let ps = aux s in Gcomb.peek ps
    | `Snterml (e, l) -> fun strm -> e.estart (level_number e l) strm
    | `Snterm e -> fun strm -> e.estart 0 strm  (* No filter any more *)          
    | `Sself -> fun strm -> entry.estart 0 strm 
    | `Skeyword kwd -> fun strm ->
        (match XStream.peek strm with
        | Some (`KEYWORD tok,_) when tok = kwd ->
            (XStream.junk strm ; Gaction.mk tok )
        |_ -> raise XStream.NotConsumed )
    | `Stoken (f, _,_) -> fun strm ->
        match XStream.peek strm with
        |Some (tok,_) when f tok -> (XStream.junk strm; Gaction.mk tok)
        |_ -> raise XStream.NotConsumed in with_loc (aux s)



(* entrance for the start [clevn] is the current level *)  
let start_parser_of_levels entry =
  let rec aux clevn  (xs:  level list) : int ->  Gaction.t FToken.parse =
    match xs with 
    | [] -> fun _ -> fun _ -> raise XStream.NotConsumed  
    | lev :: levs ->
        let hstart = aux  (clevn+1) levs in
        match lev.lprefix with
        | DeadEnd -> hstart (* try the upper levels *)
        | tree ->
            let cstart = 
              parser_of_tree entry (clevn, lev.assoc) (ArgContainer.create ())tree  in
            (* the [start] function tires its associated tree. If it works
               it calls the [continue] function of the same level, giving the
               result of [start] as parameter.
               If this continue fails, the parameter is simply returned.
               If this [start] function fails, the start function of the
               next level is tested, if there is no more levels, the parsing fails
             *)    
            fun levn strm ->
              if levn > clevn && (not ([]=levs))then
                hstart levn strm (* only higher level allowed here *)
              else
                (try
                  let (act,loc) = cstart strm in
                  fun ()  ->
                    let a = Gaction.getf act loc in entry.econtinue levn loc a strm
                with  XStream.NotConsumed  -> (fun ()  -> hstart levn strm)) () in
  aux 0
    
let start_parser_of_entry entry =
  match entry.edesc with
  | Dlevels [] -> Gtools.empty_entry entry.ename
  | Dlevels elev -> start_parser_of_levels entry  elev
  | Dparser p -> fun _ -> p
    


let rec continue_parser_of_levels entry clevn = function
  | [] -> fun _ _ _ ->  fun _ -> raise XStream.NotConsumed
  | lev :: levs ->
      let hcontinue = continue_parser_of_levels entry  (clevn+1) levs in
      match lev.lsuffix with
      | DeadEnd -> hcontinue
          (* the continue function first tries the [continue] function of the next level,
             if it fails or if it's the last level, it tries its associated tree, then
             call itself again, giving the result as parameter. If the associated tree
             fails, it returns its extra parameter
           *)
      | tree ->
        let ccontinue = parser_of_tree entry (clevn, lev.assoc) (ArgContainer.create ()) tree in
        fun levn bp a strm ->
          if levn > clevn then
            hcontinue levn bp a strm
          else
            try hcontinue levn bp a strm
            with
            | XStream.NotConsumed ->
              let (act,loc) = ccontinue strm in
              let loc = FLoc.merge bp loc in
              let a = Gaction.getf2 act a loc in entry.econtinue levn loc a strm

  
let continue_parser_of_entry entry =
  (* debug gram "continue_parser_of_entry: @[<2>%a@]@." Print.text#entry entry in *)
  match entry.edesc with
  | Dlevels elev ->
    let p = continue_parser_of_levels entry 0 elev in
    (fun levn bp a strm -> try p levn bp a strm with XStream.NotConsumed -> a )
  | Dparser _ -> fun _ _ _ _ -> raise XStream.NotConsumed  

