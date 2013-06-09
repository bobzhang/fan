

open FAst
open AstLib
open LibUtil
  
type 'a item_or_def  =
  | Str of 'a
  | Def of string * ( string list * exp)option 
  | Und of string
  (* | ITE of bool * ('a item_or_def ) list  * ('a item_or_def  )list  *)
  (* | Lazy of 'a Lazy.t   *)

let defined = ref []
    
let define ~exp ~pat eo y  =
  let incorrect_number loc l1 l2 =
  FLoc.raise loc
    (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1))) in
  begin
    (match eo with
    | Some ([], e) ->
        {:extend|
          exp: Level "simple"
          [ `Uid $y -> (new Objs.reloc _loc)#exp e ]
          pat: Level "simple"
          [ `Uid $y ->
            let p = Exp.substp _loc [] e in
            (new Objs.reloc _loc)#pat p ] |}
    | Some (sl, e) ->
        {:extend| 
          exp: Level "apply"
          [ `Uid $y; S{param} ->
            let el =
              match param with 
              | {:exp| ($par:e) |} -> list_of_com e []
              | e -> [e]   in
          if List.length el = List.length sl then
            let env = List.combine sl el in
            (new Exp.subst _loc env)#exp e
          else
            incorrect_number _loc el sl ]
        pat: Level "simple"
        [ `Uid $y; S{param} ->
          let pl =
            match param with
            | {:pat| ($par:p) |} -> list_of_com p [] (* precise *)
            | p -> [p]  in
          if List.length pl = List.length sl then
            let env = List.combine sl pl in
            let p = Exp.substp _loc env e in
            (new Objs.reloc _loc)#pat p
          else
            incorrect_number _loc pl sl ] |}
    | None -> ());
  defined := (y, eo) :: !defined
end

let undef ~exp ~pat x =
  try
    begin
      (let eo = List.assoc x !defined in
      match eo with
      | Some ([], _) -> {:delete| Fgram exp: [`Uid $x ]  pat: [`Uid $x ] |}
      | Some (_, _) ->  {:delete| Fgram exp: [`Uid $x; S ] pat: [`Uid $x; S] |}
      | None -> ()) ;
      defined := List.remove x !defined;
    end
  with Not_found -> () 
  
let  execute_macro ~exp ~pat nil (* cons *) = function
    | Str i -> i
    | Def (x, eo) -> begin  define ~exp ~pat eo x; nil  end
    | Und x -> begin  undef ~exp ~pat x; nil  end
    (* | ITE (b, l1, l2) -> execute_macro_list ~exp ~pat nil cons (if b then l1 else l2) *)
    (* | Lazy l -> Lazy.force l  (\* the semantics is unclear*\) *)

(* and execute_macro_list ~exp ~pat nil (\* cons *\) = function *)
(*   | [] -> nil *)
(*   | hd::tl -> (\* The evaluation order is important here *\) *)
(*       let il1 = execute_macro ~exp ~pat nil (\* cons *\) hd in *)
(*       let il2 = execute_macro_list ~exp ~pat nil (\* cons *\) tl in *)
(*       cons il1 il2 *)

















