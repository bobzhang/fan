

open FAst
open AstLib
open LibUtil
  
type 'a item_or_def  =
  | Str of 'a
  | Def of string * ( string list * exp)option 
  | Und of string

let defined = ref []

        
let substp loc (env: (string * pat) list) =
  let bad_pat _loc =
    FLoc.errorf _loc "this macro cannot be used in a pattern (see its definition)" in
  let rec loop (x:exp)= with {pat:exp;exp:pat}
    match x with
    | {| $e1 $e2 |} -> {@loc| $(loop e1) $(loop e2) |} 
    | {| $lid:x |} ->
        begin try List.assoc x env with
           Not_found -> {@loc| $lid:x |}
        end
    | {| $uid:x |} ->
        (try List.assoc x env with Not_found -> {@loc| $uid:x |})
    (* | #ep as x -> (x:exp) *)
    | {| $int:x |} -> {@loc| $int:x |}
    | {| $str:s |} -> {@loc| $str:s |}
    | {| $par:x |} -> {@loc| $(par:loop x) |}
    | {| $x1, $x2 |} -> {@loc| $(loop x1), $(loop x2) |}
    | {| { $bi } |} ->
        let rec substbi = with {pat:rec_exp;exp:pat} function
          | {| $b1; $b2 |} ->
            `Sem(_loc,substbi b1, substbi b2)
          | {| $id:i = $e |} -> `RecBind (loc,i,loop e)(* {@loc| $i = $(loop e) |} *)
          | _ -> bad_pat _loc  in
        {@loc| { $(substbi bi) } |}
    | _ -> bad_pat loc  in loop

(*
  [env] is a list of [string*exp],

  traverse the [exp] node
  when the identifier in pos exp in the exp has a speical meaning, using that instead
  when the identifier in pos pat in the exp has a special meaning,
  try to convert the exp meaning into pat and use that instead
 *)  
class subst loc env =  object
  inherit Objs.reloc loc as super
  method! exp = with exp function
    | {| $lid:x |} | {| $uid:x |} as e ->
         (try List.assoc x env with Not_found -> super#exp e)
    | {| LOCATION_OF $lid:x |} | {| LOCATION_OF $uid:x |} as e ->
          (try
            let loc = loc_of (List.assoc x env) in
            let (a, b, c, d, e, f, g, h) = FLoc.to_tuple loc in
            {| FLoc.of_tuple
              ($`str:a, $`int:b, $`int:c, $`int:d,
               $`int:e, $`int:f, $`int:g,
               $(if h then {| true |} else {| false |} )) |}
          with  Not_found -> super#exp e)
    | e -> super#exp e
  method! pat =  function
    | {:pat| $lid:x |} | {:pat| $uid:x |} as p ->
      (* convert expession into pattern only *)
        (try substp loc [] (List.assoc x env) with 
          Not_found -> super#pat p)
    | p -> super#pat p 
end

    
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
            let p = substp _loc [] e in
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
            (new subst _loc env)#exp e
          else
            incorrect_number _loc el sl ]
        let params:
        [ L1 param SEP "," {xs} -> xs  ]
        let param:
        [ `Lid x -> x ]
            
        pat: Level "simple"
        [ `Uid $y; S{param} ->
          let pl =
            match param with
            | {:pat| ($par:p) |} -> list_of_com p [] (* precise *)
            | p -> [p]  in
          if List.length pl = List.length sl then
            let env = List.combine sl pl in
            let p = substp _loc env e in
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
  
let  execute_macro ~exp ~pat nil = function
    | Str i -> i
    | Def (x, eo) -> begin  define ~exp ~pat eo x; nil  end
    | Und x -> begin  undef ~exp ~pat x; nil  end

















