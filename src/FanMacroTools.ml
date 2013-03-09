
open AstLoc;
open Lib;
open LibUtil;
type 'a item_or_def  =
    [ Str of 'a
    | Def of string * option (list string * expr)
    | Und of string
    | ITE of bool * list (item_or_def 'a) * list (item_or_def 'a)
    | Lazy of Lazy.t 'a ];

let defined = ref [];
let is_defined i = List.mem_assoc i !defined;
let incorrect_number loc l1 l2 =
  FanLoc.raise loc
    (Failure
        (Printf.sprintf "expected %d parameters; found %d"
            (List.length l2) (List.length l1)));



let define ~expr ~patt eo x  = begin 
  match eo with
  [ Some ([], e) ->
    {:extend|Gram
        expr: Level "simple"
          [ `Uid $x -> (new FanObjs.reloc _loc)#expr e ]
        patt: Level "simple"
          [ `Uid $x ->
            let p = Expr.substp _loc [] e
            in (new FanObjs.reloc _loc)#patt p ] |}
  | Some (sl, e) ->
      {:extend| Gram
        expr: Level "apply"
        [ `Uid $x; S{param} ->
          let el =  match param with 
            [ {:expr| ($tup:e) |} -> list_of_com e []
            | e -> [e] ]  in
          if List.length el = List.length sl then
            let env = List.combine sl el in
            (new Expr.subst _loc env)#expr e
          else
            incorrect_number _loc el sl ]
        patt: Level "simple"
        [ `Uid $x; S{param} ->
          let pl = match param with
            [ {:patt| ($tup:p) |} -> list_of_com p [] (* precise *)
            | p -> [p] ] in
          if List.length pl = List.length sl then
            let env = List.combine sl pl in
            let p = Expr.substp _loc env e in
            (new FanObjs.reloc _loc)#patt p
          else
            incorrect_number _loc pl sl ] |}
  | None -> () ];
  defined := [(x, eo) :: !defined]
end;

let undef ~expr ~patt x =
  try
    begin
      let eo = List.assoc x !defined in
      match eo with
        [ Some ([], _) -> {:delete| Gram expr: [`Uid $x ]  patt: [`Uid $x ] |}
        | Some (_, _) ->  {:delete| Gram expr: [`Uid $x; S ] patt: [`Uid $x; S] |}
        | None -> () ];
        defined := List.remove x !defined;
    end
  with
    [ Not_found -> () ];

let parse_def ~expr ~patt s =
  match Gram.parse_string expr ~loc:(FanLoc.mk "<command line>") s with
  [ {:expr| $uid:n |} -> define ~expr ~patt None n
  | {:expr| $uid:n = $e |} -> define ~expr ~patt (Some ([],e)) n
  | _ -> invalid_arg s ];
    


  
let rec execute_macro ~expr ~patt nil cons = fun
  [ Str i -> i
  | Def (x, eo) -> begin  define ~expr ~patt eo x; nil  end
  | Und x -> begin  undef ~expr ~patt x; nil  end
  | ITE (b, l1, l2) -> execute_macro_list ~expr ~patt nil cons (if b then l1 else l2)
  | Lazy l -> Lazy.force l ] (* the semantics is unclear*)

and execute_macro_list ~expr ~patt nil cons =  fun
  [ [] -> nil
  | [hd::tl] -> (* The evaluation order is important here *)
      let il1 = execute_macro ~expr ~patt nil cons hd in
      let il2 = execute_macro_list ~expr ~patt nil cons tl in
      cons il1 il2 ] ;

(* Stack of conditionals. *)
let stack = Stack.create () ;

(* Make an ITE let by extracting the result of the test from the stack. *)
let make_ITE_result st1 st2 =
  let test = Stack.pop stack in
  ITE test st1 st2 ;

type branch = [ Then | Else ];

(* Execute macro only if it belongs to the currently active branch. *)
let execute_macro_if_active_branch ~expr ~patt _loc nil cons branch macro_def =
  let _ = Format.eprintf "execute_macro_if_active_branch@."in
  let test = Stack.top stack in
  let item =
    if (test && branch=Then) || ((not test) && branch=Else) then begin 
      let res = execute_macro ~expr ~patt nil cons macro_def;
      Format.eprintf "executing branch %s@." (if branch=Then then "Then" else "Else");
      res
    end
    else (* ignore the macro *)
      nil in
  Str(item) ;

















