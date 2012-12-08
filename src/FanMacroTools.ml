
module Ast = Camlp4Ast;
open Lib;
open LibUtil;
type item_or_def 'a =
    [ Str of 'a
    | Def of string and option (list string * Ast.expr)
    | Und of string
    | ITE of bool and list (item_or_def 'a) and list (item_or_def 'a)
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
          [ `UID $x -> (new Ast.reloc _loc)#expr e ]
        patt: Level "simple"
          [ `UID $x ->
            let p = Expr.substp _loc [] e
            in (new Ast.reloc _loc)#patt p ] |}
  | Some (sl, e) ->
      {:extend| Gram
        expr: Level "apply"
        [ `UID $x; S{param} ->
          let el =  match param with 
            [ {:expr| ($tup:e) |} -> Ast.list_of_expr e []
            | e -> [e] ]  in
          if List.length el = List.length sl then
            let env = List.combine sl el in
            (new Expr.subst _loc env)#expr e
          else
            incorrect_number _loc el sl ]
        patt: Level "simple"
        [ `UID $x; S{param} ->
          let pl = match param with
            [ {:patt| ($tup:p) |} -> Ast.list_of_patt p []
            | p -> [p] ] in
          if List.length pl = List.length sl then
            let env = List.combine sl pl in
            let p = Expr.substp _loc env e in
            (new Ast.reloc _loc)#patt p
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
        [ Some ([], _) -> {:delete| Gram expr: [`UID $x ]  patt: [`UID $x ] |}
        | Some (_, _) ->  {:delete| Gram expr: [`UID $x; S ] patt: [`UID $x; S] |}
        | None -> () ];
        defined := List.remove x !defined;
    end
  with
    [ Not_found -> () ];

let parse_def ~expr ~patt s =
  match Gram.parse_string expr (FanLoc.mk "<command line>") s with
  [ {:expr| $uid:n |} -> define ~expr ~patt None n
  | {:expr| $uid:n = $e |} -> define ~expr ~patt (Some ([],e)) n
  | _ -> invalid_arg s ];
    

(* This is a list of directories to search for INCLUDE statements. *)
let include_dirs = ref [];

(* Add something to the above, make sure it ends with a slash. *)
let add_include_dir str =
  if str <> "" then
    let str =
      if String.get str ((String.length str)-1) = '/'
      then str else str ^ "/"
    in include_dirs := !include_dirs @ [str]
  else ();
  
let parse_include_file entry =
  let dir_ok file dir = Sys.file_exists (dir ^ file) in
  fun file ->
    let file =
      try (List.find (dir_ok file) (!include_dirs @ ["./"])) ^ file
      with [ Not_found -> file ]
    in
    let ch = open_in file in
    let st = XStream.of_channel ch in
      Gram.parse entry (FanLoc.mk file) st;

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

















