open Ast;
open FanUtil;
open Meta;
open AstLoc;

let meta_loc_exp _loc loc =
  match !AstQuotation.current_loc_name with
  [ None -> lid _loc !FanLoc.name
  | Some "here" -> meta_loc _loc loc
  | Some x -> lid _loc x  ];;


let meta_loc_pat _loc _ =  {:pat| _ |}; (* we use [subst_first_loc] *)

  
  
let gm () =
  match !FanConfig.compilation_unit with
  [Some "FanAst" -> "" 
  | Some _ -> "FanAst" 
  | None ->  "FanAst"];

  
let antiquot_expander ~parse_pat ~parse_exp = object
  inherit Objs.map as super;
  method! pat (x:pat)= with pat'
    match x with 
    [`Ant(_loc, {cxt;sep;decorations;content=code}) ->
      (* when the antiquotation appears in the pattern position,
         its final context is [pat]
       *)
      let mloc _loc =meta_loc_pat  _loc _loc in
      let e = parse_pat _loc code in
      match (decorations,cxt,sep) with
      [("anti",_,_) -> {| `Ant ($(mloc _loc), $e) |}
      |("uid",_,_) -> {|`Uid($(mloc _loc), $e)|}
      |("lid",_,_) -> {|`Lid($(mloc _loc), $e)|}
      |("par",_,_) ->  {| `Par ($(mloc _loc), $e)|}
      |("seq",_,_) -> {| `Seq ($(mloc _loc), $e) |}
      |("flo",_,_) -> {| `Flo($(mloc _loc), $e)|}
      |("int",_,_) -> {| `Int ($(mloc _loc), $e)|}
      |("int32",_,_)-> {| `Int32 ($(mloc _loc),$e)|}
      |("int64",_,_) -> {| `Int64($(mloc _loc),$e)|}
      |("nativeint",_,_) -> {|`NativeInt ($(mloc _loc),$e)|}
      |("chr",_,_) -> {|`Chr($(mloc _loc),$e)|}
      |("str",_,_) -> {|`Str($(mloc _loc),$e)|}
      |("vrn","exp",_) -> {|`Vrn($(mloc _loc),$e)|}
      |("vrn","pat",_) -> {|`Vrn($(mloc _loc),$e)|}
    | _ -> super#pat e ]
 | e -> super#pat e];
  method! exp (x:exp) = with exp'
    match x with 
    [`Ant(_loc,{cxt;sep;decorations;content=code}) ->
      let mloc _loc = (meta_loc_exp _loc _loc :> exp) in
      let e = parse_exp _loc code in
      match (decorations,cxt,sep) with
      [ ("anti",_,__) -> {|`Ant($(mloc _loc),$e)|}
      | ("par",_,_) -> {|`Par($(mloc _loc),$e)|}
      | ("seq",_,_) -> {|`Seq($(mloc _loc),$e)|}
      | ("vrn","exp",_) -> {|`Vrn($(mloc _loc),$e)|}
      | ("vrn","pat",_) -> {|`Vrn($(mloc _loc),$e)|}
      | ("lid",_,_) -> {|`Lid($(mloc _loc),$e)|}
      | ("uid",_,_) -> {|`Uid($(mloc _loc),$e)|}
      | ("str",_,_) ->  {|`Str($(mloc _loc),$e)|}
      | ("chr",_,_) -> {|`Chr ($(mloc _loc), $e)|}
      | ("int",_,_) -> {|`Int($(mloc _loc),$e)|}
      | ("int32",_,_) -> {|`Int32($(mloc _loc),$e)|}
      | ("int64",_,_) -> {|`Int64($(mloc _loc),$e)|}
      | ("flo",_,_) -> {|`Flo($(mloc _loc),$e)|}
      | ("nativeint",_,_) -> {|`NativeInt ($(mloc _loc),$e)|}
      | ("`nativeint",_,_) ->
          let e = {| Nativeint.to_string $e |} in
          {| `NativeInt ($(mloc _loc), $e) |}
      | ("`int",_,_) ->
          let e = {|string_of_int $e |} in
          {| `Int ($(mloc _loc), $e) |}
      | ("`int32",_,_) ->
          let e = {|Int32.to_string $e |} in
          {| `Int32 ($(mloc _loc), $e) |}
      | ("`int64",_,_) ->
          let e = {|Int64.to_string $e |} in
          {| `Int64 ($(mloc _loc), $e) |}
      | ("`chr",_,_) ->
          let e = {|Char.escaped $e|} in
          {| `Chr ($(mloc _loc), $e) |}
      | ("`str",_,_) ->
          let e = {|String.escaped $e |} in
          {| `Str ($(mloc _loc), $e) |}
      | ("`flo",_,_) ->
          let e = {| FanUtil.float_repres $e |} in
          {| `Flo ($(mloc _loc), $e) |}
      | ("`bool",_,_) ->
          let x = {| `Lid ($(mloc _loc), (if $e then "true" else "false" )) |} in
          {| {| $(id:$x)  |} |}
      | ("list","module_exp",_) ->
          {| $(uid:gm()).app_of_list $e |}
      | ("list","module_type",_) ->
          {| $(uid:gm()).mtApp_of_list $e |}
      | ("list","ident",_) ->
          {| $(uid:gm()).dot_of_list $e |}
      | ("list",
         ("binding"|"module_binding"|
          "with_constr"|"class_type"|
          "class_exp"|"ctypand"),_) ->
            {| $(uid:gm()).and_of_list $e |}
      |("list","ctyp*",_) ->
          {| $(uid:gm()).sta_of_list $e |}
      |("list","ctyp|",_)
      |("list","case",_) ->
          {| $(uid:gm()).bar_of_list $e |}
      |("list","ctyp&",_) ->
          {| $(uid:gm()).amp_of_list $e |}
      |("listlettry","case",_) ->
          {| (($(uid:gm()).match_pre)#case
                ($(uid:gm()).bar_of_list $e)) |}
      |("antilettry","case",_) ->
          {| $(uid:gm()).match_pre#case (`Ant ($(mloc _loc), $e)) |}
      |("lettry","case",_) ->
          {| $(uid:gm()).match_pre#case $e |}
      |("list",("ctyp,"|"pat,"|"exp,"),_) ->
          {| $(uid:gm()).com_of_list $e |}
      |("list",
        ("binding;"|"stru"
      |"sig_item"|"class_sig_item"
            |"cstru"|"rec_exp"
            |"ctyp;"|"pat;"|"exp;"),_) ->
                {| $(uid:gm()).sem_of_list $e |}
      | _ -> super#exp e]
      | e -> super#exp e];
  end;
                  

