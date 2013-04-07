(*    
let ant_pat  ~parse_pat (x:ant) k = with pat 
  match x with 
  [`Ant(_loc, {cxt;sep;decorations;content=code}) ->
      let mloc _loc =meta_loc_pat  _loc _loc in
      let e = parse_pat _loc code in
      match (decorations,cxt,sep) with
      [("anti",_,_) -> k {| `Ant ($(mloc _loc), $e) |}
      |("uid",_,_) -> k {|`Uid($(mloc _loc), $e)|}
      |("lid",_,_) ->  {|`Lid($(mloc _loc), $e)|}
            (* |("id","ctyp",_) -> {|`Id($(mloc _loc),$e)|} *)
      |("par",_,_) ->  Some {| `Par ($(mloc _loc), $e)|}
      |("seq",_,_) -> Some {| `Seq ($(mloc _loc), $e) |}
      |("flo",_,_) -> Some {| `Flo($(mloc _loc), $e)|}
      |("int",_,_) -> Some {| `Int ($(mloc _loc), $e)|}
      |("int32",_,_)-> Some {| `Int32 ($(mloc _loc),$e)|}
      |("int64",_,_) -> Some {| `Int64($(mloc _loc),$e)|}
      |("nativeint",_,_) -> Some {|`NativeInt ($(mloc _loc),$e)|}
      |("chr",_,_) -> Some {|`Chr($(mloc _loc),$e)|}
      |("str",_,_) -> Some {|`Str($(mloc _loc),$e)|}
      |("vrn","exp",_) -> Some {|`Vrn($(mloc _loc),$e)|}
      |("vrn","pat",_) -> Some {|`Vrn($(mloc _loc),$e)|}
      | _ -> None ] ];
let ant_exp ~parse_exp (x:ant) =  with exp 
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
                {| $(uid:gm()).sem_of_list $e |} ]];
*)

(* broken
   ideas add type annotations for specific variants, some
   variants can be annoated, some can not
   
   {:ctyp| ! 'a 'b 'c. 'a -> 'b -> 'c|}
   `TyPol
             (_loc,
             (`App
             (_loc,
             (`App
              (_loc,
                (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "a"))))),
                (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "b"))))))),
           (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "c"))))))),
             (`Arrow
             (_loc, (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "a"))))),
             (`Arrow
             (_loc,
             (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "b"))))),
             (`Quote (_loc, (`Normal _loc), (`Some (`Lid (_loc, "c"))))))))))
           *)        
          (* |("list","forall",_) -> *)
          (*     {| $(uid:gm()).tyVarApp_of_list $e |} *)
