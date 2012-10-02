open Camlp4.PreCast;
open Lib_common;
open Fan_basic;





value ctyp_eq t1 t2 =
   let strip_locs t = (Ast.map_loc (fun _ -> Ast.Loc.ghost))#ctyp t in
   strip_locs t1 = strip_locs t2
  ;

value arm tid tvars a l =  match a with
  [ <:ctyp< .$uid:_$. >> -> l
  | <:ctyp< $uid:cid$ of $parts$ >> ->
      let tdef = Fan_ctyp.apply <:ctyp< .$lid:tid$. >> tvars in
      let tdef' = Fan_ctyp.apply <:ctyp< .$lid:tid^"'"$. >> tvars in
      let parts = Ast.list_of_ctyp parts [] in
      let tdefs = List.length (List.filter (fun t -> ctyp_eq t tdef) parts) in
      let rec loop i =
        if i >= tdefs then l
        else
          let rec loop2 j = fun
            [ [] -> []
            | [h::t] when ctyp_eq h tdef ->
                let h = if j = i then tdef' else h in
                [h :: loop2 (j+1) t]
            | [h::t] -> [h :: loop2 j t] ] in
          [ <:ctyp< $uid:cid^string_of_int i$ of $list:loop2 0 parts$ >> :: loop (i+1) ]
      in
      loop 0
      | _ -> assert False]
;
(*    
let filter =
  object
    inherit Ast.map as super

    method str_item = function
      | <:str_item< type $Ast.TyDcl (_, tid, tvars, <:ctyp< [ .$arms$. ] >>, [])$ >> as si ->
          let arms = List.fold_right (arm tid tvars) (Ast.list_of_ctyp arms []) [] in
          let si' =
            <:str_item< type $Ast.TyDcl (_loc, tid^"'", tvars, <:ctyp< [ Top | .$list:arms$. ] >>, [])$ >> in
          <:str_item<
            $si$;
            $si'$
          >>
      | si -> super#str_item si
  end
*)


