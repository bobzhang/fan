open Format;
open Fan_easy;
open Lib_common;
open Fan_basic;
open Fan_frame;
open Fan_transform;
open Fan_sig;
open Fan_expr;
open Camlp4.PreCast;
open List;
value mk_variant cons params =
    params |> map (fun [ {ty_expr;_} -> ty_expr]) |> apply (of_str cons) ;
value mk_tuple params =
    params |> map (fun [{ty_expr; _ } -> ty_expr]) |> tuple_of_list ;
value mk_record cols =
    cols |> map (fun [ {record_label; record_info={ty_expr;_ } ; _ }  ->
          (record_label,ty_expr) ] )  |> mk_record ;
value trail = (
     fun
     [(_,1) -> <:match_case< >>
     |(_,_) ->  <:match_case< (_,_) -> invalid_arg "map2 failure" >> ] )
;
  
module M = Fan_frame.Make(struct
  value mk_variant = mk_variant;
  value mk_record = mk_record;
  value mk_tuple = mk_tuple;
  value var = `Pre "mf_";
  value arity = 2;
  value right_var =
    `Exp (fun
      [v -> let v = basic_transform var v
      in  <:expr< .$lid:v$. self >> ]) ;
  value tctor_var = `Fun (fun x -> x);
  value id = `Obj ;
  value trail = trail;
  value names = ["fmt"] ;
  end);
  
(*
  {[
  M.obj_simple_expr_of_ctyp <:ctyp< m_list (tree 'a) >> |>
  (fun [Fan_common.Left x -> x |> Fan_expr.eprint ] );
  
  self#m_list (fun self -> self#tree mf_a)
  ]}
 *)


















