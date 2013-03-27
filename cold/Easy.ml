open Transform
open FSig
open Lib
let _loc = FanLoc.ghost
let gen_stru ?module_name  ?(arity= 1)  ?(trail=
  (`App
     (_loc, (`Id (_loc, (`Lid (_loc, "failwith")))),
       (`Str (_loc, "arity >= 2 in other branches"))) : Ast.exp ))
   ?cons_transform  ~id:(id : basic_id_transform)  ~names  ~mk_tuple 
  ~mk_record  ~mk_variant  () =
  let left_type_variable = `Pre "mf_" in
  let right_type_variable = `Pre "mf_" in
  let left_type_id = id in
  let right_type_id =
    match module_name with
    | None  -> (id :>full_id_transform)
    | Some m ->
        `Last
          ((fun s  ->
              (`Dot
                 (_loc, (`Uid (_loc, m)),
                   (`Lid (_loc, (basic_transform id s)))) : Ast.ident ))) in
  let trail (_,number) =
    if number > 1
    then
      let pat = EP.tuple_of_number (`Any _loc : Ast.pat ) arity in
      Some (`Case (_loc, pat, trail) : Ast.case )
    else None in
  let names = names in
  let mk_record = mk_record in
  let cons_transform = cons_transform in
  Frame.check names;
  let open Frame in
    stru_of_module_types ?module_name ?cons_transform ~arity ~names ~trail
      ~mk_variant ~left_type_id ~left_type_variable ~mk_record
      (normal_simple_exp_of_ctyp ~arity ~names ~mk_tuple ~right_type_id
         ~left_type_id ~right_type_variable)
let gen_object ?module_name  ?(arity= 1)  ?(trail=
  (`App
     (_loc, (`Id (_loc, (`Lid (_loc, "failwith")))),
       (`Str (_loc, "arity >= 2 in other branches"))) : Ast.exp ))
   ?cons_transform  ~kind  ~base  ~class_name  =
  let make ~names  ~mk_tuple  ~mk_record  ~mk_variant  () =
    Frame.check names;
    (let left_type_variable = `Pre "mf_" in
     let right_type_variable =
       `Exp
         (fun v  ->
            let v = basic_transform left_type_variable v in
            (`App
               (_loc, (`Id (_loc, (`Lid (_loc, v)))),
                 (`Id (_loc, (`Lid (_loc, "self"))))) : Ast.exp )) in
     let left_type_id = `Pre "" in
     let right_type_id = `Obj (basic_transform left_type_id) in
     let trail (_,number) =
       if number > 1
       then
         let pat = EP.tuple_of_number (`Any _loc : Ast.pat ) arity in
         Some (`Case (_loc, pat, trail) : Ast.case )
       else None in
     let open Frame in
       obj_of_module_types ?cons_transform ?module_name ~arity ~names ~trail
         ~left_type_variable ~mk_record ~mk_variant base class_name
         (obj_simple_exp_of_ctyp ~right_type_id ~left_type_variable
            ~right_type_variable ~names ~arity ~mk_tuple) kind) in
  make