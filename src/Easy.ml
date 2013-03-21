open Transform;
open FSig;
open Lib;
let _loc = FanLoc.ghost ;

(**
   For var, in most cases we just add a prefix
   mf_, so we just fix it here

   For Objects, tctor_var, always (`Fun (fun x -> x))
   FIXME we may need a more flexible way to compose branches
 *)
let gen_stru
    ?module_name
    ?(arity=1)
    ?(trail= {:exp| failwith "arity >= 2 in other branches" |} )
    ?cons_transform
    ~id:(id:basic_id_transform)  ~names  
    (* you must specify when arity >=2 *)
    ~mk_tuple  ~mk_record ~mk_variant ()= begin 
      let left_type_variable  = `Pre "mf_" ;
      let right_type_variable = `Pre "mf_";
      let left_type_id = id ;
      let right_type_id =
        match module_name with
        [None ->   (id:>full_id_transform)
        |Some m ->
          `Last (fun s -> {:ident| $uid:m.$(lid:basic_transform id s) |} ) ] ;
      let trail (_,number)=
        if number > 1 then
          let pat = EP.tuple_of_number {:pat| _ |} arity in 
          Some {:case| $pat:pat -> $trail |}
        else (* {:case| |} *) None ;
      let names = names ;
      let mk_record = mk_record ;
      let cons_transform = cons_transform;
      Frame.check names ;
      Frame.(stru_of_module_types
               ?module_name
               ?cons_transform
               ~arity
               ~names
               ~trail
               ~mk_variant
               ~left_type_id
               ~left_type_variable
               ~mk_record
               (normal_simple_exp_of_ctyp
                  ~arity ~names ~mk_tuple
                  ~right_type_id
                  ~left_type_id ~right_type_variable)
               );
    end;
let gen_object
    ?module_name
    ?(arity=1)
    ?(trail= {:exp| failwith "arity >= 2 in other branches" |} )
    ?cons_transform
    ~kind
    ~base
    ~class_name = 
  let make ~names ~mk_tuple  ~mk_record  ~mk_variant ()= 
    begin
      Frame.check names ;
      let left_type_variable  = `Pre "mf_";
      let right_type_variable =
        `Exp (fun
          [v -> let v = basic_transform left_type_variable v
          in  {:exp| $lid:v self |} ]) ;
     let left_type_id  = `Pre "";
     let right_type_id  =
       `Obj (basic_transform left_type_id) ;
     let trail (_,number)=
       if number > 1 then
         let pat = EP.tuple_of_number {:pat| _ |} arity in 
         Some {:case| $pat:pat -> $trail |}
       else None in
    Frame.(obj_of_module_types
             ?cons_transform
             ?module_name
             ~arity
             ~names
             ~trail
             ~left_type_variable
             ~mk_record
             ~mk_variant
             base
             class_name
             (obj_simple_exp_of_ctyp
                ~right_type_id ~left_type_variable ~right_type_variable
                 ~names ~arity ~mk_tuple  )
             kind)
    end in
  make
;



















