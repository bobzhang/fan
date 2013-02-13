open LibUtil;
open Llvm;
exception Error of string;
let context = global_context ();
let the_module = create_module context "my cool jit";
let builder = builder context;
let named_values: Hashtbl.t string llvalue = Hashtbl.create 10;
let double_type = double_type context;

let rec expr : LAst.expr -> llvalue = fun
  [ `Number n -> const_float double_type n
  | `Variable n ->
      try Hashtbl.find  named_values n with
      [Not_found ->raise (Error "unknown variable name")]
  |`Binary(op,l,r) ->
      let lhs = expr l in
      let rhs = expr r in
      match op with
      ['+' -> build_fadd lhs rhs "addtmp" builder
      |'-' -> build_fsub lhs rhs "addtmp" builder
      |'*' -> build_fmul lhs rhs "multmp" builder
      |'<' ->
          let i = build_fcmp Fcmp.Ult lhs rhs "cmptmp" builder in
          build_uitofp i double_type "booltmp" builder
      | x -> failwithf "%c not supported yet"  x]
   |`Call(callee,args) ->
       let callee =
         match lookup_function callee the_module with
         [Some x -> x | None -> raise (Error "unknown function referenced")] in
       let params= params callee in
       let () = if not (Array.length params = Array.length args ) then
         raise (Error "incorrect # arguments passed") in
       let args = Array.map expr args in
       build_call callee args "calltmp" builder ];

let proto = function
  [`Prototype(name,args) ->
    let doubles = Array.(make (length args) double_type) in
    let ft  = function_type double_type doubles in
    let f =
      match lookup_function name the_module with
      [None -> declare_function name ft the_module
      |Some f -> begin 
          if block_begin f <> At_end f then
            raise (Error "redefinition of function");
          (* TODO *)
          if element_type (type_of f) <> ft then
            raise (Error "redefinition of function with different # args");
          f
      end] in begin 
        Array.iteri
          (fun i a -> let n = args.(i) in begin
            set_value_name n a;
            Hashtbl.add named_values n a;
          end) (params f); f
      end
 ];  
let func : LAst.func -> llvalue= fun
  [`Function(f,body) -> begin 
    Hashtbl.clear named_values;
    let the_function = proto f;
    let bb = append_block context "entry" the_function ;
    position_at_end bb builder;
    try
      let ret_val = expr body in
      let _  = build_ret ret_val builder in begin 
        Llvm_analysis.assert_valid_function the_function;
        the_function
      end
    with [e -> begin delete_function the_function; raise e end]
  end
 ];
