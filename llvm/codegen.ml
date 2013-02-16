open LibUtil;
open Llvm;
open Format;
exception Error of string;
let context = global_context ();
let the_module = create_module context "my cool jit";
let builder = builder context;
let named_values: Hashtbl.t string llvalue = Hashtbl.create 10;
let double_type = double_type context;


let create_entry_block_alloca the_function var_name =
  let builder = builder_at context (instr_begin (entry_block the_function)) in
  build_alloca double_type var_name builder;
  
let rec expr : LAst.expr -> llvalue = fun
  [ `Number n -> const_float double_type n
  | `Variable n ->
      try Hashtbl.find  named_values n with
      [Not_found ->raise (Error (sprintf "unknown variable name %s" n))]
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
       build_call callee args "calltmp" builder
   |`If(cond,then_,else_) -> begin 
       let cond = expr cond;
       let zero = const_float double_type 0.0 ;
       let cond_val = build_fcmp Fcmp.One cond zero "ifcond" builder ;

       let start_bb = insertion_block builder;
       let the_function = block_parent start_bb;

       let then_bb = append_block context "then" the_function;  
       position_at_end then_bb builder;
       let then_val = expr then_;
       let new_then_bb = insertion_block builder;

       let else_bb = append_block context "else" the_function;
       position_at_end else_bb builder;
       let else_val = expr else_;
       let new_else_bb = insertion_block builder ;

       let merge_bb = append_block context "ifcont" the_function ;
       position_at_end merge_bb builder;
       let incoming = [(then_val, new_then_bb);(else_val,new_else_bb)];
       let phi = build_phi incoming "iftmp" builder;

       position_at_end start_bb builder;
       ignore(build_cond_br cond_val then_bb else_bb builder);
       position_at_end new_then_bb builder;
       ignore(build_br merge_bb builder);
       position_at_end new_else_bb builder;
       ignore(build_br merge_bb builder);
         
       position_at_end merge_bb builder;
       phi
   end
   |`For(var_name,start,end_,step_val,body) -> begin
       let start_val = expr start ;
       let preheader_bb = insertion_block builder;
       let the_function = block_parent preheader_bb ;
       let loop_bb = append_block context "loop" the_function ;
       ignore (build_br loop_bb builder);

       position_at_end loop_bb builder;
       let variable = build_phi [(start_val,preheader_bb)] var_name builder;
         
       let old_val =
         Hashtbl.find_opt named_values var_name;
       Hashtbl.replace named_values var_name variable;  
       ignore (expr body);

       let step_val =
         match step_val with
         [Some x -> expr x
         |None -> const_float double_type 1.0];
           
       let next_var = build_fadd variable step_val "nextvar" builder;
       let end_cond = expr end_ ;

       let zero = const_float double_type 0.0 ;
       let end_cond = build_fcmp Fcmp.One end_cond zero "loopcond" builder;

       let loop_end_bb = insertion_block builder;

       let after_bb = append_block context "afterloop" the_function;
         
       ignore (build_cond_br end_cond loop_bb after_bb builder);
         
       position_at_end after_bb builder;

       add_incoming (next_var, loop_end_bb) variable;
       match old_val with
       [Some old_val -> Hashtbl.replace named_values var_name old_val
       |None  -> ()];
       const_null double_type  
   end
   | `Var(var_names,body) -> begin 
       let old_bindings = ref [];
       let the_function = block_parent (insertion_block builder);

       Array.iter
           (fun (var_name,init) ->
             let init_val =
               match init with
               [Some init -> expr init
               |None -> const_float double_type 0.0] in

             let alloca = create_entry_block_alloca the_function var_name in begin
               ignore (build_store init_val alloca builder);
               match Hashtbl.find_opt named_values var_name with
               [Some i ->Ref.modify old_bindings (cons (var_name,i))
               |None -> ()];
               Hashtbl.add named_values var_name alloca
             end
           ) var_names;

       let body = expr body;
       List.iter
           (fun (var_name,old_value) ->
             Hashtbl.add named_values var_name old_value) !old_bindings;
      body
   end
 | `Sem(a,b) -> begin 
     ignore (expr a);
     expr b
 end];

(*
  Note first that this function returns a "Function*"
  instead of a "Value*"
  (although at the moment they both are modeled by llvalue in ocaml).

   Because a “prototype” really talks about the external interface for a function
  (not the value computed by an expression), it makes sense for it to return
  the LLVM Function it corresponds to when codegen’d

  Note that Types in LLVM are uniqued just like Constant‘s are, so
  you don’t “new” a type, you “get” it.
 *)
  
let proto = function
  [`Prototype(name,args) ->
    let doubles = Array.(make (length args) double_type) in
    let ft  = function_type double_type doubles in
    let f =
      match lookup_function name the_module with
      [None ->
        (* “external linkage” means that the function may
           be defined outside the current module and/or
           that it is callable by functions outside the module. *)
        declare_function name ft the_module
      |Some f -> begin 
          if block_begin f <> At_end f then
            raise (Error "redefinition of function");
          (* TODO *)
          if element_type (type_of f) <> ft then
            raise (Error "redefinition of function with types");
          f
      end] in begin 
        Array.iteri
          (fun i a -> let n = args.(i) in begin
            set_value_name n a;
            (* FIXME *)
            Hashtbl.add named_values n a;
          end) (params f); f
      end
 ];  
let func the_fpm : LAst.func -> llvalue= fun
  [`Function(f,body) -> begin 
    Hashtbl.clear named_values;
    let the_function = proto f;
    let bb = append_block context "entry" the_function ;
    position_at_end bb builder;
    try
      let ret_val = expr body in
      let _  = build_ret ret_val builder in begin 
        Llvm_analysis.assert_valid_function the_function;

        (* add an optmization layer *)
        ignore (PassManager.run_function the_function the_fpm);
        the_function
          
      end
    with [e -> begin delete_function the_function; raise e end]
  end
 ];

let top the_fpm = fun
  [ (#LAst.proto as x) -> proto x 
  | (#LAst.func as x)-> func the_fpm x ];
let tops the_fpm xs = List.map (top the_fpm) xs;
  
open Llvm_executionengine;
open Llvm_target;
(* open Llvm_scalar_opts  ; *)

(* Create the JIT. *)
let the_execution_engine = ExecutionEngine.create the_module; 
let the_fpm = PassManager.create_function the_module;  

(* Set up the optimizer pipeline.  Start with registering info about how the
 * target lays out data structures.
   The first pass is basically boilerplate, it adds a pass
   so that later optimizations know how the data structures in
   the program are laid out. 
 *)
DataLayout.add (ExecutionEngine.target_data the_execution_engine) the_fpm;

(* Do simple "peephole" optimizations and bit-twiddling optzn. *)
(* add_instruction_combination the_fpm; *)

(* (\* reassociate expressions. *\) *)
(* add_reassociation the_fpm; *)

(* (\* Eliminate Common SubExpressions. *\) *)
(* add_gvn the_fpm; *)

(* (\* Simplify the control flow graph (deleting unreachable blocks, etc). *\) *)
(* add_cfg_simplification the_fpm; *)

ignore (PassManager.initialize the_fpm);  


 (*
   Because the LLVM JIT compiler matches the native platform ABI, this means that
   you can just cast the result pointer to a function pointer of that type and call it directly.
   This means, there is no difference between JIT compiled code and native
   machine code that is statically linked into your application.

   The JIT provides a number of other more advanced interfaces for things like
   freeing allocated machine code, rejit’ing functions to update them, etc.
  *) 
