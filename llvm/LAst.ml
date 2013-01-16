open Llvm;

type expr =
  [= `Number of float
  | `Variable of string
  | `Binary of (char * expr * expr)
  | `Call of (string * array expr) ]
and proto = [= `Prototype of (string * array string) ]
and func = [= `Function of (proto * expr) ];

let g = Gram.create_gram ();

{:extend.create|
  (g:Gram.t)
  (expr: Gram.t expr)
  (proto: Gram.t proto)
  (func: Gram.t func)
|};

  
{:extend| (g:Gram.t)  
expr:
  {"+"
   [S{a};"+";S{b} -> `Binary ('+',a,b)
   |S{a};"-";S{b} -> `Binary ('-',a,b)]
   "*"  
   [S{a};"*";S{b} -> `Binary ('*',a,b)
   |S{a};"/";S{b} -> `Binary ('/',a,b)]
   "call"
   [ `Lid x; "(";L0 expr SEP "," {ls};")" -> `Call(x,Array.of_list ls) ]  
   "simple"
   [`Flo(x,_) -> `Number x
   |`Lid x -> `Variable x 
   |"(";S{x};")" -> x ]}
  
proto:
  [ OPT "extern"; `Lid x; "("; L0 [`Lid x -> x ] SEP ","{ls}; ")"
    ->  `Prototype(x,Array.of_list ls) ]
func:
  [ "def"; `Lid x; "("; L0 [`Lid x -> x ] SEP "," {xs} ;")" ; expr{e} ->
    `Function(`Prototype(x,Array.of_list xs),e) ]
|};


(* Gram.parse_string test FanLoc.string_loc "extern x y"  ; *)
  (* Gram.parse_string test FanLoc.string_loc "def extern x(z,y)  x + y + z"; *)
(* {:extend|(LAst.g:Gram.t) LAst.expr: Level "simple"[`LID x -> `Variable x ] |}; *)
let context = global_context();
let the_module = create_module context "jit";
let builder = builder context;
let named_values : Hashtbl.t string llvalue = Hashtbl.create 40;
let double_type = double_type context;

exception  Error of string;

let rec codegen_expr : expr -> llvalue= fun
  [`Number n ->
    const_float double_type n 
  |`Variable name ->
      try Hashtbl.find named_values name with
        Not_found -> raise (Error "unknown variable name")
  | `Binary (op,l,r) ->
    let lhs = codegen_expr l in
    let rhs = codegen_expr r in
    match op with
    ['+' -> build_fadd lhs rhs "addtmp" builder
    |'-' -> build_fsub lhs rhs "subtmp" builder
    |'*' -> build_fadd lhs rhs "multmp" builder
    |'<' ->
        let i = build_fcmp Fcmp.Ult lhs rhs "cmptmp" builder in
        build_uitofp i double_type "booltmp" builder
    | _ -> raise (Error "invalid binary operator") ]  

  | `Call(callee,args) ->
      let callee =
        match lookup_function callee the_module with
        [Some callee -> callee
        |None -> raise (Error "unknown function referenced")] in
          
      let params = params callee in
      let _ =
        if Array.length params == Array.length args then
          ()
        else
          raise (Error "incorrect # arguments parssed ") in
      let args = Array.map codegen_expr args in
      build_call callee args "calltmp" builder  
  ];
  
let  codegen_proto  = fun
  [`Prototype(name,args) ->
    let doubles = Array.make (Array.length args) double_type in
    let ft = function_type double_type doubles in
    let f =
      match lookup_function name the_module with
      [ None -> declare_function name ft the_module
      | Some f -> begin 
          if block_begin f <> At_end f then
            raise (Error "redefinition of function")
          else ();
          if element_type (type_of f) <> ft then
            raise (Error "redefinition of function with different # args")
          else ();
          f
      end ] in
    begin 
      Array.iteri (fun i a ->
      let n = args.(i) in
      let () = set_value_name n a in
      Hashtbl.add named_values n a) (params f);
      f
    end
 ];

let codegen_func = fun
  [`Function(proto,body) -> begin
    Hashtbl.clear named_values;
    let the_function = codegen_proto proto ;
    let bb = append_block context "entry" the_function;
    position_at_end bb builder;
    try
      let ret_val = codegen_expr body in
      let _ = build_ret ret_val builder in
      let () = Llvm_analysis.assert_valid_function the_function in
      the_function
    with
      e -> begin delete_function the_function ; raise e end
  end
 ];
