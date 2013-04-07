(** High-Level Virtual Machine (HLVM).

    The design and implementation of this high-performance garbage collected
    virtual machine is described in detail in the OCaml Journal articles
    "Building a Virtual Machine with LLVM" from January to March 2009.
*)

open Printf
open Llvm
open Llvm_executionengine
open Llvm_target
open Llvm_scalar_opts
open Llvm_analysis

module Options = struct
  (** Global boolean to enable viewing of generated functions. *)
  let view = ref false
  
  (** Global boolean to enable debug output in both the compiler and the
      generated code. Enabled by the command-line argument "--debug". *)
  let debug = ref false
  
  (** Compile without evaluating. *)
  let compile_only = ref false
  
  (** Allow the GC to be disabled. *)
  let gc_enabled = ref true

  (** Allow the shadow stack to be disabled. *)
  let shadow_stack_enabled = ref true

  (** Maximum depth of the shadow stacks, visit stack and allocated list. *)
  let max_depth = 1 lsl 23

  (** Tail call elimination enabled. *)
  let tco = ref true

  (** Number of ticks before a GC check for cooperative synchronization. *)
  let ticks = 65536

  (** Use a stack handler to catch stack overflows. *)
  let stack_handler = ref true

  (** The maximum number of times a recursive function will be unrolled. *)
  let unroll = 8
end

let rec list_to_string f sep () = function
  | [] -> ""
  | [h] -> f () h
  | h::t -> sprintf "%a%s%a" f h sep (list_to_string f sep) t
      
module Type = struct
  (** The type system. *)
  type t =
      [ `Unit
      | `Bool
      | `Byte
      | `Int
      | `Int64
      | `Float
      | `Struct of t list
      | `Array of t
      | `Function of t list * t
      | `Reference ]
  
  let rec eq (ty1: t) (ty2: t) = match ty1, ty2 with
    | `Struct tys1, `Struct tys2 -> eqs tys1 tys2
    | `Array ty1, `Array ty2 -> eq ty1 ty2
    | `Function(ty_args1, ty_ret1), `Function(ty_args2, ty_ret2) ->
	eqs (ty_ret1::ty_args1) (ty_ret2::ty_args2)
    | ty1, ty2 -> ty1 = ty2
  and eqs tys1 tys2 = List.for_all2 eq tys1 tys2
  
  open Printf
  
  let rec to_string () : t -> string = function
    | `Unit -> "`Unit"
    | `Bool -> "`Bool"
    | `Byte -> "`Byte"
    | `Int -> "`Int"
    | `Int64 -> "`Int64"
    | `Float -> "`Float"
    | `Struct tys -> sprintf "`Struct[%a]" to_strings tys
    | `Array ty -> sprintf "`Array(%a)" to_string ty
    | `Function(tys_arg, ty_ret) ->
        sprintf "`Function([%a], %a)" to_strings tys_arg to_string ty_ret
    | `Reference -> "`Reference"
  and to_strings tys = list_to_string to_string "; " tys
end

module Expr = struct
  (** Expressions of a first-order intermediate language. *)
  type t =
    | Null
    | Unit
        (** The value () of the type unit. *)
    | Bool of bool
        (** A literal boolean value. *)
    | Byte of int
	(** A literal byte value. *)
    | Int of int
        (** A literal native int value. *)
    | Int64 of Int64.t
        (** A literal 64-bit int value. *)
    | Float of float
        (** A literal 64-bit floating point value. *)
    | Struct of t list
        (** A struct literal. *)
    | GetValue of t * int
        (** Extract a field from a struct. *)
    | Var of string
        (** A variable. *)
    | UnArith of [`Neg] * t
	(** A unary arithmetic operation. *)
    | BinArith of [`Add|`Sub|`Mul|`Div|`Mod] * t * t
        (** A binary arithmetic operation. *)
    | Cmp of [`Lt|`Le|`Eq|`Ge|`Gt|`Ne] * t * t
        (** A comparison. *)
    | If of t * t * t
        (** An "if" expression. *)
    | Let of string * t * t
        (** Evaluate the first expression, bind the resulting value to the
	    given variable name and evaluate the last expression. *)
    | Alloc of t * t
        (** Allocate and initialize an array the given number of elements and
	    element value. *)
    | Length of t
        (** Find the length of the given array. *)
    | Get of t * t
        (** Get(a, i) gets the element at index "i" from the array "a". *)
    | Set of t * t * t
        (** Set(a, i, x) sets the element at index "i" in the array "a" to
            "x". *)
    | Apply of t * t list
        (** Applying the given function pointer to the given list of
            arguments. *)
    | Printf of string * t list
        (** Call C printf (unsafe). *)
    | IntOfInt of [`Byte | `Int | `Int64] * t
        (** Convert from one integer type to another. *)
    | IntOfFloat of [`Byte | `Int | `Int64] * t
        (** Convert a float to an int. *)
    | FloatOfInt of [`Float] * t
        (** Convert an int to a float. *)
    | Construct of string * t
        (** Construct a boxed value. *)
    | IsType of t * string
        (** Check the type of a value against the type with the given name. *)
    | Print of t
        (** Generic printing of any value. *)
    | Exit of t
        (** Call the C "exit" function. *)
    | AddressOf of t
        (** For internal use only. Return the address of the given reference
            type as a native integer. *)
    | Cast of t * string
        (** Cast the given reference value to the given type, returning the
            argument of the type constructor (unsafe). *)
    | Free of t
        (** For internal use only. Deallocate the given value. *)
    | Load of Llvm.llvalue * Type.t
        (** For internal use only. Load a value from an LLVM global variable. *)
    | Store of Llvm.llvalue * t
        (** For internal use only. Store a value to an LLVM global variable. *)
    | Visit of t
        (** For internal use only. Obtain the GC visit function associated with
            the type of the given value. *)
    | Llvalue of Llvm.llvalue * Type.t
        (** For internal use only. A literal LLVM value. *)
    | Magic of t * Type.t
        (** For internal use only. Convert between the two kinds of reference
            types: arrays and boxed values. *)
    | Return of t * Type.t
        (** For internal use only. Used to propagate tail calls. *)
    | GetMark of t
        (** For internal use only. Get the mark byte of a reference type. *)
    | SetMark of t * int
        (** For internal use only. Set the mark byte of a reference type. *)
    | Time
	(** Read the current time as a `Float. *)
    | CreateThread of t * t
	(** Create and initialize a thread and apply the given function to the
	    given argument on it. *)
    | JoinThread of t
	(** Join with the given thread. *)
    | CreateMutex
	(** Create a mutex. *)
    | UnsafeLockMutex of t
	(** Lock a mutex. *)
    | UnlockMutex of t
	(** Unlock a mutex. *)
    | ExtGetThreadLocal
	(** Read thread local data using POSIX thread local state. *)
    | ExtSetThreadLocal of t
	(** Write thread local data using POSIX thread local state. *)
    | GetThreadLocal
	(** Read thread local data. *)
    | SetThreadLocal of t
	(** Write thread local data. *)
    | Unsafe of t
	(** Do not inject GC-related instructions when compiling this
	    subexpression. *)
    | GetThreadTick
	(** Read the tick counter of the current thread (used to amortize
	    expensive GC checks). *)
    | SetThreadTick of t
	(** Write the tick counter of the current thread (used to amortize
	    expensive GC checks). *)

  (** Helper operators. *)
  let ( <: ) f g = Cmp(`Lt, f, g)
  let ( <=: ) f g = Cmp(`Le, f, g)
  let ( =: ) f g = Cmp(`Eq, f, g)
  let ( <>: ) f g = Cmp(`Ne, f, g)
  let ( >=: ) f g = Cmp(`Ge, f, g)
  let ( >: ) f g = Cmp(`Gt, f, g)
  let ( ~-: ) f = UnArith(`Neg, f)
  let ( +: ) f g = BinArith(`Add, f, g)
  let ( -: ) f g = BinArith(`Sub, f, g)
  let ( *: ) f g = BinArith(`Mul, f, g)
  let ( /: ) f g = BinArith(`Div, f, g)
  let ( %: ) f g = BinArith(`Mod, f, g)
  let ( &&: ) f g = If(f, g, Bool false)
  let ( ||: ) f g = If(f, Bool true, g)
  let rec compound = function
    | [] -> Unit
    | [h] -> h
    | h::t -> Let("", h, compound t)
  
  open Printf
  
  let logic_to_string () = function
    | `And -> "`And"
    | `Or -> "`Or"
  
  let arith_to_string () = function
    | `Add -> "`Add"
    | `Sub -> "`Sub"
    | `Mul -> "`Mul"
    | `Div -> "`Div"
    | `Mod -> "`Mod"
  
  let cmp_to_string () = function
    | `Lt -> "`Lt"
    | `Le -> "`Le"
    | `Eq -> "`Eq"
    | `Ne -> "`Ne"
    | `Ge -> "`Ge"
    | `Gt -> "`Gt"
  
  let rec to_string () = function
    | Null -> "Null"
    | Unit -> "Unit"
    | Bool b -> sprintf "Bool %b" b
    | Byte n -> sprintf "Byte %d" n
    | Int n -> sprintf "Int %d" n
    | Int64 n -> sprintf "Int64 %Ld" n
    | Float x -> sprintf "Float %g" x
    | Struct xs ->
        sprintf "Struct[%a]" (to_strings "; ") xs
    | GetValue(s, i) -> sprintf "GetValue(%a, %d)" to_string s i
    | Var s -> sprintf "Var \"%s\"" (String.escaped s)
    | UnArith(`Neg, f) ->
	sprintf "UnArith(`Neg, %a)" to_string f
    | BinArith(op, f, g) ->
        sprintf "BinArith(%a, %a, %a)"
	  arith_to_string op to_string f to_string g
    | Cmp(op, f, g) ->
        sprintf "Cmp(%a, %a, %a)" cmp_to_string op to_string f to_string g
    | If(p, t, f) ->
        sprintf "If(%a, %a, %a)" to_string p to_string t to_string f
    | Let(x, f, g) ->
        sprintf "Let(\"%s\", %a, %a)" (String.escaped x) to_string f to_string g
    | Alloc(f, g) ->
        sprintf "Alloc(%a, %a)" to_string f to_string g
    | Length f ->
        sprintf "Length(%a)" to_string f
    | Get(a, i) ->
        sprintf "Get(%a, %a)" to_string a to_string i
    | Set(a, i, x) ->
        sprintf "Set(%a, %a, %a)" to_string a to_string i to_string x
    | Apply(f, xs) ->
        sprintf "Apply(%a, [%a])" to_string f (to_strings "; ") xs
    | Printf(s, fs) ->
        sprintf "Printf(\"%s\", [%a])" (String.escaped s) (to_strings "; ") fs
    | IntOfInt(_, f) ->
        sprintf "IntOfInt(%a)" to_string f
    | IntOfFloat(_, f) ->
        sprintf "IntOfFloat(%a)" to_string f
    | FloatOfInt(_, f) ->
        sprintf "FloatOfInt(%a)" to_string f
    | Return(f, ty) ->
        sprintf "Return(%a, %a)" to_string f Type.to_string ty
    | Construct(constr, f) ->
        sprintf "Construct(\"%s\", %a)" constr to_string f
    | IsType(f, constr) ->
        sprintf "IsType(%a, \"%s\")" to_string f constr
    | Cast(f, constr) ->
        sprintf "Cast(%a, \"%s\")" to_string f constr
    | Visit f ->
        sprintf "Visit(%a)" to_string f
    | Print f ->
        sprintf "Print(%a)" to_string f
    | Exit f ->
        sprintf "Exit(%a)" to_string f
    | Load(_, ty) ->
        sprintf "Load(<llvalue>, %a)" Type.to_string ty
    | Store(_, f) ->
        sprintf "Store(<llvalue>, %a)" to_string f
    | AddressOf f ->
        sprintf "AddressOf(%a)" to_string f
    | Free f ->
        sprintf "Free(%a)" to_string f
    | Llvalue(_, ty) ->
        sprintf "Llvalue(<llvalue>, %a)" Type.to_string ty
    | Magic(f, ty) ->
        sprintf "Magic(%a, %a)" to_string f Type.to_string ty
    | GetMark f ->
        sprintf "GetMark(%a)" to_string f
    | SetMark(f, n) ->
        sprintf "SetMark(%a, %d)" to_string f n
    | Time -> "Time"
    | CreateThread(f, x) ->
        sprintf "CreateThread(%a, %a)" to_string f to_string x
    | JoinThread f ->
        sprintf "JoinThread(%a)" to_string f
    | CreateMutex -> "CreateMutex"
    | UnsafeLockMutex f ->
        sprintf "UnsafeLockMutex(%a)" to_string f
    | UnlockMutex f ->
        sprintf "UnlockMutex(%a)" to_string f
    | ExtGetThreadLocal -> "ExtGetThreadLocal"
    | ExtSetThreadLocal f ->
        sprintf "ExtSetThreadLocal(%a)" to_string f
    | GetThreadLocal -> "GetThreadLocal"
    | SetThreadLocal f ->
        sprintf "SetThreadLocal(%a)" to_string f
    | GetThreadTick -> "GetThreadTick"
    | SetThreadTick f ->
        sprintf "SetThreadTick(%a)" to_string f
    | Unsafe f ->
        sprintf "Unsafe(%a)" to_string f
  and to_strings sep = list_to_string to_string sep
  
  (** Apply the given rule to the next level of subexpressions. *)
  let rewrite r = function
    | Struct es -> Struct(List.map r es)
    | GetValue(e, i) -> GetValue(r e, i)
    | UnArith(op, f) -> UnArith(op, r f)
    | BinArith(op, f, g) -> BinArith(op, r f, r g)
    | Cmp(op, f, g) -> Cmp(op, r f, r g)
    | If(p, t, f) -> If(r p, r t, r f)
    | Let(x, body, rest) -> Let(x, r body, r rest)
    | Alloc(n, x) -> Alloc(r n, r x)
    | Length e -> Length(r e)
    | Get(a, i) -> Get(r a, r i)
    | Set(a, i, x) -> Set(r a, r i, r x)
    | Apply(f, xs) -> Apply(r f, List.map r xs)
    | Printf(s, xs) -> Printf(s, List.map r xs)
    | IntOfInt(ty, x) -> IntOfInt(ty, r x)
    | IntOfFloat(ty, x) -> IntOfFloat(ty, r x)
    | FloatOfInt(ty, x) -> FloatOfInt(ty, r x)
    | Construct(c, v) -> Construct(c, r v)
    | IsType(v, c) -> IsType(r v, c)
    | Print e -> Print(r e)
    | Exit x -> Exit(r x)
    | AddressOf x -> AddressOf(r x)
    | Cast(v, c) -> Cast(r v, c)
    | Free x -> Free(r x)
    | Return(x, ty) -> Return(r x, ty)
    | Visit x -> Visit(r x)
    | Magic(v, ty) -> Magic(r v, ty)
    | GetMark v -> GetMark(r v)
    | SetMark(v, n) -> SetMark(r v, n)
    | CreateThread(f, x) -> CreateThread(r f, r x)
    | JoinThread f -> JoinThread(r f)
    | UnsafeLockMutex f -> UnsafeLockMutex(r f)
    | UnlockMutex f -> UnlockMutex(r f)
    | ExtSetThreadLocal f -> ExtSetThreadLocal(r f)
    | SetThreadLocal f -> SetThreadLocal(r f)
    | SetThreadTick f -> SetThreadTick(r f)
    | Unsafe f -> Unsafe(r f)
    
    | Null
    | Unit
    | Bool _
    | Byte _
    | Int _
    | Int64 _
    | Float _
    | Var _
    | Load _
    | Store _
    | Llvalue _
    | Time
    | CreateMutex
    | ExtGetThreadLocal
    | GetThreadLocal
    | GetThreadTick as e -> e

  let rec apply_tail r = function
    | If(p, t, f) -> If(p, apply_tail r t, apply_tail r f)
    | Let(x, body, rest) -> Let(x, body, apply_tail r rest)
    | f -> r f

  let count f =
    let n = ref 0 in
    let rec loop f =
      incr n;
      rewrite loop f in
    ignore(loop f);
    !n
  
  let rec unroll_rule f params body = function
    | Let(x, body, rest) as e when x=f ->
	(* If this function's name is shadowed by a "let"-binding then stop
	   rewriting subexpressions. *)
	e
    | Apply(Var g, args) when f=g ->
	(* Replace recursive calls with a "let"-binding of the arguments and
	   a copy of the function body. *)
        let rec aux (i, t) param =
          i+1, Let(param, GetValue(Var " args", i), t) in
        let _, body = List.fold_left aux (0, body) params in
        Let(" args", Struct args, body)
    | e -> rewrite (unroll_rule f params body) e
  
  let rec unroll f args body body' =
    let body'' = rewrite (unroll_rule f (List.map fst args) body) body' in
    if count body'' > 4000 then body' else body''

  let rec nest n f x =
    if n=0 then x else nest (n-1) f (f x)

  let unroll f args body =
    nest Options.unroll (unroll f args body) body

  let dPrintf(string, args) =
    if !Options.debug then
      Printf(string, args)
    else
      Unit

  let d2Printf(s, xs) = Printf(s, xs)
  let d2Printf _ = Unit

  let lock(mutex, body) =
    Let("mutex", mutex,
	compound
	  [ d2Printf("%p locking %p\n", [AddressOf GetThreadLocal; mutex]);
	    UnsafeLockMutex(Var "mutex");
	    Let("result", body,
		compound
		  [ d2Printf("%p unlocking %p\n",
			     [AddressOf GetThreadLocal; mutex]);
		    UnlockMutex(Var "mutex");
		    Var "result" ])])

  let rec trace_rule fn = function
    | If(p, t, f) -> If(p, trace_rule fn t, trace_rule fn f)
    | Let(x, f, g) -> Let(x, f, trace_rule fn g)
    | Apply(f, gs) ->
	let rec loop i xs = function
	  | [] ->
	      compound
		[ Printf("Tail call from "^fn^" to ", []);
		  Apply(Var "trace", List.rev xs) ]
	  | g::gs ->
	      let x = sprintf "arg%d" i in
	      Let(x, g, loop (i+1) (Var x::xs) gs) in
	Let("trace", f, loop 0 [] gs)
    | f ->
	Let("trace", f,
	    compound
	      [ Printf(fn^" return ", []);
		Print(Var "trace");
		Printf("\n", []);
		Var "trace" ])

  (** Insert debug information at the beginning of each function. *)
  let rec trace(f, args, ty_ret, body) =
    let body = trace_rule f body in
    printf "%s %s\n" f (to_string () body);
    (f, args, ty_ret,
     compound
       [Printf(f, []);
	Print(Struct(List.map (fun (x, _) -> Var x) args));
	Printf("\n", []);
	body ])

  let is_leaf f =
    let rec r = function
      | Apply _ -> raise Pervasives.Exit
      | f -> rewrite r f in
    try ignore(r f); true with Pervasives.Exit -> false
end

open Expr

(** Definitions related to the list type. *)
module List = struct
  include List

  let iteri f xs =
    ignore(fold_left (fun i x -> (f i x: unit); i+1) 0 xs)

  let mapi f xs =
    let _, ys = fold_left (fun (i, ys) x -> i+1, f i x::ys) (0, []) xs in
    List.rev ys

  let init n f =
    Array.to_list(Array.init n f)

  let rec between sep xs =
    match xs with
    | [] -> []
    | [x] -> [x]
    | x::xs -> x::sep::between sep xs
end

(** Binding to a function that enables TCO in LLVM. *)
external enable_tail_call_opt : unit -> unit = "llvm_enable_tail_call_opt"

let () = enable_tail_call_opt()

let llcontext = global_context()

let void_type = void_type llcontext

let struct_type = struct_type llcontext

let i1_type = i1_type llcontext

let i8_type = i8_type llcontext

let i32_type = i32_type llcontext

let i64_type = i64_type llcontext

let single_type = float_type llcontext

let double_type = double_type llcontext

let float_type = double_type

(** Create an aggregate register (a struct) containing the given llvalues. *)
let mk_struct state vs =
  let llty = struct_type (Array.of_list(List.map type_of vs)) in
  let aux (i, s) x = i+1, build_insertvalue s x i "" state#bb in
  snd(List.fold_left aux (0, undef llty) vs)

let extractvalue state s i =
  build_extractvalue s i "" state#bb

(** Type of a C-compatible null-terminated string. *)
let string_type = pointer_type i8_type

(** Type of a native int. *)
let int_type = match Sys.word_size with
  | 32 -> i32_type
  | 64 -> i64_type
  | _ -> failwith "Unknown word size"

(** Is the given type represented by a struct. *)
let is_struct = function
  | `Array _ | `Struct _ | `Reference -> true
  | `Unit | `Bool | `Byte | `Int | `Int64 | `Float | `Function _ -> false

(** Is the given type a reference type. *)
let is_ref_type = function
  | `Array _ | `Reference -> true
  | `Struct _ | `Unit | `Bool | `Byte | `Int | `Int64 | `Float
  | `Function _ -> false

(** Layout of a reference type. *)
module Ref = struct
  (** Run-time representation of values of reference types as an LLVM type. *)
  let lltype =
    struct_type[|string_type; int_type; string_type; string_type|]

  (** Index of the field containing the pointer to the run-time type. *)
  let llty = 0

  (** Index of the field containing the int metadata. *)
  let tag = 1

  (** Index of the field containing the pointer to allocated data. *)
  let data = 2

  (** Index of the field containing the pointer to the mark bit. *)
  let mark = 3

  (** Construct a reference type with the given run-time type, metadata and
      allocated data. *)
  let mk state llty tag data mark =
    mk_struct state [ state#bitcast llty string_type;
		      tag;
		      state#bitcast data string_type;
		      mark ]
end

(** Convert a type from our type system into LLVM's type system. *)
let rec lltype_of : Type.t -> lltype = function
  | `Unit -> int_type
  | `Bool -> i1_type
  | `Byte -> i8_type
  | `Int -> int_type
  | `Int64 -> i64_type
  | `Float -> float_type
  | `Struct tys -> struct_type_of tys
  | `Function ty -> pointer_type(function_type_of ty)
  | `Array _ | `Reference -> Ref.lltype

(** Representation of function pointers. *)
and function_type_of ?(pass_tl=true) = function
  | ty_args, ty_ret when is_struct ty_ret ->
      let args =
	pointer_type(lltype_of ty_ret) :: List.map lltype_of ty_args in
      let args = if pass_tl then string_type :: args else args in
      function_type (lltype_of `Unit) (Array.of_list args)
  | ty_args, ty_ret ->
      let args = List.map lltype_of ty_args in
      let args = if pass_tl then string_type :: args else args in
      function_type (lltype_of ty_ret) (Array.of_list args)

(** Representation of structs. *)
and struct_type_of tys =
  struct_type (Array.of_list (List.map lltype_of tys))

(** Run-time types. *)
module RTType = struct
  let ty_visit = `Function([`Reference], `Unit)
  let ty_print = `Function([`Reference], `Unit)

  (** The lltype of our run-time types. *)
  let lltype =
    lltype_of
      (`Struct[ `Function([`Reference], `Unit);
		`Function([`Reference], `Unit) ])

  let visit = 0
  let print = 1
end

let print_type_of v =
  printf "%s\n%!" (string_of_lltype(type_of v))

(** Create an LLVM native int. *)
let int n = const_int int_type n

(** LLVM value used to represent the value () of the type unit. *)
let unit = undef int_type

(** Create an LLVM 8-bit int. *)
let int8 n = const_int i8_type n

(** Create an LLVM 32-bit int. *)
let int32 n = const_int i32_type n

(** Create an LLVM 64-bit int. *)
(* FIXME: We should use const_int_of_string to allow the full range of
   64-bit int literals. *)
let int64 n = const_int i64_type (Int64.to_int n)

(** Create an LLVM 64-bit float. *)
let float64 x = const_float (lltype_of `Float) x

(** LLVM representation of the NULL pointer. *)
let null = const_null string_type

(** Create a default value of the given type. *)
let rec null_of = function
  | `Unit -> Unit
  | `Bool -> Bool false
  | `Byte -> Byte 0
  | `Int -> Int 0
  | `Int64 -> Int64 0L
  | `Float -> Float 0.0
  | `Struct tys -> Struct(List.map null_of tys)
  | `Array ty -> Alloc(Int 0, null_of ty)
  | `Function(_, _) as ty -> Llvalue(null, ty)
  | `Reference -> Null

(** Search for a binding and give a comprehensible error if it is not found. *)
let find k kvs =
  try List.assoc k kvs with Not_found ->
    eprintf "Unknown '%s'\n%!" k;
    raise Not_found



(** Global LLVM module. *)
let m = create_module llcontext "toplevel"

(** Global LLVM module provider. *)
(* let mp = ModuleProvider.create m *)

(** Global LLVM execution engine. *)
let ee =
  ignore(Llvm_executionengine.initialize_native_target());
  ExecutionEngine.create_jit m 0

(** Type used to represent stacks and unordered sequences (bags). *)
(* FIXME: Sequences should resize themselves when more space is required
   and free old versions if necessary. *)
module Seq = struct
  (** Type of a bag (unsorted collection). *)
  let ty ty = `Struct[`Int; `Array ty]

  (** Construct an empty sequence. *)
  let empty x = Struct[Int 0; Alloc(Int Options.max_depth, x)]

  (** Extract internal array from sequence. *)
  let arr seq = GetValue(seq, 1)

  (** Fetch element at index in a sequence. *)
  let get(seq, i) = Get(arr seq, i)

  (** Set element at index in a sequence. *)
  let set(seq, i, x) = Set(arr seq, i, x)

  (** Deallocate a sequence. *)
  let free seq = Free(arr seq)

  (** Number of elements in a sequence. *)
  let count seq = GetValue(seq, 0)

  (** Append an element to a sequence. *)
  let push(seq, x) =
    compound
      [ set(seq, count seq, x);
	Struct[count seq +: Int 1; arr seq] ]

  let remove_at(seq, i) =
    compound
      [ set(seq, i, get(seq, count seq -: Int 1));
	Struct[count seq -: Int 1; arr seq] ]
end

(** Run/suspend state used for individial threads and global objective to
    cooperatively synchronize for the stop-the-world GC phase. *)
module State = struct
  let run, suspend = 0, 1
end

(** Representation of thread-local data. *)
module ThreadLocal = struct
  let internal_ty = `Struct[`Int; `Int; `Array `Int; Seq.ty `Reference]

  (** Type of thread-local state. *)
  let ty = `Array internal_ty

  (** Allocate new thread local state. *)
  let make =
    let f s b k =
      compound
	[ dPrintf(s^"\n", []);
	  Let(s, b, k) ] in
    compound
      [ dPrintf("ThreadLocal.make\n", []);
	f "mutex" CreateMutex
	  (f "state" (Alloc(Int 1, Int State.run))
	     (f "stack" (Seq.empty Null)
		(f "thread_local" (Alloc(Int 1, Struct[Int 0; Var "mutex"; Var "state"; Var "stack"]))
		(compound [dPrintf("ThreadLocal.make ends\n", []);
			   Var "thread_local"])))) ]

  let time_of tl = GetValue(tl, 0)

  let mutex_of tl = GetValue(tl, 1)

  let state_of tl = GetValue(tl, 2)

  let stack_of tl = GetValue(tl, 3)

  let free tl =
    Let("tl", Get(tl, Int 0),
	compound
	  [ dPrintf("Freeing thread-local data at %p\n", [AddressOf tl]);
	    Free(state_of(Var "tl"));
	    Seq.free(stack_of(Var "tl"));
	    Free tl ])

  let load_state tl =
    Let("tl", Get(tl, Int 0),
	compound [ dPrintf("%p ThreadLocal.load_state %p\n",
			   [AddressOf GetThreadLocal; mutex_of(Var "tl")]);
		   lock(mutex_of(Var "tl"),
			Get(state_of(Var "tl"), Int 0)) ])
	  
  let store_state tl n =
    Let("tl", Get(tl, Int 0),
	compound [ dPrintf("%p ThreadLocal.store_state %p\n",
			   [AddressOf GetThreadLocal; mutex_of(Var "tl")]);
		   lock(mutex_of(Var "tl"),
			Set(state_of(Var "tl"), Int 0, Int n)) ])

  let eq x y = AddressOf x =: AddressOf y
end

(** Global thread data including the thread-local data of every mutator
    thread. *)
module ThreadGlobal = struct
  let mutex = define_global "threadglobal_mutex" (int 0) m

  let ty = Seq.ty ThreadLocal.ty

  let list = define_global "threadglobal_list" (const_null(lltype_of ty)) m

  let lock f =
    compound [ dPrintf("%p ThreadGlobal.lock %p\n",
		       [AddressOf GetThreadLocal; Load(mutex, `Int)]);
	       lock(Load(mutex, `Int), f) ]

  let load_list = Load(list, ty)

  let store_list xs = Store(list, xs)

  let state = define_global "threadglobal_state" (int 0) m

  let load_state = Load(state, `Int)

  let store_state n = Store(state, Int n)

  let init =
    compound
      [ dPrintf("Creating global thread mutex...\n", []);
	Store(mutex, CreateMutex) ]
end

let enter_blocking_section() =
  if !Options.gc_enabled then
    Unsafe
      (compound
	 [ dPrintf("%p entering blocking section with %d roots\n",
		   [AddressOf GetThreadLocal;
		    Seq.count(ThreadLocal.stack_of
				(Get(GetThreadLocal, Int 0)))]);
	   ThreadGlobal.lock
	     (ThreadLocal.store_state GetThreadLocal State.suspend) ])
  else
    Unit

let leave_blocking_section() =
  if !Options.gc_enabled then
    Apply(Var "spin", [])
  else
    Unit

let lockMutex f =
  Let("mutex", f,
      compound [ enter_blocking_section();
		 UnsafeLockMutex(Var "mutex");
		 leave_blocking_section() ])

(** The visit stack is an array of unvisited reference types. *)
let visit_stack =
  define_global "visit_stack" (const_null(lltype_of(`Array `Reference))) m

(** Number of unvisited references on the visit stack. *)
let n_visit = define_global "n_visit" (int 0) m

let allocated_mutex = define_global "allocated_mutex" (int 0) m

(** The allocated list is an array of reference types. *)
let allocated =
  define_global "allocated" (const_null (lltype_of(`Array `Reference))) m

(** Number of allocated references. *)
let n_allocated = define_global "n_allocated" (int 0) m

(** Number of allocations required to incur a garbage collection. *)
let quota = define_global "quota" (int 0) m

module Extern = struct
  (** LLVM declaration of C's putchar function. *)
  let putchar =
    declare_function "putchar" (function_type int_type [|int_type|]) m
      
  (** LLVM declaration of C's exit function. *)
  let exit =
    declare_function "exit" (function_type void_type [|int_type|]) m
      
  (** LLVM declaration of C's printf function. *)
  let printf =
    declare_function "printf"
      (var_arg_function_type int_type [|string_type|]) m
      
  (** LLVM declaration of libdl's dlopen function. *)
  let dlopen =
    declare_function "dlopen"
      (function_type string_type [|string_type; int_type|]) m
      
  (** LLVM declaration of libdl's dlsym function. *)
  let dlsym =
    declare_function "dlsym"
      (function_type string_type [|string_type; string_type|]) m
      
  let dlfn f ty_ret ty_args =
    let ty = pointer_type(function_type ty_ret (Array.of_list ty_args)) in
    let ptr = define_global f (const_null ty) m in
  object
    method ty = ty
    method ptr = ptr
  end
  
  let alloc = dlfn "hlvm_alloc" string_type [int_type; int_type]
  
  let free = dlfn "hlvm_free" void_type [string_type]

  let time = dlfn "hlvm_time" float_type []

  let init = dlfn "hlvm_init" void_type []

  let create_thread =
    dlfn "hlvm_create_thread" string_type [string_type; int_type]

  let join_thread = dlfn "hlvm_join_thread" void_type [string_type]

  let create_mutex = dlfn "hlvm_create_mutex" int_type []

  let lock_mutex = dlfn "hlvm_lock_mutex" void_type [int_type]

  let unlock_mutex = dlfn "hlvm_unlock_mutex" void_type [int_type]

  let get_thread_local = dlfn "hlvm_get_thread_local" int_type []

  let set_thread_local = dlfn "hlvm_set_thread_local" void_type [int_type]

  (** Initialize all of the dynamically-loaded functions. *)
  let load_fns load_fn =
    load_fn alloc "hlvm_alloc";
    load_fn free "hlvm_free";
    load_fn time "hlvm_time";
    load_fn init "hlvm_init";
    load_fn create_thread "hlvm_create_thread";
    load_fn join_thread "hlvm_join_thread";
    load_fn create_mutex "hlvm_create_mutex";
    load_fn lock_mutex "hlvm_lock_mutex";
    load_fn unlock_mutex "hlvm_unlock_mutex";
    load_fn get_thread_local "hlvm_get_thread_local";
    load_fn set_thread_local "hlvm_set_thread_local"
end

(** LLVM global to store the total time spent in the suspend phase. *)
let suspend_time = define_global "suspend_time" (const_float float_type 0.0) m

(** LLVM global to store the total time spent in the mark phase. *)
let mark_time = define_global "mark_time" (const_float float_type 0.0) m

(** LLVM global to store the total time spent in the sweep phase. *)
let sweep_time = define_global "sweep_time" (const_float float_type 0.0) m

(** Default calling convention used by HLVM. *)
let cc = CallConv.fast

(** Mapping from bound variable names to their LLVM values and HLVM types. *)
type vars =
    { vals: (string * (llvalue * Type.t)) list }

(** Default variable bindings. *)
let vars = { vals = [] }

(** Bound types (including internal types such as wrapper reference
    types for arrays). *)
let types = Hashtbl.create 1

(** Container of internal functions such as visitors to traverse the heap. *)
let functions = Hashtbl.create 1

(** Search for a type with the given name from the  *)
let find_type name =
  try Hashtbl.find types name with Not_found as e ->
    eprintf "Type '%s' not found\n%!" name;
    raise e

(** Bind a new variable. *)
let add_val x vars = { vals = x :: vars.vals }

(** Push a reference type onto the shadow stack. *)
let push self stack depth v =
  if !Options.shadow_stack_enabled then begin
    if !Options.debug then
      printf "state#push\n%!";
    let d = self#load depth [int 0] in
    let data = extractvalue self (self#load stack [int 0]) Ref.data in
    let data = self#bitcast data (pointer_type(type_of v)) in
    self#store data [d] v;
    self#store depth [int 0] (build_add (int 1) d "" self#bb)
  end

(** Get pointers to the stack depth and stack. *)
(* FIXME: This hack should be replaced with term rewriting. *)
let get_stack state =
  assert(state#gc_enabled);
  let data =
    state#bitcast state#thread_local
      (pointer_type(lltype_of ThreadLocal.internal_ty)) in
  state#gep data [int 0; int32 3; int32 0],
  state#gep data [int 0; int32 3; int32 1]

(** Create a state object that encapsulates our interface for emitting LLVM
    instructions. *)
class state pass_tl func = object (self : 'self)
  val blk = entry_block func
  val odepth = lazy(int 0)
  val gc_enabled = true
  val roots = false
  val thread_local = if pass_tl then `Internal else `External

  (** Get the current LLVM instruction block. *)
  method blk = blk

  (** Get an LLVM instruction builder to insert instructions at the end of the
      current LLVM instruction block. *)
  method bb = builder_at_end llcontext blk

  (** Issue an LLVM get element pointer instruction. *)
  method gep a ns = build_gep a (Array.of_list ns) "" self#bb

  (** Issue an LLVM load instruction. *)
  method load a ns = build_load (self#gep a ns) "" self#bb

  (** Issue an LLVM store instruction. *)
  method store a ns x = ignore(build_store x (self#gep a ns) self#bb)

  (** Issue LLVM instructions to call the hlvm_alloc function. *)
  method malloc llty n =
    let size = build_trunc (size_of llty) int_type "" self#bb in
    let llalloc = self#load Extern.alloc#ptr [int 0] in
    let data = build_call llalloc [|n; size|] "" self#bb in
    self#bitcast data (pointer_type llty)

  (** Issue LLVM instructions to call the hlvm_free function. *)
  method free x =
    let llfree = self#load Extern.free#ptr [int 0] in
    ignore(build_call llfree [|x|] "" self#bb)

  (** Define a global LLVM variable. *)
  method define_global x v = define_global x v m

  (** Create a new instruction block and return a new state that will insert
      instructions into it. *)
  method mk s = ({< blk = append_block llcontext s func >} : 'self)

  (** Issue an LLVM return instruction. *)
  method ret v = ignore(build_ret v self#bb)

  (** Issue an LLVM unconditional branch instruction. *)
  method br (s: 'self) = ignore(build_br s#blk self#bb)

  (** Issue an LLVM bitcast instruction. *)
  method bitcast v ty = build_bitcast v ty "" self#bb

  (** Issue an LLVM call instruction using the given calling convention. *)
  method call cc f args =
    if !Options.debug then begin
      printf "call ";
      List.iter (fun arg -> printf "%s " (string_of_lltype(type_of arg)))
	args;
      printf "\n%!"
    end;
    let call = build_call f (Array.of_list args) "" self#bb in
    set_instruction_call_conv cc call;
    call

  (** Get the LLVM value of the pointer to the return struct. *)
  method sret = param func (if pass_tl then 1 else 0)

  (** Issue an LLVM alloca instruction to allocate on the stack. *)
  method alloca ty =
    build_alloca ty "" (builder_at_end llcontext (entry_block func))

  (** Are we emitting code to keep the GC informed. *)
  method gc_enabled = gc_enabled

  (** Push the given value of a reference type onto the shadow stack. *)
  method gc_root v =
    if gc_enabled && !Options.shadow_stack_enabled then
      let stack_depth, stack = get_stack self in
      push self stack stack_depth v;
      {< roots = true >}
    else
      self

  (** Restore the shadow stack depth to the value it was when this function
      was entered. *)
  method gc_restore() =
    if gc_enabled && roots then
      if !Options.shadow_stack_enabled then begin
	if !Options.debug then
	  printf "state#restore\n%!";
	let stack_depth, _ = get_stack self in
	self#store stack_depth [int 0] self#odepth
      end

  (** Return a "state" object that will not inject instructions to keep the
      GC informed if false. *)
  method gc enabled = {< gc_enabled = enabled >}

  (** Depth the shadow stack was at when this function was entered. *)
  method odepth = Lazy.force odepth

  (** Prepare to reset the shadow stack depth to this value. *)
  method set_depth d = {< odepth = d >}

  (** Record the thread-local data. *)
  method set_thread_local thread_local =
    {< thread_local = `Custom thread_local >}

  (** Get the thread local data. *)
  method thread_local =
    match thread_local with
    | `Custom tl -> tl
    | `Internal -> param func 0
    | `External ->
	let get_thread_local =
	  self#load Extern.get_thread_local#ptr [int 0] in
	let ptr = self#call CallConv.c get_thread_local [] in
	self#ptr_of_int ptr string_type

  (** Issue an LLVM ptrtoint instruction. *)
  method int_of_ptr ptr = build_ptrtoint ptr int_type "" self#bb

  (** Issue an LLVM inttoptr instruction. *)
  method ptr_of_int n ty = build_inttoptr n ty "" self#bb

  (** Issue LLVM instructions to call the hlvm_time function. *)
  method time =
    let lltime = self#load Extern.time#ptr [int 0] in
    build_call lltime [||] "" self#bb
end

(** Create a state object and save the current shadow stack depth. *)
let mk_state ?(pass_tl=true) func =
  let state = new state pass_tl func in
  let depth state =
    if state#gc_enabled && !Options.shadow_stack_enabled then
      let stack_depth, _ = get_stack state in
      state#load stack_depth [int 0]
    else
      int 0 in
  state#set_depth(lazy(depth state))

(** Create a unique string based upon the given string. *)
let uniq =
  let m = Hashtbl.create 1 in
  let rec aux s =
    try
      Hashtbl.find m s;
      aux(s ^ "'")
    with Not_found ->
      Hashtbl.add m s ();
      s in
  aux

(** Exception raised after the return expression is compiled. *)
exception Returned

(** Top-level definitions. *)
type t =
    [ `UnsafeFunction of string * (string * Type.t) list * Type.t * Expr.t
    | `Function of string * (string * Type.t) list * Type.t * Expr.t
    | `Expr of Expr.t
    | `Extern of string * Type.t list * Type.t
    | `Type of string * Type.t ]

(** Helper function for type checking. *)
let type_check err ty1 ty2 =
  if not(Type.eq ty1 ty2) then
    invalid_arg
      (sprintf "%s: %a != %a" err Type.to_string ty1 Type.to_string ty2)

(** Constant string literals are memoized here. *)
let string_cache = Hashtbl.create 1

(** Memoize a string. *)
let mk_string string =
  try Hashtbl.find string_cache string with Not_found ->
    let spec = define_global "buf" (const_stringz llcontext string) m in
    Hashtbl.add string_cache string spec;
    spec

(** List of functions that have been evaluated. The "main" function generated
    for standalone computation calls each of these functions in turn. *)
let eval_functions = ref []

(** Register a function and execute it. *)
let run_function llf =
  eval_functions := !eval_functions @ [llf];
  (* We pass a single dummy argument because the current OCaml bindings in
     LLVM are broken if no arguments are passed (they call malloc(0)). *)
  if not !Options.compile_only then
    ignore
      (ExecutionEngine.run_function llf [|GenericValue.of_int int_type 0|] ee)

(* Push a reference onto the visit stack. *)
let gc_push p =
  Let("p", p,
      If(AddressOf(Var "p") =: Int 0, Unit,
	 Let("a", Load(visit_stack, `Array `Reference),
	     Let("n", Load(n_visit, `Int),
		 compound
		   [ Set(Var "a", Var "n", Var "p");
		     Store(n_visit, Var "n" +: Int 1) ]))))

let gc_check() =
  if !Options.gc_enabled then
    Let("tick", GetThreadTick,
	If(Var "tick" <: Int Options.ticks,
	   SetThreadTick(Int 1 +: Var "tick"),
	   Apply(Var "gc_check", [])))
  else
    Unit

let llty_null = ref None

(** Compile an expression in the context of current vars into the given
    LLVM state. *)
let rec expr vars (state: state) e =
  if !Options.debug then
    printf "-> expr %s\n%!" (Expr.to_string () e);
  let state, (x, ty_x) as ret =
    try expr_aux vars state e with
    | Returned as exn -> raise exn
    | exn ->
	printf "ERROR: %s\n%!" (Expr.to_string () e);
	raise exn in
  if !Options.debug then
    printf "<- %s\n%!" (string_of_lltype(type_of x));
  ret
and expr_aux vars state = function
  | Return(Unsafe f, ty) -> expr vars state (Unsafe(Return(f, ty)))
  | Unsafe f ->
      let enabled = state#gc_enabled in
      let state, result = expr vars (state#gc false) f in
      state#gc enabled, result
  | Null ->
      let llty =
	match !llty_null with
	| Some llty -> llty
	| None ->
	    let name = "Null" and ty = `Unit in
	    let llty = define_global name (undef RTType.lltype) m in
	    Hashtbl.add types name (llty, ty);
	    let llvisit = def_visit vars name name ty in
	    let llprint = def_print vars name name ty in
	    init_type name llty llvisit llprint;
	    llty_null := Some llty;
	    llty in
      state, (Ref.mk state llty (int 0) null null, `Reference)
  | Unit -> state, (unit, `Unit)
  | Bool b -> state, (const_int i1_type (if b then 1 else 0), `Bool)
  | Byte n -> state, (int8 n, `Byte)
  | Int n -> state, (int n, `Int)
  | Int64 n -> state, (int64 n, `Int64)
  | Float x -> state, (float64 x, `Float)
  | Struct fs ->
      let state, (fs, tys_f) = exprs vars state fs in
      state, (mk_struct state fs, `Struct tys_f)
  | GetValue(s, i) ->
      let state, (s, ty_s) = expr vars state s in
      begin
	match ty_s with
	| `Struct tys ->
	    let v = extractvalue state s i in
	    state, (v, List.nth tys i)
	| ty -> invalid_arg(sprintf "GetValue of %a" Type.to_string ty)
      end
  | Construct(f, x) ->
      let llty, ty = find_type f in
      let state, (x, ty_x) = expr vars state x in
      type_check "Type constructor argument of wrong type" ty ty_x;
      let state, s =
	match ty_x with
	| `Unit -> state, Ref.mk state llty (int 0) null null
	| _ ->
	    let px = state#malloc (lltype_of ty_x) (int 1) in
	    state#store px [int 0] x;
	    let mark = state#malloc i8_type (int 1) in
	    state#store mark [int 0] (int8 0);
	    let s = Ref.mk state llty (int 0) px mark in
	    let state = gc_root vars state (lazy s) `Reference in
	    let state = gc_alloc vars state s in
	    state, s in
      state, (s, `Reference)
  | IsType(f, ty_name) ->
      let state, (f, ty_f) = expr vars state f in
      type_check "IsType of non-reference type" ty_f `Reference;
      let llty_f = extractvalue state f Ref.llty in
      let llty_f = state#bitcast llty_f (pointer_type RTType.lltype) in
      let llty, ty = find_type ty_name in
      state, (build_icmp Icmp.Eq llty_f llty "" state#bb, `Bool)
  | Cast(f, ty_name) ->
      (* FIXME: This is unsafe. *)
      let state, (f, ty_f) = expr vars state f in
      type_check "Cast of non-reference type" ty_f `Reference;
      let llty, ty = find_type ty_name in
      if ty = `Unit then state, (unit, `Unit) else
	let v = extractvalue state f Ref.data in
	let v = state#bitcast v (pointer_type(lltype_of ty)) in
	let v = state#load v [int 0] in
	let state = gc_root vars state (lazy v) ty in
	state, (v, ty)
  | Var x ->
      let x, ty_x = find x vars.vals in
      state, (x, ty_x)
  | UnArith(`Neg, f) ->
      let state, (f, f_ty) = expr vars state f in
      let build =
	match f_ty with
	| `Byte | `Int | `Int64 -> build_neg
	| `Float -> build_fneg
	| _ -> invalid_arg "expr.unarith" in
      state, (build f "" state#bb, f_ty)
  | BinArith(op, f, g) ->
      let state, (f, f_ty), (g, g_ty) = expr2 vars state f g in
      let build =
	match op, (f_ty, g_ty) with
	| `Add, (`Byte, `Byte | `Int, `Int | `Int64, `Int64) -> build_add
	| `Sub, (`Byte, `Byte | `Int, `Int | `Int64, `Int64) -> build_sub
	| `Mul, (`Byte, `Byte | `Int, `Int | `Int64, `Int64) -> build_mul
	| `Add, (`Float, `Float) -> build_fadd
	| `Sub, (`Float, `Float) -> build_fsub
	| `Mul, (`Float, `Float) -> build_fmul
	| `Div, (`Byte, `Byte | `Int, `Int | `Int64, `Int64) -> build_sdiv
	| `Mod, (`Byte, `Byte | `Int, `Int | `Int64, `Int64) -> build_srem
	| `Div, (`Float, `Float) -> build_fdiv
	| _ -> invalid_arg "expr.arith" in
      state, (build f g "" state#bb, f_ty)
  | Cmp(op, f, g) ->
      let state, (f, f_ty), (g, g_ty) = expr2 vars state f g in
      let build =
	match f_ty, g_ty with
	| `Byte, `Byte | `Int, `Int | `Int64, `Int64 ->
	  let op = match op with
	  | `Lt -> Icmp.Slt
	  | `Le -> Icmp.Sle
	  | `Eq -> Icmp.Eq
	  | `Ne -> Icmp.Ne
	  | `Ge -> Icmp.Sge
	  | `Gt -> Icmp.Sgt in
	  build_icmp op
	| `Float, `Float ->
	  let op = match op with
	  | `Lt -> Fcmp.Olt
	  | `Le -> Fcmp.Ole
	  | `Eq -> Fcmp.Oeq
	  | `Ne -> Fcmp.One
	  | `Ge -> Fcmp.Oge
	  | `Gt -> Fcmp.Ogt in
	  build_fcmp op
	| _ -> invalid_arg "expr.cmp" in
      state, (build f g "" state#bb, `Bool)
  | Return(If(p, t, f), ty_ret) ->
      (* Tail expressions in both branches. *)
      let state, (p, ty_p) = expr vars state p in
      type_check "Predicate of non-bool type" ty_p `Bool;
      let t_state, f_state = state#mk "pass", state#mk "fail" in
      let _ = build_cond_br p t_state#blk f_state#blk state#bb in
      return vars t_state t ty_ret;
      return vars f_state f ty_ret;
      raise Returned
  | If(p, t, f) ->
      let state, (p, ty_p) = expr vars state p in
      type_check "Predicate of non-bool type" ty_p `Bool;
      let t_state, f_state = state#mk "pass", state#mk "fail" in
      let _ = build_cond_br p t_state#blk f_state#blk state#bb in
      let k_state = state#mk "cont" in
      let t_state, (t, ty_t) = expr vars t_state t in
      t_state#br k_state;
      let f_state, (f, ty_f) = expr vars f_state f in
      f_state#br k_state;
      type_check "If" ty_t ty_f;
      if ty_t = `Unit then k_state, (unit, `Unit) else
	let v = build_phi [t, t_state#blk; f, f_state#blk] "" k_state#bb in
	k_state, (v, ty_t)
  | Return(Let(x, f, g), ty_ret) ->
      (* Tail expression in "rest". *)
      expr vars state (Let(x, f, Return(g, ty_ret)))
  | Let(x, f, g) ->
      let state, (f, ty_f) = expr vars state f in
      let state, (g, ty_g) = expr (add_val (x, (f, ty_f)) vars) state g in
      state, (g, ty_g)
  | Alloc(n, x) ->
      let state, (n, ty_n), (x, ty_x) = expr2 vars state n x in
      type_check "Alloc with non-int length" ty_n `Int;
      let data = state#malloc (lltype_of ty_x) n in
      let state, (mark_size, _) =
	expr vars state (If(Llvalue(n, `Int) =: Int 0, Int 0, Int 1)) in
      let mark = state#malloc i8_type mark_size in
      let a = Ref.mk state (mk_array_type ty_x) n data mark in
      let ty_a = `Array ty_x in
      let fill = fill vars ty_x in
      let state, _ =
	expr vars state
	  (Let("a", Llvalue(a, `Array ty_x),
	       compound
		 [ Apply(fill, [ Var "a";
				 Llvalue(x, ty_x);
				 Int 0;
				 Llvalue(n, `Int) ]);
		   If(AddressOf(Var "a") =: Int 0, Unit,
		      SetMark(Var "a", 0)) ])) in
      let state = gc_root vars state (lazy a) ty_a in
      let state = gc_alloc vars state a in
      state, (a, ty_a)
  | Length a ->
      let state, (a, ty_a) = expr vars state a in
      (match ty_a with `Array _ -> ()
       | _ -> invalid_arg "Length of non-array");
      state, (extractvalue state a Ref.tag, `Int)
  | Get(a, i) ->
      let state, (a, ty_a), (i, ty_i) = expr2 vars state a i in
      let ty_elt = match ty_a with
	| `Array ty -> ty
	| _ -> invalid_arg "Index into non-array type" in
      type_check "Index" ty_i `Int;
      let state, _ =
	expr vars state
	  (If((Llvalue(i, `Int) >=: Int 0) &&:
		(Llvalue(i, `Int) <: Length(Llvalue(a, ty_a))), Unit,
	      compound [ Printf("Array index out of bounds\n", []);
			 Exit(Int 1) ])) in
      let data = extractvalue state a Ref.data in
      let data = state#bitcast data (pointer_type(lltype_of ty_elt)) in
      let x, ty_x = state#load data [i], ty_elt in
      let state = gc_root vars state (lazy x) ty_x in
      state, (x, ty_x)
  | Set(a, i, x) ->
      let state, (a, ty_a), (i, ty_i), (x, ty_x) =
	expr3 vars state a i x in
      type_check "Set with invalid element type" ty_a (`Array ty_x);
      type_check "Set with non-int index" ty_i `Int;
      let state, _ =
	expr vars state
	  (If((Llvalue(i, `Int) >=: Int 0) &&:
		(Llvalue(i, `Int) <: Length(Llvalue(a, ty_a))), Unit,
	      compound [ Printf("Array index out of bounds\n", []);
			 Exit(Int 1) ])) in
      let data = extractvalue state a Ref.data in
      let data = state#bitcast data (pointer_type(lltype_of ty_x)) in
      state#store data [i] x;
      state, (unit, `Unit)
  | Return(Apply(f, args), ty_ret) ->
      let state, (f, ty_f) = expr vars state f in
      let state, (args, tys_arg) = exprs vars state args in
      state#gc_restore();
      type_check "Function" ty_f (`Function(tys_arg, ty_ret));
      let call =
	if is_struct ty_ret then
	  (* Tail call returning struct. Pass the sret given to us by our
	     caller on to our tail callee. *)
          state#call cc f (state#thread_local :: state#sret :: args)
	else
	  (* Tail call returning single value. *)
	  state#call cc f (state#thread_local :: args) in
      if !Options.tco then
	set_tail_call true call;
      state#ret call;
      raise Returned
  | Apply(f, args) ->
      let state, (f, ty_f) = expr vars state f in
      let state, (args, tys_arg) = exprs vars state args in
      let ret, ty_ret =
	match ty_f with
	| `Function(tys_arg', ty_ret) when is_struct ty_ret ->
	    (* Non-tail call returning multiple values. *)
	    List.iter2 (type_check "Arg") tys_arg tys_arg';
            let ret = state#alloca (lltype_of ty_ret) in
            let _ = state#call cc f (state#thread_local :: ret :: args) in
            state#load ret [int 0], ty_ret
	| `Function(tys_arg', ty_ret) ->
	    (* Non-tail call returning single value. *)
	    List.iter2 (type_check "Arg") tys_arg tys_arg';
	    state#call cc f (state#thread_local :: args), ty_ret
	| _ -> invalid_arg "Apply of non-function" in
      let state = gc_root vars state (lazy ret) ty_ret in
      state, (ret, ty_ret)
  | Printf(spec, args) ->
      let spec = state#gep (mk_string spec) [int32 0; int 0] in
      let state, (args, _) = exprs vars state args in
      let ext x =
	if type_of x <> single_type then x else
	  build_fpext x double_type "" state#bb in
      let args = List.map ext args in
      ignore(state#call CallConv.c Extern.printf (spec::args));
      state, (unit, `Unit)
  | IntOfInt(ty, f) ->
      let ty = (ty :> Type.t) in
      let state, (f, ty_f) = expr vars state f in
      let build = match ty_f, ty with
      | `Byte, `Int | `Int, `Int64 -> build_zext
      | `Int64, `Int | `Int, `Byte -> build_trunc
      | `Byte, `Byte | `Int, `Int | `Int64, `Int64 ->
	(fun f _ _ _ -> f)
      | _ -> invalid_arg "IntOfInt" in
      state, (build f (lltype_of ty) "" state#bb, ty)
  | IntOfFloat(ty, f) ->
      let ty = (ty :> Type.t) in
      let state, (f, ty_f) = expr vars state f in
      type_check "IntOfFloat of non-float" ty_f `Float;
      state, (build_fptosi f (lltype_of ty) "" state#bb, ty)
  | FloatOfInt(ty, f) ->
      let ty = (ty :> Type.t) in
      let state, (f, ty_f) = expr vars state f in
      (* FIXME: Should also handle Byte and Int64 integers. *)
      type_check "FloatOfInt of non-int" ty_f `Int;
      state, (build_sitofp f (lltype_of ty) "" state#bb, ty)
  | Print f ->
      let state, (f, ty_f) = expr vars state f in
      let vars = add_val ("x", (f, ty_f)) vars in
      begin
	match ty_f with
	| `Unit -> expr vars state (Printf("()", []))
	| `Bool ->
	    expr vars state
	      (If(Var "x", Printf("true", []), Printf("false", [])))
	| `Byte | `Int -> expr vars state (Printf("%d", [Var "x"]))
	| `Int64 -> expr vars state (Printf("%lldL", [Var "x"]))
	| `Float -> expr vars state (Printf("%#g", [Var "x"]))
	| `Struct tys ->
	    let aux i = Print(GetValue(Var "x", i)) in
	    let xs = List.init (List.length tys) aux in
	    expr vars state
	      (compound
		 [ Printf("(", []);
		   compound(List.between (Printf(", ", [])) xs);
		   Printf(")", []) ])
	| `Function _ -> expr vars state (Printf("<fun>", []))
	| `Array _
	| `Reference ->
	    (*
	      expr vars state
	      (Printf("<abstr>", []))
	    *)
	    let llty = extractvalue state f Ref.llty in
	    let llty = state#bitcast llty (pointer_type RTType.lltype) in
	    let llty = state#load llty [int 0] in
	    let p = extractvalue state llty RTType.print in
	    let ty_p = `Function([ty_f], `Unit) in
	    let vars = add_val ("p", (p, ty_p)) vars in
	    expr vars state (Apply(Var "p", [Var "x"]))
      end
  | Visit f ->
      let state, (f, ty_f) = expr vars state f in
      begin
	match ty_f with
	| `Reference ->
	    let llty = extractvalue state f Ref.llty in
	    let llty = state#bitcast llty (pointer_type RTType.lltype) in
	    let llty = state#load llty [int 0] in
	    let p = extractvalue state llty RTType.visit in
	    state, (p, RTType.ty_visit)
	| ty -> invalid_arg "Visit of non-reference"
      end
  | Free f ->
      let state, (f, ty_f) = expr vars state f in
      begin
	match ty_f with
	| `Array _ | `Reference ->
	    state#free (extractvalue state f Ref.data);
	    state#free (extractvalue state f Ref.mark);
	| `Int ->
	    state#free (state#ptr_of_int f string_type);
	| _ -> invalid_arg "Free of non-(array|reference|int)"
      end;
      state, (unit, `Unit)
  | Exit f ->
      (** FIXME: The libc exit function is not thread safe so we should
	  lock. *)
      let state, (f, ty_f) = expr vars state f in
      type_check "Exit" ty_f `Int;
      ignore(state#call CallConv.c Extern.exit [f]);
      state, (unit, `Unit)
  | Load(ptr, ty) ->
      state, (state#load ptr [int 0], ty)
  | Store(ptr, f) ->
      let state, (f, ty_f) = expr vars state f in
      state#store ptr [int 0] f;
      state, (unit, `Unit)
  | AddressOf f ->
      let state, (f, ty_f) = expr vars state f in
      if not(is_ref_type ty_f) then
	invalid_arg "AddressOf of non-reference";
      let ptr = extractvalue state f Ref.data in
      let ptr = state#int_of_ptr ptr in
      state, (ptr, `Int)
  | Llvalue(v, ty) -> state, (v, ty)
  | Magic(f, ty) ->
      let state, (f, ty_f) = expr vars state f in
      begin
	match ty_f, ty with
	| `Int, `Array ty_elt ->
	    let f = state#ptr_of_int f string_type in
	    state, (Ref.mk state (mk_array_type ty_elt) (int 1) f null, ty)
	| `Reference, `Array _ | `Array _, `Reference -> state, (f, ty)
	| _ -> invalid_arg "Magic of non-(int|reference)";
      end
  | Return(f, ty_ret) ->
      let state, (f, ty_f) = expr vars state f in
      type_check "Return" ty_ret ty_f;
      state#gc_restore();
      if is_struct ty_f then begin
	state#store state#sret [int 0] f;
	state#ret unit;
      end else
	state#ret f;
      raise Returned
  | GetMark f ->
      let state, (f, ty_f) = expr vars state f in
      if not(is_ref_type ty_f) then
	invalid_arg "GetMark of non-reference";
      let mark = extractvalue state f Ref.mark in
(*
      let ptr = state#int_of_ptr mark in
      let state, _ =
	expr vars state
	  (If(Llvalue(ptr, `Int) =: Int 0,
	      compound [ Printf("Missing mark state\n", []);
			 Exit(Int 1) ],
	      Unit)) in
*)
      let mark = state#load mark [int 0] in
      let mark = build_zext mark int_type "int_of_mark" state#bb in
      state, (mark, `Int)
  | SetMark(f, n) ->
      let state, (f, ty_f) = expr vars state f in
      if not(is_ref_type ty_f) then
	invalid_arg "GetMark of non-reference";
      let mark = extractvalue state f Ref.mark in
      state#store mark [int 0] (int8 n);
      state, (unit, `Unit)
  | Time -> state, (state#time, `Float)
  | CreateThread(f, x) ->
      let state, (f, f_ty), (x, x_ty) = expr2 vars state f x in
      type_check "CreateThread" f_ty (`Function([x_ty], `Unit));
      let cf =
	(* Create a wrapper function with the C calling convention that
	   unboxes "x", initializes the thread-local state and applies "f" to
	   "x". *)
	(* Note that the spawned thread will push its arguments onto its
	   shadow stack before the first safe point (that the GC must wait
	   for), so the argument can never get collected between becoming
	   unreachable in this parent thread and reachable in the child
	   thread. *)
	let f_name = sprintf "hlvm_thread_apply<%a>()" Type.to_string x_ty in
	mk_fun ~pass_tl:false vars CallConv.c f_name ["ptr", `Int] `Unit
	  (Unsafe
	     (Let("tfx",
		  Get(Magic(Var "ptr",
			    `Array(`Struct[ThreadLocal.ty; f_ty; x_ty])),
		      Int 0),
		  compound
		    [ Free(Var "ptr");
		      dPrintf("%d registered threads\n",
			      [Seq.count ThreadGlobal.load_list]);
		      SetThreadLocal(GetValue(Var "tfx", 0));
		      ExtSetThreadLocal GetThreadLocal;

		      dPrintf("%p just starting\n",
			      [AddressOf GetThreadLocal]);
		      Apply(GetValue(Var "tfx", 1),
			    [GetValue(Var "tfx", 2)]);

		      dPrintf("%p removing itself from global thread list\n",
			      [AddressOf GetThreadLocal]);
		      ThreadGlobal.lock
			(ThreadGlobal.store_list
			   (Apply(seq_remove ThreadLocal.eq ThreadLocal.ty,
				  [ThreadGlobal.load_list;
				   GetThreadLocal;
				   Int 0])));
		      dPrintf("%p terminating\n", [AddressOf GetThreadLocal]);
		      ThreadLocal.free GetThreadLocal;
		      dPrintf("%d registered threads\n",
			      [Seq.count ThreadGlobal.load_list]);
		    ]))) in
      (* Create new thread-local state, register it on the global thread list
	 and return it. Note that this means a GC cannot occur without the
	 cooperation of the thread we are spawning so we known it will have
	 pushed its roots onto the shadow stack before the next GC. *)
      let state, (t, t_ty) =
	expr vars state
	  (Unsafe
	     (compound
		[ ThreadGlobal.lock
		    (Let("t", ThreadLocal.make,
			 compound
			   [ ThreadGlobal.store_list
			       (Seq.push(ThreadGlobal.load_list, Var "t"));
			     Var "t" ])) ])) in
      type_check "CreateThread internal" t_ty ThreadLocal.ty;
      let state, (ptr, _) =
	expr vars state
	  (Unsafe(AddressOf(Alloc(Int 1, Struct[Llvalue(t, t_ty);
						Llvalue(f, f_ty);
						Llvalue(x, x_ty)])))) in
      let create_thread = state#load Extern.create_thread#ptr [int 0] in
      let cf = state#bitcast cf string_type in
      let thread = state#call CallConv.c create_thread [cf; ptr] in
      state, (thread, `Int)
  | JoinThread f ->
      let state, (f, ty_f) = expr vars state f in
      type_check "JoinThread" ty_f `Int;
      let state, _ = expr vars state (enter_blocking_section()) in
      let join_thread = state#load Extern.join_thread#ptr [int 0] in
      ignore(state#call CallConv.c join_thread [f]);
      let state, _ = expr vars state (leave_blocking_section()) in
      state, (unit, `Unit)
  | CreateMutex ->
      let create_mutex = state#load Extern.create_mutex#ptr [int 0] in
      let m = state#call CallConv.c create_mutex [] in
      state, (m, `Int)
  | UnsafeLockMutex f ->
      let state, (f, ty_f) = expr vars state f in
      type_check "UnsafeLockMutex" ty_f `Int;
      let lock_mutex = state#load Extern.lock_mutex#ptr [int 0] in
      ignore(state#call CallConv.c lock_mutex [f]);
      state, (unit, `Unit)
  | UnlockMutex f ->
      let state, (f, ty_f) = expr vars state f in
      type_check "UnlockMutex" ty_f `Int;
      let unlock_mutex = state#load Extern.unlock_mutex#ptr [int 0] in
      ignore(state#call CallConv.c unlock_mutex [f]);
      state, (unit, `Unit)
  | ExtGetThreadLocal ->
      let get_thread_local = state#load Extern.get_thread_local#ptr [int 0] in
      let ptr = state#call CallConv.c get_thread_local [] in
      let ptr = state#ptr_of_int ptr string_type in
      let llty = mk_array_type ThreadLocal.internal_ty in
      state, (Ref.mk state llty (int 1) ptr null, ThreadLocal.ty)
  | ExtSetThreadLocal f ->
      let state, (f, ty_f) = expr vars state f in
      type_check "ExtSetThreadLocal" ty_f ThreadLocal.ty;
      let ptr = extractvalue state f Ref.data in
      let ptr = state#int_of_ptr ptr in
      let set_thread_local = state#load Extern.set_thread_local#ptr [int 0] in
      ignore(state#call CallConv.c set_thread_local [ptr]);
      state, (unit, `Unit)
  | GetThreadLocal ->
      let llty = mk_array_type ThreadLocal.internal_ty in
      let ptr = state#thread_local in
      state, (Ref.mk state llty (int 1) ptr null, ThreadLocal.ty)
  | SetThreadLocal f ->
      let state, _ = expr vars state (ExtSetThreadLocal f) in
      let state, (tl, ty) = expr vars state ExtGetThreadLocal in
      let ptr = extractvalue state tl Ref.data in
      state#set_thread_local ptr, (unit, `Unit)
  | GetThreadTick ->
      let ptr = state#bitcast state#thread_local (pointer_type int_type) in
      state, (state#load ptr [int 0], `Int)
  | SetThreadTick f ->
      let state, (f, ty_f) = expr vars state f in
      type_check "ExtSetThreadLocal" ty_f `Int;
      let ptr = state#bitcast state#thread_local (pointer_type int_type) in
      state#store ptr [int 0] f;
      state, (unit, `Unit)

(** Compile two expressions. *)
and expr2 vars state f g =
  let state, f = expr vars state f in
  let state, g = expr vars state g in
  state, f, g

(** Compile three expressions. *)
and expr3 vars state f g h =
  let state, f = expr vars state f in
  let state, g = expr vars state g in
  let state, h = expr vars state h in
  state, f, g, h

(** Compile a list of expressions. *)
and exprs vars state fs =
  let aux (state, rfs, rtys_f) f =
    let state, (f, ty_f) = expr vars state f in
    state, f::rfs, ty_f::rtys_f in
  let state, rfs, rtys_f = List.fold_left aux (state, [], []) fs in
  state, (List.rev rfs, List.rev rtys_f)

(** Compile an expression and return from it, marking any calls in tail
    position as tail calls. *)
and return vars state f ty_f =
  try
    let _ = expr vars state (Return(f, ty_f)) in
    failwith "Internal error: return"
  with Returned ->
    ()

(** Register all reference types in the given value as live roots for the
    GC. *)
and gc_root vars state v ty =
  if !Options.debug then
    printf "gc_root %s\n%!" (Type.to_string () ty);
  match ty with
  | `Unit | `Bool | `Byte | `Int | `Int64 | `Float | `Function _ -> state
  | `Struct tys ->
      let f (i, state) ty =
	let v = lazy(extractvalue state (Lazy.force v) i) in
	i+1, gc_root vars state v ty in
      snd(List.fold_left f (0, state) tys)
  | `Array _  | `Reference ->
      state#gc_root(Lazy.force v)

(** Register an allocated value if necessary. *)
and gc_alloc vars state v =
  if not state#gc_enabled || not !Options.gc_enabled then state else
    let state, _ =
      expr vars state
	(Let("p", Llvalue(v, `Reference),
	     If(AddressOf(Var "p") =: Int 0, Unit,
		Expr.lock
		  (Load(allocated_mutex, `Int),
		   Let("n", Load(n_allocated, `Int),
		       compound
			 [ Set(Load(allocated, `Array `Reference), Var "n",
			       Var "p");
			   Store(n_allocated, Var "n" +: Int 1) ]))))) in
    state

(** Define a function with the given calling convention, name, arguments
    and return type using the function argument "k" to fill in the body of the
    defined function. *)
and defun ?(pass_tl=true) vars cc f args ty_ret k =
  let tys_args = List.map snd args in
  let ty = tys_args, ty_ret in
  if !Options.debug then
    printf "defun ~pass_tl:%b %s %s\n%!" pass_tl f
      (string_of_lltype(function_type_of ~pass_tl ty));
  let llvm_f = define_function f (function_type_of ~pass_tl ty) m in
  set_function_call_conv cc llvm_f;
  
  let entry = mk_state ~pass_tl llvm_f in
  let start = entry#mk "start" in

  (* Bind the function name so the body can make recursive calls. *)
  let vars = add_val (f, (llvm_f, `Function ty)) vars in

  (* Bind the function arguments in the context of its body. *)
  let _, vals' =
    let aux (i, args) (arg, ty) =
      i+1, (arg, (param llvm_f i, ty))::args in
    let i = if is_struct ty_ret then 1 else 0 in
    let i = if pass_tl then i+1 else i in
    List.fold_left aux (i, vars.vals) args in

  k { vals = vals' } start;

  let _ = entry#br start in

  if !Options.view then
    Llvm_analysis.view_function_cfg llvm_f;
  Llvm_analysis.assert_valid_function llvm_f;

  vars

(** Compile a top-level definition. *)
and def vars = function
  | `UnsafeFunction(f, args, ty_ret, body) ->
      let (f, args, ty_ret, body) =
	(if !Options.debug then trace else (fun x -> x))
	  (f, args, ty_ret, body) in

      if !Options.debug then
	printf "def %s\n%!" f;

      let body = Expr.unroll f args body in

      if !Options.debug then
	printf "%s: %d subexpressions\n%!" f (Expr.count body);

      defun vars cc f args ty_ret
	(fun vars state ->
	   return vars state (Unsafe body) ty_ret)
  | `Function(f, args, ty_ret, body) ->
      let (f, args, ty_ret, body) =
	(if !Options.debug then trace else (fun x -> x))
	  (f, args, ty_ret, body) in

      if !Options.debug then
	printf "def %s\n%!" f;

      let body = Expr.unroll f args body in

      if !Options.debug then
	printf "%s: %d subexpressions\n%!" f (Expr.count body);

      defun vars cc f args ty_ret
	(fun vars state ->
	   (* Push arguments of reference types onto the shadow stack. *)
	   let ps() =
	     let _, (ps, _) =
	       expr vars state
		 (Struct(List.map (fun (s, _) -> Var s) args)) in
	     ps in
	   let state =
	     gc_root vars state (lazy(ps())) (`Struct (List.map snd args)) in
	   let body =
	     if is_leaf body then body else
	       compound
		 [ gc_check();
		   body ] in
	   return vars state body ty_ret)
  | `Expr f ->
      if !Options.debug then
	printf "def <expr>\n%!";

      let handler k state =
	if not !Options.stack_handler then k state else
	  let size = 16384 in
	  let stack = state#alloca (array_type i8_type size) in
	  let stack = state#bitcast stack string_type in
	  
	  let ty_handler = function_type void_type [|int_type; string_type|] in
	  let stackoverflow_install_handler =
	    declare_function "stackoverflow_install_handler"
	      (function_type int_type
		 [|pointer_type ty_handler; string_type; int_type|]) m in
	  let stackoverflow_deinstall_handler =
	    declare_function "stackoverflow_deinstall_handler"
	      (function_type void_type [||]) m in
	  
	  let llvm_handler =
	    let llvm_f = define_function "handler" ty_handler m in
	    let state = mk_state ~pass_tl:false llvm_f in
	    String.iter
	      (fun c ->
		 ignore(state#call CallConv.c Extern.putchar [int(Char.code c)]))
	      "Stack overflow\n";
	    let _ = state#call CallConv.c Extern.exit [int 1] in
	    let _ = build_ret_void state#bb in
	    Llvm_analysis.assert_valid_function llvm_f;
	    llvm_f in

	  let _ =
	    if not !Options.stack_handler then unit else
	      state#call CallConv.c stackoverflow_install_handler
		[llvm_handler; stack; int size] in

	  let state = k state in

	  let _ =
	    state#call CallConv.c stackoverflow_deinstall_handler [] in

	  state in

      let f_name = "eval_expr" in
      let vars' =
	defun ~pass_tl:false vars CallConv.c f_name ["", `Int] `Unit
	  (fun vars state ->
	     let state =
	       handler
		 (fun state ->
		    let start_time = Llvalue(state#time, `Float) in
		    (*
		      let state, _ =
		      expr vars state
		      (dPrintf("# "^Expr.to_string () f^"\n", [])) in
		    *)
		    let state, (f, ty_f) =
		      expr vars state
			(compound
			   [ Store(suspend_time, Float 0.0);
			     Store(mark_time, Float 0.0);
			     Store(sweep_time, Float 0.0);
			     f ]) in
		    let f =
		      compound
			[ Printf("- : "^Type.to_string () ty_f^" = ", []);
			  Print(Llvalue(f, ty_f));
			  Printf("\n", []) ] in
		    let state, _ = expr vars state f in
		    state#gc_restore();
		    let state, _ =
		      expr vars state
			(Printf("Live: %d\n", [Load(n_allocated, `Int)]))in
		    let state, _ =
		      expr vars state
			(Printf("%gs total; %gs suspend; %gs mark; %gs sweep\n",
				[ Time -: start_time;
				  Load(suspend_time, `Float);
				  Load(mark_time, `Float);
				  Load(sweep_time, `Float) ])) in
		    state)
		 state in
	     return vars state Unit `Unit) in
      let llvm_f, _ = List.assoc f_name vars'.vals in
      ignore(run_function llvm_f);

      vars
  | `Extern(_, _, `Struct _) ->
      failwith "Not yet implemented: FFI returning struct"
  | `Extern(f, tys_arg, ty_ret) ->
      if !Options.debug then
	printf "def extern %s\n%!" f;
      let fn =
	let ty =
	  function_type (lltype_of ty_ret)
	    (Array.of_list (List.map lltype_of tys_arg)) in
	declare_function f ty m in
      let ty = tys_arg, ty_ret in
      let llvm_f = define_function (uniq("vm_"^f)) (function_type_of ty) m in
      set_function_call_conv cc llvm_f;
      let entry = mk_state llvm_f in
      let start = entry#mk "start" in
      let args =
	List.init (List.length tys_arg) (fun i -> param llvm_f (i+1)) in
      start#ret(start#call CallConv.c fn args);
      let _ = entry#br start in
      Llvm_analysis.assert_valid_function llvm_f;
      add_val (f, (llvm_f, `Function ty)) vars
  | `Type(c, ty) ->
      if !Options.debug then
	printf "def type %s\n%!" c;
      (* Define a new type constructor. *)
      let name = sprintf "%s<%a>" c Type.to_string ty in
      if !Options.debug then
	printf "def `Type %s\n%!" name;
      let llty = define_global name (undef RTType.lltype) m in
      Hashtbl.add types c (llty, ty);
      let llvisit = def_visit vars name c ty in
      let llprint = def_print vars name c ty in
      init_type name llty llvisit llprint;
      vars

(** Define a function to traverse a reference. *)
and def_visit vars name c ty =
  let f = "visit_" ^ name in
  let body, vars = visit vars (Var "x") ty in
  let vars =
    def vars
      (`UnsafeFunction(f, ["x", `Reference], `Unit,
		       Let("x", Cast(Var "x", c), body))) in
  let llvisit, _ = List.assoc f vars.vals in
  llvisit

(** Generate an expression that applies the function "f" to every value of a
    reference type in the value "v". *)
and visit vars v = function
  | `Unit | `Bool | `Byte | `Int | `Int64 | `Float | `Function _ -> Unit, vars
  | `Struct tys ->
      let f (i, (fs, vars)) ty =
	let f, vars = visit vars (GetValue(v, i)) ty in
	i+1, (f::fs, vars) in
      let _, (fs, vars) = List.fold_left f (0, ([], vars)) tys in
      compound fs, vars
  | `Array _ -> gc_push(Magic(v, `Reference)), vars
  | `Reference -> gc_push v, vars

(** Define a function that visits every value of a reference type in an
    array. *)
and def_visit_array vars ty =
  let f = sprintf "visit_array<%s>" (Type.to_string () ty) in
  let body, vars = visit vars (Get(Var "a", Var "i")) ty in
  if body = Unit then
    mk_fun vars cc f [ "a", `Reference ] `Unit Unit
  else
    let llvisitaux =
      let f = sprintf "visit_array_aux<%s>" (Type.to_string () ty) in
      mk_fun vars cc f [ "a", `Array ty;
			 "i", `Int ] `Unit
	(Unsafe
	   (If(Var "i" =: Length(Var "a"), Unit,
	       compound
		 [ body;
	           Apply(Var f, [Var "a"; Var "i" +: Int 1]) ]))) in
    mk_fun vars cc f [ "a", `Reference ] `Unit
      (Unsafe
	 (Apply(Llvalue(llvisitaux, `Function([`Array ty; `Int], `Unit)),
		[Magic(Var "a", `Array ty); Int 0])))

(** Define a function to print a boxed value. *)
and def_print vars name c ty =
  let f = sprintf "print<%s>" name in
  mk_fun ~debug:false vars cc f ["x", `Reference] `Unit
    (Unsafe
       (compound
	  [ Printf(c^"(", []);
	    Print(Cast(Var "x", c));
	    Printf(")", []) ]))

(** Define a function to print an array. *)
and def_print_array vars ty =
  let f = "print_array<" ^ Type.to_string () ty ^ ">" in
  let aux = "print_array_aux<" ^ Type.to_string () ty ^ ">" in
  let aux =
    mk_fun ~debug:false vars cc aux ["a", `Array ty; "i", `Int] `Unit
      (Unsafe
	 (compound
	    [ Print(Get(Var "a", Var "i"));
	      If(Var "i" <: Length(Var "a") -: Int 1,
		 If(Var "i" =: Int 5,
		    Printf("; ..", []),
		    compound
		      [ Printf("; ", []);
			Apply(Var aux, [Var "a"; Var "i" +: Int 1]) ]),
		 Unit)])) in
  mk_fun ~debug:false vars cc f ["x", `Reference] `Unit
    (Unsafe
       (Let("a", Magic(Var "x", `Array ty),
	    compound
	      [ Printf("[|", []);
		If(Length(Var "a") =: Int 0, Unit,
		   Apply(Llvalue(aux, `Function([`Array ty; `Int], `Unit)),
			 [Var "a"; Int 0]));
		Printf("|]", [])])))

(** Create and memoize a reference type. Used to create wrapper reference
    types. *)
and mk_type vars ty =
  let name = "Box("^Type.to_string () ty^")" in
  if !Options.debug then
    printf "mk_type %s\n%!" name;
  try vars, (name, Hashtbl.find types name) with Not_found ->
    let vars = def vars (`Type(name, ty)) in
    vars, (name, find_type name)

(** Create and memoize an array type. *)
and mk_array_type ty =
  if !Options.debug then
    printf "mk_array_type<%s>\n%!" (Type.to_string () ty);
  let name = sprintf "mk_array_type<%a>" Type.to_string ty in
  try fst(Hashtbl.find types name) with Not_found ->
    let llty = define_global name (undef RTType.lltype) m in
    Hashtbl.add types name (llty, `Array ty);
    let llvisit = def_visit_array vars ty in
    let llprint = def_print_array vars ty in
    init_type name llty llvisit llprint;
    llty

(** Compile and run code to initialize the contents of a new type. *)
and init_type name llty llvisit llprint =
  if !Options.debug then
    printf "init_type %s\n%!" name;

  let f = sprintf "init_type<%s>" name in
  let vars =
    defun ~pass_tl:false vars CallConv.c f ["", `Int] `Unit
      (fun vars state ->
	 let state = state#gc false in
	 let state, _ =
	   if not !Options.debug then state, (unit, `Unit) else
	     expr vars state (Printf(f^"()\n", [])) in
	 let s =
	   Struct
	     [ Llvalue(llvisit, `Function([`Reference], `Unit));
	       Llvalue(llprint, `Function([`Reference], `Unit)) ] in
	 let state, _ = expr vars state (Unsafe(Store(llty, s))) in
	 let state, _ =
	   if not !Options.debug then state, (unit, `Unit) else
	     expr vars state (Printf(f^" end\n", [])) in
	 return vars state Unit `Unit) in
  let llvm_f, _ = List.assoc f vars.vals in
  if !Options.debug then
    printf "Running init_type %s\n%!" name;
  ignore(run_function llvm_f);
  if !Options.debug then
    printf "Ran init_type %s\n%!" name

(** Create and memoize a function. Used to create visitor functions, print
    functions and array fill functions. *)
and mk_fun ?(debug=true) ?(pass_tl=true) vars cc f args ty_ret body =
  let (f, args, ty_ret, body) =
    (if false && debug && !Options.debug then trace else (fun x -> x))
      (f, args, ty_ret, body) in
  if !Options.debug then
    printf "mk_fun ~pass_tl:%b %s\n%!" pass_tl f;
  try Hashtbl.find functions f with Not_found ->
    let body = Expr.unroll f args body in
    let vars =
      defun ~pass_tl vars cc f args ty_ret
	(fun vars state ->
	   let state = state#gc false in
	   return vars state body ty_ret) in
    let llty, _ = find f vars.vals in
    Hashtbl.add functions f llty;
    llty

(** Define a function to fill an array of the given type. *)
(* Note that this uses recursive subdivision to keep stack consumption down
   to O(log n) so our bootstrapping code can work (fill the shadow stack etc.)
   without stack overflowing even if tail calls are disabled. *)
and fill vars ty =
  (*
    let f = sprintf "Array.fill<%a>" Type.to_string ty in
    let llvm_f =
    mk_fun vars cc f [ "a", `Array ty;
    "x", ty;
    "i", `Int;
    "j", `Int] `Unit
    (If(Var "i" <: Var "j",
    compound
    [ Set(Var "a", Var "i", Var "x");
    Apply(Var f, [Var "a"; Var "x"; Var "i" +: Int 1; Var "j"]) ],
    Unit)) in
    Llvalue(llvm_f, `Function([`Array ty; ty; `Int; `Int], `Unit))
  *)
  let f = sprintf "Array.fill<%a>" Type.to_string ty in
  let llvm_f =
    mk_fun vars cc f [ "a", `Array ty;
		       "x", ty;
		       "i", `Int;
		       "j", `Int ] `Unit
      (Unsafe
	 (If(Var "j" -: Var "i" <: Int 2,
	     If(Var "i" =: Var "j" , Unit,
		Set(Var "a", Var "i", Var "x")),
	     Let("m", Var "i" +: (Var "j" -: Var "i") /: Int 2,
		 compound
		   [ Apply(Var f, [Var "a"; Var "x"; Var "i"; Var "m"]);
		     Apply(Var f, [Var "a"; Var "x"; Var "m"; Var "j"]) ]))))
  in
  Llvalue(llvm_f, `Function([`Array ty; ty; `Int; `Int], `Unit))

(** Define a function to remove an element from a bag using linear search. *)
and seq_remove eq ty =
  let f =
    sprintf "Seq.remove<%a, %a>"
      Expr.to_string (eq (Var "x") (Var "y"))
      Type.to_string ty in
  let llvm_f =
    mk_fun vars cc f [ "seq", Seq.ty ty;
		       "x", ty;
		       "i", `Int ] (Seq.ty ty)
      (If(Var "i" =: Seq.count(Var "seq"),
	  compound
	    [ Printf("Not found in '"^f^"'\n", []);
	      Exit(Int 1);
	      Var "seq" ],
	  If(eq (Var "x") (Seq.get(Var "seq", Var "i")),
	     Seq.remove_at(Var "seq", Var "i"),
	     Apply(Var f, [Var "seq"; Var "x"; Var "i" +: Int 1])))) in
  Llvalue(llvm_f, `Function([Seq.ty ty; ty; `Int], Seq.ty ty))

(** Dynamically load the runtime and initialize the shadow stack and GC. *)
let init() =
  (** Turn on TCO in LLVM. *)
  if !Options.tco then
    enable_tail_call_opt();
  if !Options.debug then
    printf "init()\n%!";
  let f_name = "init_runtime" in
  let vars' =
    defun ~pass_tl:false vars CallConv.c f_name ["", `Int] `Unit
      (fun vars state ->
	 let state = state#gc false in
	 let state, _ =
	   if not !Options.debug then state, (unit, `Unit) else
	     expr vars state (Printf("init_runtime()\n", [])) in
	 let str s =
	   let str = state#define_global "str" (const_stringz llcontext s) in
	   state#gep str [int32 0; int 0] in
	 let libruntime =
	   state#call CallConv.c Extern.dlopen
	     [str "./libruntime.so"; int 1] in
	 let state, _ =
	   let libruntime = state#int_of_ptr libruntime in
	   expr vars state
	     (Unsafe
		(If(Llvalue(libruntime, `Int) =: Int 0,
		    compound
		      [Printf("ERROR: libruntime.so not found\n", []);
		       Exit(Int 1)],
		    Unit))) in
	 (* FIXME: We should check dlerror in case the required symbols are
	    not found. *)
	 let state, _ =
	   if not !Options.debug then state, (unit, `Unit) else
	     expr vars state
	       (Printf("Dynamically loading externs...\n", [])) in
	 let load_fn ll f =
	   let ptr = state#call CallConv.c Extern.dlsym [libruntime; str f] in
	   state#store ll#ptr [int 0] (state#bitcast ptr ll#ty) in
	 Extern.load_fns load_fn;
	 ignore(state#call CallConv.c (state#load Extern.init#ptr [int 0]) []);
	 let n = Options.max_depth in
	 let state, _ =
	   expr vars state
	     (Unsafe
		(compound [ dPrintf("Storing empty global thread list...\n",
				    []);
			    ThreadGlobal.store_list(Seq.empty(Int 0));
			    dPrintf("Registering main thread...\n", []);
			    Let("thread", ThreadLocal.make,
				compound
				  [ dPrintf("%p setting thread local\n",
					    [Var "thread"]);
				    ExtSetThreadLocal(Var "thread");
				    SetThreadLocal(Var "thread");
				    dPrintf("%p registering\n",
					    [AddressOf GetThreadLocal]);
				    ThreadGlobal.store_list
				      (Seq.push(ThreadGlobal.load_list,
						Var "thread")) ]);
			    dPrintf("Creating visit stack...\n", []);
			    Store(visit_stack, Alloc(Int n, Null));
			    dPrintf("Creating allocation list...\n", []);
			    Store(allocated, Alloc(Int n, Null));
			    dPrintf("Creating allocation list's mutex...\n",
				    []);
			    Store(allocated_mutex, CreateMutex);
			    dPrintf("Initializing thread global...\n", []);
			    ThreadGlobal.init;
			    dPrintf("init_runtime() ends\n", []) ])) in
	 return vars state Unit `Unit) in
  let _ =
    let llvm_f, _ = List.assoc f_name vars'.vals in
    run_function llvm_f in
  vars

let boot() : t list =
  if not !Options.gc_enabled then [] else
    let printf(x, y) =
      if !Options.debug then Printf(x, y) else Unit in

    let getMark f = GetMark f in
    let setMark(f, n) = SetMark(f, n) in

    (* If the given reference is non-null and unmarked then mark it and push
       its child references onto the visit stack. *)
    let mark_one p =
      Let("p", p,
	  If(getMark(Var "p") =: Int 1, Unit,
	     compound
	       [ setMark(Var "p", 1);
		 Apply(Visit(Var "p"), [Var "p"]) ])) in
    
    [ `Type("Null", `Unit);

      (* Mark the whole heap: while the visit stack is non-empty, pop a
	 reference and mark it. *)
      `UnsafeFunction
	("gc_mark_3", [], `Unit,
	 Let("n", Load(n_visit, `Int) -: Int 1,
	     If(Var "n" <: Int 0, Unit,
		Let("a", Load(visit_stack, `Array `Reference),
		    compound
		      [ Store(n_visit, Var "n");
			mark_one(Get(Var "a", Var "n"));
			Apply(Var "gc_mark_3", []) ]))));
      
      (* Mark all roots on the given shadow stack. *)
      `UnsafeFunction
	("gc_mark_2", ["stack", Seq.ty `Reference; "i", `Int], `Unit,
	 If(Var "i" =: Seq.count(Var "stack"), Unit,
	    compound
	      [ Let("p", Seq.get(Var "stack", Var "i"),
		    If(AddressOf(Var "p") =: Int 0, Unit,
		       mark_one(Var "p")));
		Apply(Var "gc_mark_2", [Var "stack";
					Var "i" +: Int 1]) ]));
      
      (* Mark each shadow stack and then mark the whole heap. *)
      `UnsafeFunction
	("gc_mark", ["i", `Int], `Unit,
	 Let("a", ThreadGlobal.load_list,
	     If(Var "i" =: Seq.count(Var "a"),
		Apply(Var "gc_mark_3", []),
		compound
		  [ dPrintf("Marking %d roots from %p\n",
			    [Seq.count
			       (ThreadLocal.stack_of
				  (Get(Seq.get(Var "a", Var "i"), Int 0)));
			     AddressOf(Seq.get(Var "a", Var "i"))]);
		    Apply(Var "gc_mark_2",
			  [ ThreadLocal.stack_of
			      (Get(Seq.get(Var "a", Var "i"), Int 0));
			    Int 0 ]);
		    Apply(Var "gc_mark", [Var "i" +: Int 1]) ])));

      (* Search the allocated list for unmarked references and free them,
	 shrinking the allocated list if it is non-empty by overwriting the
	 freed reference with the last reference. Reset marked references to
	 unmarked. *)
      `UnsafeFunction
	("gc_sweep", ["i", `Int], `Unit,
	 Let("n", Load(n_allocated, `Int),
	     If(Var "i" >=: Var "n", Unit,
		Let("a", Load(allocated, `Array `Reference),
		    Let("p", Get(Var "a", Var "i"),
			Let("i",
			    If(getMark(Var "p") =: Int 1,
			       compound
				 [ setMark(Var "p", 0);
				   Var "i" +: Int 1 ],
			       compound
				 [ Free(Var "p");
				   Set(Var "a", Var "i",
				       Get(Var "a", Var "n" -: Int 1));
				   Store(n_allocated, Var "n" -: Int 1);
				   Var "i" ]),
			    Apply(Var "gc_sweep", [Var "i"])))))));
      
      (* Check if any mutator thread is still running. *)
      `UnsafeFunction
	("gc_suspend_2", ["i", `Int], `Bool,
	 Let("a", ThreadGlobal.load_list,
	     If(Var "i" =: Seq.count(Var "a"),
		Bool true,
		Let("t", Seq.get(Var "a", Var "i"),
		    If(ThreadLocal.eq (Var "t") GetThreadLocal,
		       Apply(Var "gc_suspend_2", [Var "i" +: Int 1]),
		       If(ThreadLocal.load_state(Var "t") =: Int State.run,
			  compound
			    [ dPrintf("%d/%d %p waiting for %p\n",
				      [Var "i";
				       Seq.count(Var "a");
				       AddressOf GetThreadLocal;
				       AddressOf(Var "t")]);
			      Let("", Apply(Var "gc_suspend_2",
					    [Var "i" +: Int 1]),
				  Bool false) ],
			  compound
			    [ dPrintf("%d/%d %p not waiting for %p\n",
				      [Var "i";
				       Seq.count(Var "a");
				       AddressOf GetThreadLocal;
				       AddressOf(Var "t")]);
			      Apply(Var "gc_suspend_2",
				    [Var "i" +: Int 1]) ]))))));
      
      `Extern("usleep", [`Int], `Unit);

      (* Loop until all threads have suspended themselves. *)
      `UnsafeFunction
	("gc_suspend", [], `Unit,
	 If(ThreadGlobal.lock(Apply(Var "gc_suspend_2", [Int 0])),
	    Unit,
	    compound
	      [ Apply(Var "gc_suspend", []) ]));

      (* Clear, mark and sweep. *)
      `UnsafeFunction
	("gc", [], `Unit,
       let time t fs =
	 Let("time", Time,
	     compound
	       (fs @
		  [ printf("Took %gs\n", [Time -: Var "time"]);
		    Store(t, Load(t, `Float) +: Time -: Var "time") ])) in
       if not !Options.gc_enabled then compound [] else
	 compound
	   [
	     dPrintf("GC suspending...\n", []);
	     time suspend_time [ Apply(Var "gc_suspend", []) ];
	     dPrintf("GC marking...\n", []);
	     time mark_time [ Apply(Var "gc_mark", [Int 0]) ];
	     dPrintf("GC sweeping...\n", []);
	     time sweep_time [ Apply(Var "gc_sweep", [Int 0]) ];
	     dPrintf("GC done. Live: %d\n\n", [Load(n_allocated, `Int)]);
	     Store(quota, Int 256 +: Int 2 *: Load(n_allocated, `Int));
	     dPrintf("GC finished. Restarting %d mutators with quota %d.\n",
		     [Seq.count(ThreadGlobal.load_list); Load(quota, `Int)]);
	     ThreadGlobal.lock(ThreadGlobal.store_state State.run);
	   ]);

      (* Wait in a loop until the global thread state reverts back to "run"
	 and then set the state of this thread to running. *)
      `UnsafeFunction
	("spin", [], `Unit,
	 If(ThreadGlobal.lock
	      (If(ThreadGlobal.load_state =: Int State.run,
		  compound
		    [ ThreadLocal.store_state GetThreadLocal State.run;
		      Bool true ],
		  Bool false)),
	    Unit,
	    Apply(Var "spin", [])));

      `UnsafeFunction
	("gc_check", [], `Unit,
	 Let("tl", GetThreadLocal,
	     Let("status",
		 ThreadGlobal.lock
		   (If(ThreadGlobal.load_state =: Int State.run,
		       If(Load(n_allocated, `Int) <=: Load(quota, `Int),
			  Int 0,
			  compound
			    [ dPrintf("Suspending all other threads %p\n",
				      [AddressOf(Var "tl")]);
			      ThreadGlobal.store_state State.suspend;
			      Int 1 ]),
		       Int 2)),
		 compound
		   [ If(Var "status" =: Int 0,
			Unit,
			If(Var "status" =: Int 1,
			   Apply(Var "gc", []),
			   compound
			     [ dPrintf("%p suspending itself\n",
				       [AddressOf(Var "tl")]);
			       ThreadLocal.store_state (Var "tl")
				 State.suspend;
			       Apply(Var "spin", []);
			       dPrintf("%p resuming itself\n",
				       [AddressOf(Var "tl")]) ]));
		     SetThreadTick(Int 0);
		   ])));
    ]

(** Bound variables. *)
let vars = ref vars

(** Evaluate a statement, updating the bound variables. *)
let eval stmt =
  vars := def !vars stmt

(** Save everything that has been evaluated as a standalone program. *)
let save() =
  let f_name = "main" in
  let _ =
    defun ~pass_tl:false !vars CallConv.c f_name [] `Unit
      (fun vars state ->
	 let state = state#gc false in
	 let call llf =
	   ignore(state#call CallConv.c llf [int 0]) in
	 List.iter call !eval_functions;
	 return vars state Unit `Unit) in
  Llvm_analysis.assert_valid_module m;
  ignore(Llvm_bitwriter.write_bitcode_file m "aout.bc")

let () =
  Array.iter (function
		| "--debug" -> Options.debug := true
		| "--compile" -> Options.compile_only := true
		| "--view-functions" -> Options.view := true
		| "--no-shadow-stack" ->
		    printf "Shadow stack and GC disabled.\n%!";
		    Options.shadow_stack_enabled := false;
		    Options.gc_enabled := false
		| "--no-gc" ->
		    printf "GC disabled.\n%!";
		    Options.gc_enabled := false
		| "--no-stack-handler" ->
		    printf "Stack handler disabled.\n%!";
		    Options.stack_handler := false
		| "--no-tco" ->
		    printf "Tail call elimination disabled.\n%!";
		    Options.tco := false
		| _ -> ()) Sys.argv;
  vars := List.fold_left def (init()) (boot())
