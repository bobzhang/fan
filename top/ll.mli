(* A simplified subset of the LLVM IR *)

(* Unique identifiers and labels *)
type uid = int * string
type lbl = string


(* Binary operations *)
type bop = Add | Sub | Mul | Shl | Lshr | Ashr | And | Or | Xor      

type ty =  I32 | Ptr of ty 


    
(* Operands *)
type opn = Local of uid  | Const of int32 | Global of uid

type operand = {
    opn:opn;
    ty:ty
  }
(* Comparison Operators *)
type cmpop = Eq | Ne | Slt | Sle | Sgt | Sge
  
(* Instructions *)
type insn = 
  | Binop of operand * bop   *  operand * operand
        (* "%s = %s i32 %s, %s"  *)
  | Alloca of  operand
        (* "%s = alloca i32" *)

  | Load of  operand * operand
        (* "%s = load i32* %s" *)
  | Store of  operand * operand
        (* "store i32 %s, i32* %s" *)
  | Icmp of   operand * cmpop * operand * operand
        (* 
           "%s = icmp %s i32 %s, %s"
           "%s = zext i1 %s to i32"
         *)
  | Call of operand option * fn * operand list

  | Bitcast of operand * operand
        
  | Gep of operand *  operand * operand * operand option
and fn = {
    rty: ty option;
    ty_args: ty list;
    name: string
  }

(* Block terminators *)
type terminator = 
  | Ret of operand option
        (* "ret i32 %s" *)
  | Br of lbl
        (* "br label %%%s" *)
  | Cbr of operand * lbl * lbl
        (*
          "%s = icmp ne i32 %s, 0" 
          "br i1 %s, label %%%s, label %%%s"
         *)

(* Basic blocks *)
type bblock =
    {label: lbl;
     insns: insn list;
     terminator: terminator
   }
and fdecl = {
    ll_name: string;
    ll_type: ty option;
    ll_args: operand list;
    ll_cfg: bblock list;
    (* ll_entry: lbl; *)
  }

type elt = L of lbl | I of insn | T of terminator
type stream = elt list
      
type prog = {
    defs:fdecl list;
    protos: fn list;
    variables: operand list
  }


