type simple = A of int

type u = A of int
and b = B of bool 


type loc =   FanLoc.t
and meta_bool = 
| BTrue | BFalse | BAnt of  string  and rec_flag = 
| ReRecursive | ReNil | ReAnt of  string
and direction_flag = 
| DiTo | DiDownto | DiAnt of  string  and mutable_flag = 
| MuMutable | MuNil | MuAnt of  string  and private_flag = 
| PrPrivate | PrNil | PrAnt of  string  and virtual_flag = 
| ViVirtual | ViNil | ViAnt of  string  and override_flag = 
| OvOverride | OvNil | OvAnt of  string  and row_var_flag = 
| RvRowVar | RvNil | RvAnt of  string  and 'a meta_option = 
| ONone | OSome of 'a  | OAnt of  string  and 'a meta_list = 
| LNil | LCons of 'a *'a  meta_list  | LAnt of  string  and ident = 
| IdAnt of  loc * string  and ctyp = 
| TyDcl of  loc * string * ctyp  list * ctyp *( ctyp * ctyp ) list 
| TyVrnInfSup of  loc * ctyp * ctyp 
| TyAmp of  loc * ctyp * ctyp 
and patt = 
| ExFor of  loc * string * expr * expr * direction_flag * expr 

type u 
type u = [`a | `b];;

type u = v = A of int ;;
type u = v = private A of int ;;

type _ a =
| A : int -> int a
| B : float -> float a
type _ a = A : int -> int a
type _ a = A : int -> int a | B : int -> float a    



type ('u,'v) a =
  | A of int list * bool
  | B of bool 








