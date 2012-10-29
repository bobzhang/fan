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

type 'a ab = [< `a|`b] as 'a 
type 'a ac = 'a constraint 'a = [< `a | `c ]
type ('a,'b) m = [< `m of 'a ab & 'a ac ] as 'b

type _ a =
| A : int -> int a
| B : float -> float a
type _ a = A : int -> int a
type _ a = A : int -> int a | B : int -> float a    



type ('u,'v) a =
  | A of int list * bool
  | B of bool

type  gram = 
  { gfilter: filter ;
    gkeywords:( string , int  ref ) Hashtbl.t ;
    glexer:
      ( FanLoc.t ->
        (char Stream.t  ->
              ( token * FanLoc.t ) Stream.t ) ) ;
    warning_verbose: bool  ref ;
    error_verbose: bool  ref }
and  symbol =
  [ `Smeta of ( string * symbol  list * Action.t )
  | `Snterm of  internal_entry
  | `Snterml of ( internal_entry * string )
  | `Slist0 of  symbol
  | `Slist0sep of  ( symbol * symbol )
  | `Slist1 of  symbol
  | `Slist1sep of ( symbol * symbol )
  | `Sopt of  symbol
  | `Stry of  symbol
  | `Sself
  | `Snext
  | `Stoken of  token_pattern
  | `Skeyword of string
  | `Stree of  tree ]
        

type +'a my = H of 'a | D;;
type (+'a,-'b) u = H of 'a | D of ('b -> int)

type 'a item_or_def =  
  | SdStr of 'a
  | SdDef of string *(string  list *Ast.expr ) option 
  | SdUnd of string 
  | SdITE of bool *'a item_or_def  list *'a item_or_def  list 
  | SdLazy of 'a Lazy.t  
