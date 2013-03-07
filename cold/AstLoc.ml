open LibUtil
open FanLoc.Ops
include Ast
let _ = ()
let loc_of =
  function
  | `PaOlbi (_loc,_,_,_) -> _loc
  | `Any _loc -> _loc
  | `Tup (_loc,_) -> _loc
  | `Array (_loc,_) -> _loc
  | `MtFun (_loc,_,_,_) -> _loc
  | `Id (_loc,_) -> _loc
  | `ArrayDot (_loc,_,_) -> _loc
  | `Directive (_loc,_,_) -> _loc
  | `TypeSubst (_loc,_,_) -> _loc
  | `QuoteAny (_loc,_) -> _loc
  | `TyEq (_loc,_,_) -> _loc
  | `ModuleBind (_loc,_,_,_) -> _loc
  | `Sta (_loc,_,_) -> _loc
  | `Match (_loc,_,_) -> _loc
  | `Obj (_loc,_,_) -> _loc
  | `TyPol (_loc,_,_) -> _loc
  | `Val (_loc,_,_) -> _loc
  | `C (_loc,_) -> _loc
  | `Str (_loc,_) -> _loc
  | `Or (_loc,_,_) -> _loc
  | `New (_loc,_) -> _loc
  | `Value (_loc,_,_) -> _loc
  | `PolyInfSup (_loc,_,_) -> _loc
  | `Try (_loc,_,_) -> _loc
  | `Downto _loc -> _loc
  | `App (_loc,_,_) -> _loc
  | `Assign (_loc,_,_) -> _loc
  | `Normal _loc -> _loc
  | `True _loc -> _loc
  | `Sem (_loc,_,_) -> _loc
  | `Send (_loc,_,_) -> _loc
  | `Type (_loc,_) -> _loc
  | `Chr (_loc,_) -> _loc
  | `IfThenElse (_loc,_,_,_) -> _loc
  | `ClassType (_loc,_) -> _loc
  | `ModuleUnpack (_loc,_,_) -> _loc
  | `CeTyc (_loc,_,_) -> _loc
  | `CrVir (_loc,_,_,_) -> _loc
  | `Package (_loc,_) -> _loc
  | `While (_loc,_,_) -> _loc
  | `CrMth (_loc,_,_,_,_,_) -> _loc
  | `Dot (_loc,_,_) -> _loc
  | `Struct (_loc,_) -> _loc
  | `CeCon (_loc,_,_,_) -> _loc
  | `TyMan (_loc,_,_,_) -> _loc
  | `External (_loc,_,_,_) -> _loc
  | `CgVal (_loc,_,_,_,_) -> _loc
  | `Class (_loc,_) -> _loc
  | `LetIn (_loc,_,_,_) -> _loc
  | `Seq (_loc,_) -> _loc
  | `Quote (_loc,_,_) -> _loc
  | `TypeEq (_loc,_,_) -> _loc
  | `OptLabl (_loc,_,_) -> _loc
  | `Coercion (_loc,_,_,_) -> _loc
  | `CtFun (_loc,_,_) -> _loc
  | `Arrow (_loc,_,_) -> _loc
  | `Bind (_loc,_,_) -> _loc
  | `CtEq (_loc,_,_) -> _loc
  | `Functor (_loc,_,_,_) -> _loc
  | `With (_loc,_,_) -> _loc
  | `NativeInt (_loc,_) -> _loc
  | `TyVrnOf (_loc,_,_) -> _loc
  | `Private _loc -> _loc
  | `Virtual _loc -> _loc
  | `RowVar _loc -> _loc
  | `IfThen (_loc,_,_) -> _loc
  | `RecBind (_loc,_,_) -> _loc
  | `Sig (_loc,_) -> _loc
  | `Mutable _loc -> _loc
  | `PolySup (_loc,_) -> _loc
  | `Ctyp (_loc,_) -> _loc
  | `ReNil _loc -> _loc
  | `Lazy (_loc,_) -> _loc
  | `CeFun (_loc,_,_) -> _loc
  | `ClassPath (_loc,_) -> _loc
  | `Nil _loc -> _loc
  | `False _loc -> _loc
  | `Com (_loc,_,_) -> _loc
  | `TyRepr (_loc,_,_) -> _loc
  | `Int64 (_loc,_) -> _loc
  | `ModuleTypeOf (_loc,_) -> _loc
  | `To _loc -> _loc
  | `TyCol (_loc,_,_) -> _loc
  | `CgVir (_loc,_,_,_) -> _loc
  | `Initializer (_loc,_) -> _loc
  | `TyColMut (_loc,_,_) -> _loc
  | `ModuleEq (_loc,_,_) -> _loc
  | `Lid (_loc,_) -> _loc
  | `Record (_loc,_) -> _loc
  | `Constraint (_loc,_,_) -> _loc
  | `Ant (_loc,_) -> _loc
  | `Package_expr (_loc,_) -> _loc
  | `TypeEqPriv (_loc,_,_) -> _loc
  | `Label (_loc,_,_) -> _loc
  | `TyTypePol (_loc,_,_) -> _loc
  | `RvNil _loc -> _loc
  | `Override _loc -> _loc
  | `Include (_loc,_) -> _loc
  | `Flo (_loc,_) -> _loc
  | `Alias (_loc,_,_) -> _loc
  | `Vrn (_loc,_) -> _loc
  | `StExp (_loc,_) -> _loc
  | `Uid (_loc,_) -> _loc
  | `TyObj (_loc,_,_) -> _loc
  | `Of (_loc,_,_) -> _loc
  | `OvrInst (_loc,_) -> _loc
  | `OvNil _loc -> _loc
  | `ModuleSubst (_loc,_,_) -> _loc
  | `Positive _loc -> _loc
  | `CrVal (_loc,_,_,_,_) -> _loc
  | `TyVrn (_loc,_) -> _loc
  | `Case (_loc,_,_,_) -> _loc
  | `ExAsr (_loc,_) -> _loc
  | `Exception (_loc,_) -> _loc
  | `CtCol (_loc,_,_) -> _loc
  | `RecordWith (_loc,_,_) -> _loc
  | `And (_loc,_,_) -> _loc
  | `TyDcl (_loc,_,_,_,_) -> _loc
  | `Int32 (_loc,_) -> _loc
  | `PaRng (_loc,_,_) -> _loc
  | `RecModule (_loc,_) -> _loc
  | `LocalTypeFun (_loc,_,_) -> _loc
  | `PrNil _loc -> _loc
  | `Int (_loc,_) -> _loc
  | `Negative _loc -> _loc
  | `Fun (_loc,_) -> _loc
  | `CeApp (_loc,_,_) -> _loc
  | `Eq (_loc,_,_) -> _loc
  | `LetModule (_loc,_,_,_) -> _loc
  | `LetOpen (_loc,_,_) -> _loc
  | `PolyInf (_loc,_) -> _loc
  | `StringDot (_loc,_,_) -> _loc
  | `For (_loc,_,_,_,_,_) -> _loc
  | `CrVvr (_loc,_,_,_) -> _loc
  | `CtCon (_loc,_,_,_) -> _loc
  | `Recursive _loc -> _loc
  | `CtSig (_loc,_,_) -> _loc
  | `SigInherit (_loc,_) -> _loc
  | `ModuleType (_loc,_,_) -> _loc
  | `Inherit (_loc,_,_,_) -> _loc
  | `MuNil _loc -> _loc
  | `Method (_loc,_,_,_) -> _loc
  | `Module (_loc,_,_) -> _loc
  | `ExAsf _loc -> _loc
  | `PackageModule (_loc,_) -> _loc
  | `PolyEq (_loc,_) -> _loc
  | `CeLet (_loc,_,_,_) -> _loc
  | `Open (_loc,_) -> _loc
  | `ViNil _loc -> _loc
  | `Sum (_loc,_) -> _loc
let ghost = FanLoc.ghost
let (<+>) a b = (loc_of a) <+> (loc_of b)
let sem a b = let _loc = a <+> b in `Sem (_loc, a, b)
let com a b = let _loc = a <+> b in `Com (_loc, a, b)
let app a b = let _loc = a <+> b in `App (_loc, a, b)
let sta a b = let _loc = a <+> b in `Sta (_loc, a, b)
let ora a b = let _loc = a <+> b in `Or (_loc, a, b)
let anda a b = let _loc = a <+> b in `And (_loc, a, b)
let dot a b = let _loc = a <+> b in `Dot (_loc, a, b)
let tup x = let _loc = loc_of x in `Tup (_loc, x)
let seq a = let _loc = loc_of a in `Seq (_loc, a)
let typing a b = let _loc = a <+> b in `Constraint (_loc, a, b)
let rec or_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> ora t (or_of_list ts)
let rec and_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> anda t (and_of_list ts)
let rec sem_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> sem t (sem_of_list ts)
let rec com_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> com t (com_of_list ts)
let rec sta_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> sta t (sta_of_list ts)
let rec dot_of_list =
  function | [] -> `Nil ghost | t::[] -> t | t::ts -> dot t (dot_of_list ts)
let rec appl_of_list x =
  match x with
  | [] -> `Nil ghost
  | x::[] -> x
  | x::y::xs -> appl_of_list ((app x y) :: xs)
let rec appl_of_list1 x =
  match x with
  | [] -> failwith "appl_of_list1 empty list"
  | x::[] -> x
  | x::y::xs -> appl_of_list1 ((app x y) :: xs)
let tuple_com y =
  match y with
  | [] -> failwith "tuple_com empty"
  | x::[] -> x
  | x::_ -> let _loc = x <+> (List.last y) in `Tup (_loc, (com_of_list y))
let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | x::[] -> x
  | x::_ -> let _loc = x <+> (List.last y) in `Tup (_loc, (sta_of_list y))
let rec and_of_list1 =
  function
  | [] -> failwithf "and_of_list1 empty list"
  | t::[] -> t
  | t::ts -> anda t (and_of_list1 ts)
let rec sem_of_list1 =
  function
  | [] -> failwith "sem_of_list1 empty list"
  | t::[] -> t
  | t::ts -> sem t (sem_of_list1 ts)
let rec com_of_list1 =
  function
  | [] -> failwith "com_of_list1 empty list"
  | t::[] -> t
  | t::ts -> com t (com_of_list1 ts)
let rec dot_of_list1 =
  function
  | [] -> failwith "dot_of_list1 empty list"
  | i::[] -> i
  | i::is -> dot i (dot_of_list1 is)
let rec list_of_and x acc =
  match x with
  | `And (_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x :: acc
let rec list_of_and' x acc =
  match x with
  | `And (_,x,y) -> list_of_and' x (list_of_and' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_com x acc =
  match x with
  | `Com (_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x :: acc
let rec list_of_com' x acc =
  match x with
  | `Com (_,x,y) -> list_of_com' x (list_of_com' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_star x acc =
  match x with
  | `Sta (_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x :: acc
let rec list_of_star' x acc =
  match x with
  | `Sta (_,x,y) -> list_of_star' x (list_of_star' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_or x acc =
  match x with
  | `Or (_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x :: acc
let rec list_of_or' x acc =
  match x with
  | `Or (_,x,y) -> list_of_or' x (list_of_or' y acc)
  | `Nil _ -> acc
  | _ -> x :: acc
let rec list_of_sem x acc =
  match x with
  | `Sem (_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x :: acc
let rec list_of_sem' (x : 'a) acc =
  match x with
  | `Sem (_,x,y) -> list_of_sem' x (list_of_sem' y acc)
  | `Nil _ -> acc
  | y -> y :: acc
let rec list_of_app x acc =
  match x with
  | `App (_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  | x -> x :: acc
let rec list_of_app' x acc =
  match x with
  | `App (_,t1,t2) -> list_of_app' t1 (list_of_app' t2 acc)
  | `Nil _ -> acc
  | x -> x :: acc
let rec view_app acc =
  function | `App (_,f,a) -> view_app (a :: acc) f | f -> (f, acc)
let seq_sem ls = seq (sem_of_list ls)