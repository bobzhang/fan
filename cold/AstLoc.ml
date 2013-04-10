open LibUtil

open FanLoc.Ops

open Ast

let _ = (); ()

let _ = ()

let loc_of =
  function
  | `Apply (_loc,_,_) -> _loc
  | `Any _loc -> _loc
  | `Array (_loc,_) -> _loc
  | `ArrayDot (_loc,_,_) -> _loc
  | `Directive (_loc,_,_) -> _loc
  | `TypeSubst (_loc,_,_) -> _loc
  | `QuoteAny (_loc,_) -> _loc
  | `TyEq (_loc,_,_) -> _loc
  | `ModuleBind (_loc,_,_,_) -> _loc
  | `Sta (_loc,_,_) -> _loc
  | `Match (_loc,_,_) -> _loc
  | `Obj (_loc,_) -> _loc
  | `TyPol (_loc,_,_) -> _loc
  | `Val (_loc,_,_) -> _loc
  | `C (_loc,_) -> _loc
  | `LabelS (_loc,_) -> _loc
  | `Str (_loc,_) -> _loc
  | `New (_loc,_) -> _loc
  | `Value (_loc,_,_) -> _loc
  | `PolyInfSup (_loc,_,_) -> _loc
  | `Try (_loc,_,_) -> _loc
  | `Downto _loc -> _loc
  | `App (_loc,_,_) -> _loc
  | `Assign (_loc,_,_) -> _loc
  | `Normal _loc -> _loc
  | `Sem (_loc,_,_) -> _loc
  | `Send (_loc,_,_) -> _loc
  | `ModuleTypeEnd (_loc,_) -> _loc
  | `Type (_loc,_) -> _loc
  | `Chr (_loc,_) -> _loc
  | `IfThenElse (_loc,_,_,_) -> _loc
  | `ClassType (_loc,_) -> _loc
  | `ModuleUnpack (_loc,_) -> _loc
  | `CrVir (_loc,_,_,_) -> _loc
  | `Package (_loc,_) -> _loc
  | `While (_loc,_,_) -> _loc
  | `CrMth (_loc,_,_,_,_,_) -> _loc
  | `Dot (_loc,_,_) -> _loc
  | `Struct (_loc,_) -> _loc
  | `TyMan (_loc,_,_,_) -> _loc
  | `External (_loc,_,_,_) -> _loc
  | `CgVal (_loc,_,_,_,_) -> _loc
  | `Class (_loc,_) -> _loc
  | `CrMthS (_loc,_,_,_,_) -> _loc
  | `LetIn (_loc,_,_,_) -> _loc
  | `Nativeint (_loc,_) -> _loc
  | `Seq (_loc,_) -> _loc
  | `Quote (_loc,_,_) -> _loc
  | `TypeEq (_loc,_,_) -> _loc
  | `DirectiveSimple (_loc,_) -> _loc
  | `OptLabl (_loc,_,_) -> _loc
  | `Coercion (_loc,_,_,_) -> _loc
  | `CtFun (_loc,_,_) -> _loc
  | `Arrow (_loc,_,_) -> _loc
  | `ObjEnd _loc -> _loc
  | `Bind (_loc,_,_) -> _loc
  | `Subtype (_loc,_,_) -> _loc
  | `Functor (_loc,_,_,_) -> _loc
  | `With (_loc,_,_) -> _loc
  | `TyVrnOf (_loc,_,_) -> _loc
  | `TyAbstr (_loc,_,_,_) -> _loc
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
  | `Field (_loc,_,_) -> _loc
  | `CeFun (_loc,_,_) -> _loc
  | `ClassPath (_loc,_) -> _loc
  | `Nil _loc -> _loc
  | `Par (_loc,_) -> _loc
  | `Com (_loc,_,_) -> _loc
  | `TyRepr (_loc,_,_) -> _loc
  | `ArrayEmpty _loc -> _loc
  | `Int64 (_loc,_) -> _loc
  | `ModuleTypeOf (_loc,_) -> _loc
  | `ClassCon (_loc,_,_,_) -> _loc
  | `TyPolEnd (_loc,_) -> _loc
  | `To _loc -> _loc
  | `TyCol (_loc,_,_) -> _loc
  | `ObjPat (_loc,_,_) -> _loc
  | `CgVir (_loc,_,_,_) -> _loc
  | `Initializer (_loc,_) -> _loc
  | `TyColMut (_loc,_,_) -> _loc
  | `ModuleEq (_loc,_,_) -> _loc
  | `Lid (_loc,_) -> _loc
  | `OptLablS (_loc,_) -> _loc
  | `Record (_loc,_) -> _loc
  | `InheritAs (_loc,_,_,_) -> _loc
  | `Constraint (_loc,_,_) -> _loc
  | `Ant (_loc,_) -> _loc
  | `Some (_loc,_) -> _loc
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
  | `ClassConS (_loc,_,_) -> _loc
  | `Positive _loc -> _loc
  | `CrVal (_loc,_,_,_,_) -> _loc
  | `ModuleConstraint (_loc,_,_) -> _loc
  | `TyVrn (_loc,_) -> _loc
  | `Case (_loc,_,_) -> _loc
  | `OvrInstEmpty _loc -> _loc
  | `Exception (_loc,_) -> _loc
  | `CtCol (_loc,_,_) -> _loc
  | `RecordWith (_loc,_,_) -> _loc
  | `And (_loc,_,_) -> _loc
  | `SigEnd _loc -> _loc
  | `ObjPatEnd (_loc,_) -> _loc
  | `TyDcl (_loc,_,_,_,_) -> _loc
  | `Assert (_loc,_) -> _loc
  | `StructEnd _loc -> _loc
  | `Int32 (_loc,_) -> _loc
  | `PaRng (_loc,_,_) -> _loc
  | `RecModule (_loc,_) -> _loc
  | `LocalTypeFun (_loc,_,_) -> _loc
  | `Package_exp (_loc,_) -> _loc
  | `CaseWhen (_loc,_,_,_) -> _loc
  | `PrNil _loc -> _loc
  | `Int (_loc,_) -> _loc
  | `Negative _loc -> _loc
  | `Fun (_loc,_) -> _loc
  | `CeApp (_loc,_,_) -> _loc
  | `Eq (_loc,_,_) -> _loc
  | `LetTryInWith (_loc,_,_,_,_) -> _loc
  | `LetModule (_loc,_,_,_) -> _loc
  | `LetOpen (_loc,_,_) -> _loc
  | `PolyInf (_loc,_) -> _loc
  | `StringDot (_loc,_,_) -> _loc
  | `For (_loc,_,_,_,_,_) -> _loc
  | `CrVvr (_loc,_,_,_) -> _loc
  | `Recursive _loc -> _loc
  | `OptLablExpr (_loc,_,_,_) -> _loc
  | `SigInherit (_loc,_) -> _loc
  | `ModuleType (_loc,_,_) -> _loc
  | `Inherit (_loc,_,_) -> _loc
  | `MuNil _loc -> _loc
  | `None _loc -> _loc
  | `ObjTy (_loc,_,_) -> _loc
  | `Method (_loc,_,_,_) -> _loc
  | `Module (_loc,_,_) -> _loc
  | `PackageModule (_loc,_) -> _loc
  | `PolyEq (_loc,_) -> _loc
  | `Bar (_loc,_,_) -> _loc
  | `Open (_loc,_) -> _loc
  | `ObjTyEnd (_loc,_) -> _loc
  | `ViNil _loc -> _loc
  | `TyObjEnd (_loc,_) -> _loc
  | `Sum (_loc,_) -> _loc

let ghost = FanLoc.ghost

let (<+>) a b = (loc_of a) <+> (loc_of b)

let sem a b = let _loc = a <+> b in `Sem (_loc, a, b)

let com a b = let _loc = a <+> b in `Com (_loc, a, b)

let app a b = let _loc = a <+> b in `App (_loc, a, b)

let apply a b = let _loc = a <+> b in `Apply (_loc, a, b)

let sta a b = let _loc = a <+> b in `Sta (_loc, a, b)

let bar a b = let _loc = a <+> b in `Bar (_loc, a, b)

let anda a b = let _loc = a <+> b in `And (_loc, a, b)

let dot a b = let _loc = a <+> b in `Dot (_loc, a, b)

let par x = let _loc = loc_of x in `Par (_loc, x)

let seq a = let _loc = loc_of a in `Seq (_loc, a)

let arrow a b = let _loc = a <+> b in `Arrow (_loc, a, b)

let typing a b = let _loc = a <+> b in `Constraint (_loc, a, b)

let rec bar_of_list =
  function
  | [] -> failwithf "bar_of_list empty"
  | t::[] -> t
  | t::ts -> bar t (bar_of_list ts)

let rec and_of_list =
  function
  | [] -> failwithf "and_of_list empty"
  | t::[] -> t
  | t::ts -> anda t (and_of_list ts)

let rec sem_of_list =
  function
  | [] -> failwithf "sem_of_list empty"
  | t::[] -> t
  | t::ts -> sem t (sem_of_list ts)

let rec com_of_list =
  function
  | [] -> failwithf "com_of_list empty"
  | t::[] -> t
  | t::ts -> com t (com_of_list ts)

let rec sta_of_list =
  function
  | [] -> failwithf "sta_of_list empty"
  | t::[] -> t
  | t::ts -> sta t (sta_of_list ts)

let rec dot_of_list =
  function
  | [] -> failwithf "dot_of_list empty"
  | t::[] -> t
  | t::ts -> dot t (dot_of_list ts)

let rec appl_of_list x =
  match x with
  | [] -> failwithf "appl_of_list empty"
  | x::[] -> x
  | x::y::xs -> appl_of_list ((app x y) :: xs)

let rec list_of_and x acc =
  match x with
  | `And (_,x,y) -> list_of_and x (list_of_and y acc)
  | _ -> x :: acc

let rec list_of_com x acc =
  match x with
  | `Com (_,x,y) -> list_of_com x (list_of_com y acc)
  | _ -> x :: acc

let rec list_of_star x acc =
  match x with
  | `Sta (_,x,y) -> list_of_star x (list_of_star y acc)
  | _ -> x :: acc

let rec list_of_or x acc =
  match x with
  | `Bar (_,x,y) -> list_of_or x (list_of_or y acc)
  | _ -> x :: acc

let rec list_of_sem x acc =
  match x with
  | `Sem (_,x,y) -> list_of_sem x (list_of_sem y acc)
  | _ -> x :: acc

let rec list_of_dot x acc =
  match x with
  | `Dot (_,x,y) -> list_of_dot x (list_of_dot y acc)
  | x -> x :: acc

let rec list_of_app x acc =
  match x with
  | `App (_,t1,t2) -> list_of_app t1 (list_of_app t2 acc)
  | x -> x :: acc

let rec view_app acc =
  function | `App (_,f,a) -> view_app (a :: acc) f | f -> (f, acc)

let seq_sem ls = seq (sem_of_list ls)

let binds bs (e : exp) =
  match bs with
  | [] -> e
  | _ ->
      let binds = and_of_list bs in
      let _loc = binds <+> e in
      (`LetIn (_loc, (`ReNil _loc), binds, e) : Ast.exp )

let lid _loc n = `Lid (_loc, n)

let uid _loc n = `Uid (_loc, n)

let unit _loc = `Uid (_loc, "()")

let ep_of_cons _loc n ps = appl_of_list ((uid _loc n) :: ps)

let tuple_com_unit _loc =
  function | [] -> unit _loc | p::[] -> p | y -> `Par (_loc, (com_of_list y))

let tuple_com y =
  match y with
  | [] -> failwith "tuple_com empty"
  | x::[] -> x
  | x::_ -> let _loc = x <+> (List.last y) in `Par (_loc, (com_of_list y))

let tuple_sta y =
  match y with
  | [] -> failwith "tuple_sta empty"
  | x::[] -> x
  | x::_ -> let _loc = x <+> (List.last y) in `Par (_loc, (sta_of_list y))