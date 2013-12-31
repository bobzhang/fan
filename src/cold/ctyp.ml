open Astfn
open Astn_util
open Util
type vrn =  
  | Sum
  | TyVrnEq
  | TyVrnSup
  | TyVrnInf
  | TyVrnInfSup
  | TyAbstr 
type col =  {
  label: string;
  is_mutable: bool;
  ty: ctyp} 
type ty_info = 
  {
  name_exp: exp;
  info_exp: exp;
  ep0: ep;
  id_ep: ep;
  id_eps: ep list;
  ty: ctyp} 
type vbranch = [ `variant of (string* ctyp list) | `abbrev of ident] 
type branch = [ `branch of (string* ctyp list)] 
type destination =  
  | Obj of kind
  | Str_item 
and kind =  
  | Fold
  | Iter
  | Map
  | Concrete of ctyp 
open Format
type warning_type =  
  | Abstract of string
  | Qualified of string 
let pp_print_warning_type: Format.formatter -> warning_type -> unit =
  fun fmt  ->
    function
    | Abstract _a0 ->
        Format.fprintf fmt "@[<1>(Abstract@ %a)@]" pp_print_string _a0
    | Qualified _a0 ->
        Format.fprintf fmt "@[<1>(Qualified@ %a)@]" pp_print_string _a0
type record_col =  {
  label: string;
  is_mutable: bool;
  info: ty_info} 
type record_info = record_col list 
type basic_id_transform =
  [ `Pre of string | `Post of string | `Fun of string -> string] 
type rhs_basic_id_transform = [ basic_id_transform | `Exp of string -> exp] 
type full_id_transform =
  [ basic_id_transform | `Idents of vid list -> vid | `Id of vid -> vid
  | `Last of string -> vid | `Obj of string -> string] 
let list_of_record (ty : name_ctyp) =
  (let (tys :name_ctyp list)= Ast_basic.N.list_of_sem ty [] in
   tys |>
     (List.map
        (fun (x : name_ctyp)  ->
           match x with
           | `RecCol (`Lid label,ty,(`Positive|`Negative as f)) ->
               {
                 label;
                 ty;
                 is_mutable =
                   (((function | `Positive -> true | _ -> false)) f)
               }
           | t0 -> failwith ("list_of_record" ^ (ObjsN.dump_name_ctyp t0)))) : 
  col list )
let mk_method_type ~number  ~prefix  (id,len) (k : destination) =
  (let a_var_lens =
     Listf.init len
       (fun i  -> (`Quote (`Normal, (`Lid (Id.allx ~off:0 i))) :>Astfn.ctyp)) in
   let b_var_lens =
     Listf.init len
       (fun i  -> (`Quote (`Normal, (`Lid (Id.allx ~off:1 i))) :>Astfn.ctyp)) in
   let a_names = appl_of_list ((id :>ctyp) :: a_var_lens) in
   let b_names = appl_of_list ((id :>ctyp) :: b_var_lens) in
   let prefix =
     List.map
       (fun s  ->
          Gensym.fresh
            ~prefix:(Stringf.drop_while (function | '_' -> true | _ -> false)
                       s) ()) prefix in
   let (<+) =
     List.fold_right
       (fun name  acc  ->
          (`Arrow ((`Quote (`Normal, (`Lid name))), (acc :>Astfn.ctyp)) :>
          Astfn.ctyp)) in
   let result_type =
     (`Quote (`Normal, (`Lid (Gensym.fresh ~prefix:"result" ()))) :>Astfn.ctyp) in
   let self_type = (`Quote (`Normal, (`Lid "self_type")) :>Astfn.ctyp) in
   let (quant,dst) =
     match k with
     | Obj (Map ) -> ((a_var_lens @ b_var_lens), b_names)
     | Obj (Iter ) -> (a_var_lens, result_type)
     | Obj (Fold ) -> (a_var_lens, self_type)
     | Obj (Concrete c) -> (a_var_lens, c)
     | Str_item  -> (a_var_lens, result_type) in
   let base =
     prefix <+
       (List.fold_right arrow (Listf.init number (const a_names)) dst) in
   if len = 0
   then ((`TyPolEnd base), dst)
   else
     (let quantifiers = appl_of_list quant in
      let params =
        Listf.init len
          (fun i  ->
             let ith_a = List.nth a_var_lens i in
             let ith_b = List.nth b_var_lens i in
             let app_src =
               List.fold_right arrow (Listf.init number (const ith_a)) in
             match k with
             | Obj u ->
                 let dst =
                   match u with
                   | Map  -> ith_b
                   | Iter  -> result_type
                   | Concrete c -> c
                   | Fold  -> self_type in
                 (arrow self_type) @@ (prefix <+ (app_src dst))
             | Str_item  -> prefix <+ (app_src result_type)) in
      ((`TyPol
          ((quantifiers :>Astfn.ctyp),
            (List.fold_right arrow params base :>Astfn.ctyp)) :>Astfn.ctyp),
        dst)) : (ctyp* ctyp) )
let mk_method_type_of_name ~number  ~prefix  (name,len) (k : destination) =
  let id = lid name in mk_method_type ~number ~prefix (id, len) k
let mk_obj class_name base body =
  (`Class
     (`ClDeclS
        (`Negative, (`Lid class_name),
          (`ObjPat
             ((`Constraint
                 ((`Lid "self"), (`Quote (`Normal, (`Lid "self_type"))))),
               (`Sem
                  ((`Inherit (`Negative, (`Lid base))),
                    (body :>Astfn.clfield))))))) :>Astfn.stru)
let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (`Lid name,_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  ObjsN.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | (`Lid i : Astfn.ctyp) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#type_info ctyp)#is_recursive
  | `And _ -> true
  | _ ->
      failwithf "is_recursive not type declartion: %s"
        (ObjsN.dump_decl ty_dcl)
let qualified_app_list (x : ctyp) =
  (match x with
   | (`App (_loc,_) : Astfn.ctyp) as x ->
       (match Ast_basic.N.list_of_app x [] with
        | (`Lid _loc : Astfn.ctyp)::_ -> None
        | (#ident' as i)::ys -> Some (i, ys)
        | _ -> None)
   | `Lid _|`Uid _ -> None
   | #ident' as i -> Some (i, [])
   | _ -> None : (ident* ctyp list) option )
let is_abstract (x : decl) = match x with | `TyAbstr _ -> true | _ -> false
let abstract_list (x : decl) =
  match x with
  | `TyAbstr (_,lst,_) ->
      (match lst with
       | `None -> Some 0
       | `Some xs -> Some (List.length @@ (Ast_basic.N.list_of_com xs [])))
  | _ -> None
let reduce_data_ctors (ty : or_ctyp) (init : 'a) ~compose 
  (f : string -> ctyp list -> 'e) =
  let branches = Ast_basic.N.list_of_bar ty [] in
  List.fold_left
    (fun acc  x  ->
       match (x : or_ctyp ) with
       | `Of (`Uid cons,tys) ->
           compose (f cons (Ast_basic.N.list_of_star tys [])) acc
       | `Uid cons -> compose (f cons []) acc
       | t -> failwithf "reduce_data_ctors: %s" (ObjsN.dump_or_ctyp t)) init
    branches
let view_sum (t : or_ctyp) =
  let bs = Ast_basic.N.list_of_bar t [] in
  List.map
    (function
     | `Uid cons -> `branch (cons, [])
     | `Of (`Uid cons,t) -> `branch (cons, (Ast_basic.N.list_of_star t []))
     | _ -> assert false) bs
let view_variant (t : row_field) =
  (let lst = Ast_basic.N.list_of_bar t [] in
   List.map
     (function
      | `TyVrnOf (`C cons,`Par t) ->
          `variant (cons, (Ast_basic.N.list_of_star t []))
      | `TyVrnOf (`C cons,t) -> `variant (cons, [t])
      | `TyVrn `C cons -> `variant (cons, [])
      | `Ctyp (#ident' as i) -> `abbrev i
      | u -> failwithf "%s %s" "view_variant" (ObjsN.dump_row_field u)) lst : 
  vbranch list )
let conversion_table: (string,string) Hashtbl.t = Hashtbl.create 50
let transform: full_id_transform -> vid -> exp =
  let open Idn_util in
    function
    | `Pre pre -> (fun x  -> (ident_map (fun x  -> pre ^ x) x : exp ))
    | `Post post -> (fun x  -> (ident_map (fun x  -> x ^ post) x : exp ))
    | `Fun f -> (fun x  -> ident_map f x)
    | `Last f -> (fun x  -> (ident_map_of_ident f x : vid  :>exp))
    | `Id f -> (fun x  -> (f x : vid  :>exp))
    | `Idents f ->
        (fun x  -> (f (Ast_basic.N.list_of_dot x []) : vid  :>exp))
    | `Obj f ->
        (function
         | `Lid x -> (`Send ((`Lid "self"), (`Lid (f x))) :>Astfn.exp)
         | t ->
             let dest = map_to_string t in
             let src = ObjsN.dump_vid t in
             let () =
               if not @@ (Hashtbl.mem conversion_table src)
               then
                 (Hashtbl.add conversion_table src dest;
                  Format.eprintf "Warning:  %s ==>  %s ==> unknown\n" src
                    dest) in
             (`Send ((`Lid "self"), (`Lid (f dest))) :>Astfn.exp))
let basic_transform =
  function
  | `Pre pre -> (fun x  -> pre ^ x)
  | `Post post -> (fun x  -> x ^ post)
  | `Fun f -> f
let right_transform =
  function
  | #basic_id_transform as x ->
      let f = basic_transform x in (fun x  -> (`Lid (f x) :>Astfn.exp))
  | `Exp f -> f
let gen_tuple_abbrev ~arity  ~annot  ~destination  name e =
  let args: pat list =
    (Listf.init arity) @@
      (fun i  ->
         (`Alias ((`ClassPath (name :>Astfn.ident)), (`Lid (Id.x ~off:i 0))) :>
         Astfn.pat)) in
  let exps =
    (Listf.init arity) @@
      (fun i  -> ((Id.xid ~off:i 0 :>Astfn.vid) :>Astfn.exp)) in
  let e = appl_of_list (e :: exps) in
  let pat = args |> tuple_com in
  match destination with
  | Obj (Map ) ->
      (`Case
         ((pat :>Astfn.pat),
           (`Coercion
              ((e :>Astfn.exp), ((name :>ctyp) :>Astfn.ctyp),
                (annot :>Astfn.ctyp)))) :>Astfn.case)
  | _ ->
      (`Case
         ((pat :>Astfn.pat),
           (`Subtype ((e :>Astfn.exp), (annot :>Astfn.ctyp)))) :>Astfn.case)
