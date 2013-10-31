open FAstN
open Astn_util
open Util
open Fid
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
let arrow_of_list f = Listf.reduce_right arrow f
let app_arrow lst acc = List.fold_right arrow lst acc
let (<+) (names : string list) (ty : ctyp) =
  List.fold_right
    (fun name  acc  ->
       (`Arrow ((`Quote (`Normal, (`Lid name))), acc) : FAstN.ctyp )) names
    ty
let (+>) (params : ctyp list) (base : ctyp) =
  List.fold_right arrow params base
let name_length_of_tydcl (x : typedecl) =
  (match x with
   | `TyDcl (`Lid name,tyvars,_,_) ->
       (name,
         ((match tyvars with
           | `None -> 0
           | `Some xs -> List.length @@ (Ast_basic.N.list_of_com xs []))))
   | tydcl ->
       failwithf "name_length_of_tydcl  %s \n" (ObjsN.dump_typedecl tydcl) : 
  (string* int) )
let gen_quantifiers1 ~arity  n =
  (((Listf.init arity
       (fun i  ->
          (Listf.init n) @@
            (fun j  ->
               (`Quote (`Normal, (`Lid (allx ~off:i j))) : FAstN.ctyp ))))
      |> List.concat)
     |> appl_of_list : ctyp )
let of_id_len ~off  ((id : ident),len) =
  appl_of_list ((id :>ctyp) ::
    (Listf.init len
       (fun i  -> (`Quote (`Normal, (`Lid (allx ~off i))) : FAstN.ctyp ))))
let of_name_len ~off  (name,len) =
  let id = lid name in of_id_len ~off (id, len)
let gen_ty_of_tydcl ~off  (tydcl : typedecl) =
  (tydcl |> name_length_of_tydcl) |> (of_name_len ~off)
let list_of_record (ty : name_ctyp) =
  (let (tys :name_ctyp list)= Ast_basic.N.list_of_sem ty [] in
   tys |>
     (List.map
        (function
         | `TyColMut (`Lid label,ty) -> { label; ty; is_mutable = true }
         | `TyCol (`Lid label,ty) -> { label; ty; is_mutable = false }
         | t0 -> failwithf "list_of_record %s" (ObjsN.dump_name_ctyp t0))) : 
  col list )
let gen_tuple_n ty n = (Listf.init n (fun _  -> ty)) |> tuple_sta
let repeat_arrow_n ty n = (Listf.init n (fun _  -> ty)) |> arrow_of_list
let result_id = ref 0
let mk_method_type ~number  ~prefix  (id,len) (k : destination) =
  (let prefix =
     List.map (fun s  -> Stringf.drop_while (fun c  -> c = '_') s) prefix in
   let app_src =
     app_arrow @@ (Listf.init number (fun _  -> of_id_len ~off:0 (id, len))) in
   let result_type: FAstN.ctyp =
     `Quote (`Normal, (`Lid ("result" ^ (string_of_int (!result_id))))) in
   let _ = incr result_id in
   let self_type: FAstN.ctyp = `Quote (`Normal, (`Lid "self_type")) in
   let (quant,dst) =
     match k with
     | Obj (Map ) -> (2, (of_id_len ~off:1 (id, len)))
     | Obj (Iter ) -> (1, result_type)
     | Obj (Fold ) -> (1, self_type)
     | Obj (Concrete c) -> (1, c)
     | Str_item  -> (1, result_type) in
   let params =
     (Listf.init len) @@
       (fun i  ->
          let app_src =
            app_arrow @@
              ((Listf.init number) @@
                 (fun _  ->
                    (`Quote (`Normal, (`Lid (allx ~off:0 i))) : FAstN.ctyp ))) in
          match k with
          | Obj u ->
              let dst =
                match u with
                | Map  ->
                    let x = allx ~off:1 i in
                    (`Quote (`Normal, (`Lid x)) : FAstN.ctyp )
                | Iter  -> result_type
                | Concrete c -> c
                | Fold  -> self_type in
              (arrow self_type) @@ (prefix <+ (app_src dst))
          | Str_item  -> prefix <+ (app_src result_type)) in
   let base = prefix <+ (app_src dst) in
   if len = 0
   then ((`TyPolEnd base), dst)
   else
     (let quantifiers = gen_quantifiers1 ~arity:quant len in
      ((`TyPol (quantifiers, (params +> base)) : FAstN.ctyp ), dst)) : 
  (ctyp* ctyp) )
let mk_method_type_of_name ~number  ~prefix  (name,len) (k : destination) =
  let id = lid name in mk_method_type ~number ~prefix (id, len) k
let mk_obj class_name base body =
  (`Class
     (`ClDeclS
        (`Negative, (`Lid class_name),
          (`ObjPat
             ((`Constraint
                 ((`Lid "self"), (`Quote (`Normal, (`Lid "self_type"))))),
               (`Sem ((`Inherit (`Negative, (`Lid base))), body)))))) : 
  FAstN.stru )
let is_recursive ty_dcl =
  match ty_dcl with
  | `TyDcl (`Lid name,_,ctyp,_) ->
      let obj =
        object (self : 'self_type)
          inherit  ObjsN.fold as super
          val mutable is_recursive = false
          method! ctyp =
            function
            | (`Lid i : FAstN.ctyp) when i = name ->
                (is_recursive <- true; self)
            | x -> if is_recursive then self else super#ctyp x
          method is_recursive = is_recursive
        end in
      (obj#type_info ctyp)#is_recursive
  | `And _ -> true
  | _ ->
      failwithf "is_recursive not type declartion: %s"
        (ObjsN.dump_typedecl ty_dcl)
let qualified_app_list (x : ctyp) =
  (match x with
   | (`App (_loc,_) : FAstN.ctyp) as x ->
       (match Ast_basic.N.list_of_app x [] with
        | (`Lid _loc : FAstN.ctyp)::_ -> None
        | (#ident' as i)::ys -> Some (i, ys)
        | _ -> None)
   | `Lid _|`Uid _ -> None
   | #ident' as i -> Some (i, [])
   | _ -> None : (ident* ctyp list) option )
let is_abstract (x : typedecl) =
  match x with | `TyAbstr _ -> true | _ -> false
let abstract_list (x : typedecl) =
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
      | u -> failwithf "view_variant %s" (ObjsN.dump_row_field u)) lst : 
  vbranch list )
let conversion_table: (string,string) Hashtbl.t = Hashtbl.create 50
let transform: full_id_transform -> vid -> exp =
  let open IdN in
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
         | `Lid x -> (`Send ((`Lid "self"), (`Lid (f x))) : FAstN.exp )
         | t ->
             let dest = map_to_string t in
             let src = ObjsN.dump_vid t in
             let () =
               if not @@ (Hashtbl.mem conversion_table src)
               then
                 (Hashtbl.add conversion_table src dest;
                  Format.eprintf "Warning:  %s ==>  %s ==> unknown\n" src
                    dest) in
             (`Send ((`Lid "self"), (`Lid (f dest))) : FAstN.exp ))
let basic_transform =
  function
  | `Pre pre -> (fun x  -> pre ^ x)
  | `Post post -> (fun x  -> x ^ post)
  | `Fun f -> f
let right_transform =
  function
  | #basic_id_transform as x ->
      let f = basic_transform x in (fun x  -> (`Lid (f x) : FAstN.exp ))
  | `Exp f -> f
let gen_tuple_abbrev ~arity  ~annot  ~destination  name e =
  let args: pat list =
    (Listf.init arity) @@
      (fun i  ->
         (`Alias ((`ClassPath name), (`Lid (x ~off:i 0))) : FAstN.pat )) in
  let exps = (Listf.init arity) @@ (fun i  -> (xid ~off:i 0 : FAstN.exp )) in
  let e = appl_of_list (e :: exps) in
  let pat = args |> tuple_com in
  match destination with
  | Obj (Map ) ->
      (`Case (pat, (`Coercion (e, (name :>ctyp), annot))) : FAstN.case )
  | _ -> (`Case (pat, (`Subtype (e, annot))) : FAstN.case )
