
open Astfn
open Astn_util
open Util
open Sigs_util



(* let () = *)
(*   ignore eq_u  *)

(************************************) 
(* Eq generator                     *)
(************************************) 
(* Unused
   depends on a small runtime [eq_int] and [Tokenf.eq_ant] *)
let mk_variant _cons : Ctyp.ty_info list   -> exp  =
  function 
  | [] ->  %exp-{true}
  | ls -> 
      Listf.reduce_left_with
        ~compose:(fun x y -> %exp-{ $x && $y}  )
        ~project:(fun (x:Ctyp.ty_info) -> x.info_exp) ls
let mk_record cols = 
    cols |> List.map (fun  (x:Ctyp.record_col) -> x.info)
         |> mk_variant None


(** it can exist without prefix only when you do not need
    any runtime -- 
 *)             
let gen_eqobj =
  Derive_obj.register {
  arity=2;
  names=[];
  plugin_name= "OEq";
  mk_record = Some mk_record;
  mk_variant = Some mk_variant;
  default = Some (Atom %exp-{false});
  excludes = [];
  kind = Iter;
  base = "eqbase";
  class_name = "eq";
 }
  (*       List.iter Typehook.register *)
  (*   [ ("OEq", some gen_eqobj ) ] *)

  (* Derive_obj.mk ~kind:Iter  ~mk_record *)
  (*    ~base:"eqbase" ~class_name:"eq" *)
  (*    ~mk_variant:mk_variant *)
  (*    ~arity:2 ~default:%exp-{false} () ;; *)

let some f  = fun x -> Some (f x)


let _ = begin

  Derive_stru.register {
    id = `Pre "eq_";
    arity = 2;
    mk_record = Some mk_record;
    mk_variant = Some mk_variant;
    default = Some (Atom %exp-{false});
    plugin_name = "Eq";
    excludes = [];
    names = [];
    annot = None;
    builtin_tbl =
     [
      (%ctyp-{int},
       %exp-{ (Pervasives.(=) : int -> int -> bool )});
      (%ctyp-{char},
       %exp-{ (Pervasives.(=) : char -> char -> bool )});
      (%ctyp-{bool},
       %exp-{ (Pervasives.(=) : bool -> bool -> bool )}
      );
      (%ctyp-{string},
       %exp-{ (Pervasives.(=) : string -> string -> bool )}
      );
      (%ctyp-{nativeint},
       %exp-{ (Pervasives.(=) : nativeint -> nativeint -> bool )}
      );
      (%ctyp-{unit},
       %exp-{ (Pervasives.(=) : unit -> unit -> bool )}
      );
      (%ctyp-{int32},
       %exp-{ (Pervasives.(=) : int32 -> int32 -> bool )}
      );
      (%ctyp-{list}, %exp-{let rec f = (function p xs ys -> 
        match (xs ,ys) with 
        |  ([],[]) -> true 
        |  (x::xs, y::ys) ->
            p x y && f p xs ys 
        |  _  -> false) in f }
      );
      (%ctyp-{option}, %exp-{ function p x y -> 
        match (x,y) with 
        | (None, None) -> true
        | (Some x , Some y) -> p x y 
        | _ -> false}
      );
      (%ctyp-{array}, %exp-{
       function  f t1 t2  -> 
         let len = Array.length t1 in
         if Array.length t2 <> len then false
         else 
           let rec loop i =
             if i < 0
             then true
             else if f t1.(i) t2.(i)
             then loop (i - 1)
             else false in
         loop (len - 1)
      })
      (* tuples, arrays, lists, sets, hash tables, queues, stacks, data streams *)
    ]
  };
   
end



(************************************) 
(* Fold generator                   *)
(************************************)     


(* [Fold2] unused *)
let (* (gen_fold,gen_fold2) *)() = 
  let mk_variant _cons params = 
    params
    |> List.map (fun (x:Ctyp.ty_info)  -> x.info_exp)
    |> (function
        | [] -> %exp-{self}
        | ls ->
            Listf.reduce_right (fun v acc -> %exp-{ let self = $v in $acc }) ls ) in
  let mk_record cols =
    cols |> List.map (fun  (x:Ctyp.record_col) -> x.info  )
         |> mk_variant None in
  begin 
    Derive_obj.register {
    kind = Fold;
    mk_record = Some mk_record;
    mk_variant = Some mk_variant;
    default = None;
    excludes = [];
    base = "foldbase";
    class_name = "fold";
    plugin_name = "FOld" ;
    arity = 1;
    names = [];
    };
    Derive_obj.register {
    kind = Fold;
    mk_record = Some mk_record;
    mk_variant = Some mk_variant;
    default = Some (Atom %exp-{invalid_arg "fold2 failure" });
    excludes = [];
    base = "foldbase";
    class_name = "fold";
    plugin_name = "FOld2" ;
    arity = 2;
    names = []; 
    }
  end
  (* (Derive_obj.mk ~kind:Fold ~mk_record *)
  (*    ~base:"foldbase" ~class_name:"fold" ~mk_variant (), *)
  (*  Derive_obj.mk ~kind:Fold  ~mk_record *)
  (*    ~base:"foldbase2" ~class_name:"fold2" *)
  (*    ~mk_variant *)
  (*    ~arity:2 ~default:%exp-{invalid_arg "fold2 failure" } () ) *)

(* let _ =  *)
(*   begin *)
(*     List.iter Typehook.register *)
(*       [("Fold",some gen_fold); ("Fold2",some gen_fold2);] *)
(*   end *)


(************************************) 
(* Map generator                   *)
(************************************)     
(* [Map2] unused, [ant] was dispatched as
   [self#tokenf_ant] [self#|Tokenf.ant|]? which was later dispatched to
   [self#unkown] -- or more compatible way
   dumping ast level do the transformation [__Tokenf_ant_]
 *)
let () = 
  let mk_variant cons params =
    let result =
      match cons with
      | Some cons -> 
          appl_of_list
            (of_str cons ::
             (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0)) )
      | None ->   params |> List.map (fun (x:Ctyp.ty_info) ->x.ep0) |> tuple_com
    in 
    List.fold_right
      (fun (x:Ctyp.ty_info) res ->
              %exp-{let $pat{x.ep0} = ${x.info_exp} in $res })  params (result:>exp) in
  let mk_record cols =
    (* (->label,info.exp0) *)
    let result = 
    cols
    |> List.map
        (fun  (x:Ctyp.record_col) ->
          match x with
          | {label; info={ep0;_ }  ; _ }  ->
            (label,(ep0:>exp))  )
    |> Expn_util.mk_record   in
    List.fold_right
      (fun ({info={info_exp=exp;ep0;_};_} : Ctyp.record_col) res ->
        %exp-{let $pat{ep0} = $exp in $res }) cols result in
  begin 
    Derive_obj.register
      {
       kind = Map ;
       mk_record = Some mk_record;
       mk_variant = Some mk_variant;
       base = "mapbase";
       class_name = "map";
       default = None ;
       excludes = [];
       names = [];
       arity = 1;
       plugin_name = "Map";
     };
    Derive_obj.register
    {
     kind = Map ;
     mk_record = Some mk_record;
     mk_variant = Some mk_variant;
     base = "mapbase";
     class_name = "map2";
     default = None ;
     excludes = [];
     names = [];
     arity = 2;
     plugin_name = "Map2";
   }
  end
(*   (Derive_obj.mk ~kind:Map  ~mk_record *)
(*      ~base:"mapbase" ~class_name:"map" *)
(*      ~mk_variant  (), *)
(*    Derive_obj.mk ~kind:Map  ~mk_record *)
(*      ~base:"mapbase2" ~class_name:"map2" ~mk_variant  *)
(*      ~arity:2 ~default: %exp-{  invalid_arg "map2 failure" } ());; *)

(* begin *)
(*   [("Map",some gen_map); *)
(*    ("Map2",some gen_map2);] *)
(*   |> List.iter Typehook.register; *)
(* end;; *)

(************************************) 
(* Strip generator                   *)
(************************************)     
(* [Astf] transformed to [Astfn].
   Types [loc] and [ant] will not be processed
 *)

let mk_variant cons params =
    let params' =
      List.filter
        (function (x:Ctyp.ty_info) -> x.ty <> `Lid "loc")
        params in
    
      match cons with
      | Some cons -> 
          let result =
            (appl_of_list
               (of_str cons  :: (params' |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) )) :> exp) in
                List.fold_right
            (fun (x:Ctyp.ty_info) res ->
              match x.ty with
              | `Lid("int" |"char"| "string" | "int32"| "unit" | "nativeint" |"bool"|"loc")
              | %ctyp-{Tokenf.quot}(** BOOTSTRAPING, associated with module [Tokenf] *)
              | %ctyp-{Tokenf.ant} -> res
              | _ ->
                  %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params' result 
      | None ->   assert false in 
Derive_stru.register {
    id =  `Same;
    arity = 1 ;
    mk_record = None;
    mk_variant = Some mk_variant;
    default = None;
    plugin_name = "Strip";
    excludes = ["loc";"ant";"quot"];
    names = [];
    builtin_tbl =  [];
    annot = Some (fun  x ->
      (* BOOTSTRAPING, associated with module [Astf], [Astfn] *)
      (%ctyp-{ Astf.$lid:x -> Astfn.$lid:x }, %ctyp-{Astfn.$lid:x}));
  }


(*******************************)
(* Fill generator              *) 
(*******************************)
(* [fill_loc] types [loc] and [ant]
   are not processed *)
let gen_fill =
  let mk_variant cons params =
    match cons with 
    |Some cons ->
        let result =
        (appl_of_list
           (of_str cons ::
            %ep-{loc} ::
            (params |> List.map (fun (x:Ctyp.ty_info) -> x.ep0) )) :> exp)  in 
      List.fold_right
      (fun (x:Ctyp.ty_info) res ->
        match x.ty with
        | `Lid("int" |"char"| "string" | "int32" |"unit"| "nativeint"|"bool" |"loc"|"ant")
        | %ctyp-{Tokenf.ant} | %ctyp-{Tokenf.quot} -> res
        | _ ->
            %exp-{let $pat{x.ep0} = ${x.info_exp} in $res }) params result
    | None -> assert false in


  Derive_stru.register {
    id = `Same;
    mk_record = None;
    arity  =  1;
    default = None; 
    mk_variant = Some mk_variant; 
    names = ["loc"];
    annot= Some (fun x ->
      (%ctyp-{ Locf.t -> Astfn.$lid:x -> Astf.$lid:x },
       %ctyp-{Astf.$lid:x} ));
    plugin_name = "Fill";
    excludes = ["loc"; "ant";"quot"];
    builtin_tbl = []
   }
    ;;

  
(*******************************)
(* Meta generator              *) 
(*******************************)
(* only object is used here *)
let mk_variant cons params = 
  let len = List.length params in 
  match cons with
  | Some cons when Stringf.ends_with cons "Ant" -> 
      (Id_epn.of_vstr_number "Ant" len :> exp)
  | Some cons -> 
    let params =
      params
         |> List.map (fun  (x:Ctyp.ty_info) -> x.info_exp ) in
    let a = (Expn_util.mee_of_str cons) in
    begin match params with
    | [] -> a
    | _ -> 
        Expn_util.mee_app a  (Expn_util.mk_tuple_ee params )
    end
  | None ->
        params
        |> List.map (fun (x:Ctyp.ty_info) -> x.info_exp)
        |> Expn_util.mk_tuple_ee 

      
      (* Fusing ?*)
let mk_record cols =
  cols
    |> List.map (fun  (x : Ctyp.record_col)    -> (x.label, x.info.info_exp))
    |> Expn_util.mk_record_ee 

let mk_tuple params =
  params
    |> List.map (fun (x:Ctyp.ty_info) -> x.info_exp)
    |> Expn_util.mk_tuple_ee 


(* Functor parameterize [meta_loc] *)
let gen_meta =
  begin
    Derive_stru.register  {
    id = (`Pre "meta_"); (* can be improved by sharing the code *)
    names = ["_loc"];
    mk_record = Some mk_record;
    mk_variant = Some  mk_variant;
    builtin_tbl = [
     (%ctyp-{int}, %exp-{fun _loc (i:int) -> %ep{$int':i} });
     (%ctyp-{int32}, %exp-{fun _loc (i:int32) -> %ep{$int32':i} });
     (%ctyp-{int64}, %exp-{fun _loc (i:int64) -> %ep{$int64':i} });
     (%ctyp-{nativeint}, %exp-{fun _loc (i:nativeint) -> %ep{$nativeint':i} });
     (%ctyp-{float}, %exp-{fun _loc (i:float) -> %ep{$flo':i} });
     (%ctyp-{string}, %exp-{fun _loc (i:string) -> %ep{$str':i} });
     (%ctyp-{char}, %exp-{fun _loc (i:char) -> %ep{$chr':i} });
     (%ctyp-{unit}, %exp-{fun _loc (i:unit) -> %ep{()} });
     (%ctyp-{bool}, %exp-{fun _loc (i:bool) -> %ep{$bool':i} });
    (%ctyp-{list},%exp-{
     let mklist loc =
       let rec loop top =  function
         | [] -> %exp'@loc{ [] }
         | e1 :: el ->
             let _loc =  (* BOOTSTRAPING *)
               if top then loc else Locf.merge (Ast_gen.loc_of e1) loc in
             %ep'{ $e1 :: ${loop false el} }
       in loop true in
     let meta_list mf_a _loc  ls =
       mklist _loc (List.map (fun x -> mf_a _loc x ) ls ) in 
     meta_list
   });
    (%ctyp-{option},
     %exp-{fun mf_a _loc -> function
       | None -> %ep-{None}
       | Some x -> %ep-{Some ${mf_a _loc x}}
         })
    ];
    plugin_name = "Meta";
    arity = 1;
    annot = None;
    default = None;
    excludes = ["loc"; "ant"; "quot"];
  } ;

  Derive_obj.register
      {
       kind = Concrete %ctyp-{Astf.ep};
       mk_record = Some mk_record;
       mk_variant = Some mk_variant;
       base = "primitive";
       class_name = "meta";
       names = ["_loc"];
       default = None;
       excludes = ["loc";"ant";"quot"];
       arity = 1;
       plugin_name = "MetaObj"
  }
  end;;
  
  



let () =
  Derive_obj.register {
  kind =   Concrete %ctyp-{unit};
  mk_record = Some Gen_print.mk_record;
  mk_variant = Some Gen_print.mk_variant;
  base = "printbase";
  class_name = "print";
  arity = 1 ;
  names = ["fmt"];
  plugin_name = "Oprint";
  default = None;
  excludes = [];
}
(*   Derive_obj.mk ~kind:(Concrete %ctyp-{unit}) (\* ~mk_tuple:mk_tuple_print *\) *)
(*     ~base:"printbase" ~class_name:"print" *)
(*     ~names:["fmt"]  ~mk_record:Gen_print.mk_record *)
(*     ~mk_variant:Gen_print.mk_variant ();; *)

(* let () = *)
(*   begin *)
    
(*     Derive_stru.register Gen_print.default; *)
(*     [ ("OPrint",some gen_print_obj)] |> List.iter Typehook.register; *)
(*   end *)






(*******************************)
(* Iter generator              *) 
(*******************************)
let mk_variant_iter _cons params :exp = 
  match params with
  | [] -> (unit:>exp)
  | _ -> 
      let lst = params
        |> List.map (fun (x:Ctyp.ty_info) -> 
            %exp-{ ${x.name_exp} ${x.id_ep} }) in
        seq_sem lst 

let mk_record_iter cols = 
  cols
  |> List.map
    (fun (x:Ctyp.record_col) ->
      %exp-{ ${x.info.name_exp} ${x.info.id_ep} })
  |> seq_sem


let () =
  Derive_obj.register
    {
     kind = Iter;
     base = "iterbase";
     class_name = "iter";
     names = [];
     mk_record = Some mk_record_iter;
     mk_variant = Some mk_variant_iter;
     arity = 1;
     default = None;
     excludes = [];
     plugin_name = "OIter" 
   }
(* let gen_iter = *)
(*   Derive_obj.mk ~kind:Iter *)
(*     ~base:"iterbase" *)
(*     ~class_name:"iter" *)
(*     ~names:[]  *)
(*     ~mk_record:mk_record_iter *)
(*     ~mk_variant:mk_variant_iter *)
(*     ();; *)

(* ("OIter",some gen_iter) |> Typehook.register;; *)

(*******************************)
(* [Locof] generator           *) 
(*******************************)
let generate (mtyps:mtyps) : stru =
  let tbl = Hashtbl.create 30 in
  let aux (_,ty) =
    match (ty:decl) with
    |`TyDcl(_,_,`TyEq(_,`PolyEq t),_) ->
      let branches = Ctyp.view_variant t in
      List.iter
        (function
          |`variant (s,ls) ->
              (let arity = List.length ls in
              let try v = Hashtbl.find tbl s in
              if v <> arity then
                failwithf "%s has diffireent arities" s
              with 
                Not_found -> Hashtbl.add tbl s arity)
          | _ -> ()) branches
    | _ -> failwithf  "generate mtyps %s" (Astfn_print.dump_decl ty)  in   
  let _ =
    List.iter
      (function
        |Mutual tys -> List.iter aux tys
        |Single t -> aux t) mtyps in
  let case = Hashtbl.fold
    (fun key arity acc ->
      if arity= 1 then
        let case = %case-{ $vrn:key _loc -> _loc } in
        match acc with
        |None ->   Some case 
        |Some acc ->
          Some (%case-{ $case | $acc }) 
      else if arity > 1 then 
        let pats =
          (%pat-{ _loc} :: Listf.init (arity - 1) (const %pat-{_}) ) in
        let case =
          %case-{ $vrn:key $pat{(tuple_com pats)} -> _loc } in
        match acc with
        |None -> Some case
        |Some acc -> Some(%case-{ $case | $acc })
      else failwithf "arity=0 key:%s" key
    ) tbl None  in
  match case with
  |Some case ->   
    %stru-{ let loc_of  = function | $case }
  |None -> failwithf "PluginsN.generate null case" ;;


Typehook.register
    ~filter:(fun s -> not (List.mem s ["loc"])) ("GenLoc",some generate);;



(********************************************)
(* DynAst generator                         *)
(********************************************)

let generate (mtyps:mtyps) : stru =
  let tys :  string list =
    Listf.concat_map
      (fun x ->
        match x with
        |Mutual tys -> List.map (fun ((x,_):named_type) -> x ) tys
        |Single (x,_) -> [x] ) mtyps in
  let decl =
    let x  =
      tys
      |> List.map (fun x -> uid  @@ String.capitalize x)
      |> bar_of_list in
    %stru-{ type 'a t = | $x  }in
  let to_string =
    let case =
      bar_of_list
        (List.map
           (fun x ->
             let u = String.capitalize x in
             %case-{ $uid:u -> $str:x }) tys) in 
    %stru-{ let to_string = function | $case  } (* BOOTSTRAPING [Dyn_tag.to_string]*)in
 let  of_string =
   let case =
     tys
     |> List.map
         (fun x -> let u = String.capitalize x in
         %case-{$str:x -> $uid:u })
     |> bar_of_list in
   %stru-{let of_string = function
     | $case  | _ -> failwith (__MODULE__ ^ "." ^ __BIND__  )}
   in
   let tags  =
   List.map
     (fun x->
       let u = String.capitalize x in
       %stru-{let $lid:x :  $lid:x t = $uid:u }) tys  in
       sem_of_list (decl::to_string::of_string::tags) ;;
  
Typehook.register
  ~filter:(fun s -> not @@ List.mem s ["loc";"ant";"quot"]) ("DynAst",some generate);;


(********************************************)
(* Map wrapper                              *)
(********************************************)
let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{
    let $lid{"map_"^f} f = object
      inherit map as super
      method! $lid:f x = f (super#$lid:f x)
    end } in
  stru_from_ty ~f:aux mtyps;;  

Typehook.register
  ~filter:(fun _ -> true) ("MapWrapper",some generate);;




let generate (mtyps:mtyps) : stru =
  let aux (f:string) : stru  =
    %stru-{  (** BOOTSTRAPING, associated with module [Formatf] *)
    let $lid{"dump_"^f}  = Formatf.to_string $lid{"pp_print_"^f}(* dump#$lid:f  *) } in
    (* sem *)
    (*   %stru-{let dump = new print} *)
      (stru_from_ty ~f:aux mtyps);;  


Typehook.register
  ~filter:(fun s -> not (List.mem s ["loc";"ant";"quot"]))
      ("PrintWrapper",some generate);; (* double registration should complain*)


(********************************************)
(* Type wrapper                             *)
(********************************************)

(* remove the loc field *)
let generate (mtyps:mtyps) : stru option = 
  let f (name,ty) =
    if  name <> "ant" then 
     let obj = Astfn_map.map_row_field @@ function
       | %row_field-{ $vrn:x of loc } -> %row_field-{ $vrn:x }
       | %row_field-{ $vrn:x of (loc * $y ) }->
           (match y with
           | %ctyp-{ $_ * $_ } -> %row_field-{ $vrn:x of $par:y }
           | _ -> %row_field-{ $vrn:x of $y })
       | x -> x in 
     obj#decl ty
  else ty  in
  (fun x ->  stru_from_mtyps ~f x) mtyps;;

Typehook.register ~filter:(fun _ -> true ) ("LocType", generate);;

(* local variables: *)
(* compile-command: "cd .. && pmake main_annot/plugins.cmo" *)
(* end: *)
