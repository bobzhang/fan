
open LibUtil

{:import|
Format:
  pp_print_string
  bprintf
  ;
|};;

(** [parse_quotation_result parse_function loc position_tag quotation quotation_result]
  It's a parser wrapper, this function handles the error reporting for you. *)
(* val parse_quotation_result: *)
(*     (FLoc.t -> string -> 'a) -> FLoc.t -> Ftoken.quotation -> string -> string -> 'a *)

(*********************************)
(* name table                    *)        
(*********************************)
        
(* [only absolute] domains can be stored *)  
let paths :  Ftoken.domains list ref  =
  ref [ `Absolute ["Fan";"Lang"];
        `Absolute ["Fan";"Lang";"Meta"];
        `Absolute ["Fan";"Lang";"Filter"]]

let concat_domain = function
  |(`Absolute xs,`Sub ys) -> `Absolute (xs@ys)
  | _ -> invalid_arg "concat_domain"



(** [names_tbl] is used to manage the namespace and names *)
let names_tbl : (Ftoken.domains,SSet.t) Hashtbl.t =
  Hashtbl.create 30 

(**  when no qualified path is given , it uses [Sub []] *)
let resolve_name (n:Ftoken.name) =
  match n with
  | ((`Sub _ as x) , v) ->
      begin 
        match List.find_opt
            (fun path  ->
              match Hashtbl.find_opt names_tbl @@ concat_domain (path,x)
              with
              | None -> false
              | Some set -> SSet.mem v set ) !paths
        with
        | None ->  None
        | Some r ->
              Some (concat_domain (r,x),v)
      end
  | x -> Some x (* absolute *)

(** expand function *)
type 'a expand_fun  = FLoc.t ->  string option -> string -> 'a
  
module ExpKey = FDyn.Pack(struct  type 'a t  = unit end)

module ExpFun = FDyn.Pack(struct  type 'a t  = 'a expand_fun  end)

let current_loc_name = ref None  

let stack = Stack.create ()
  
let current_quot () =
  try Stack.pop stack
  with Stack.Empty -> failwith "it's not in a quotation context"
    
let dump_file = ref None


(* This table is used to resolve the full qualified name first,
   After the full qualified name, is resolved, it is pared with
   tag, to finally resolve expander. So there are two kinds of errors,
   the first one is the full qualified name can not be found, the second
   is the even a fully qualified name can be found, it does not match
   the postition tag.

   Here key should be [absolute path] instead
 *)


  
type key = (Ftoken.name * ExpKey.pack)

module QMap =MapMake (struct type t =key  let compare = compare end)

(** [map] is used
    [map]  and [default] is used to help resolve default case {[ {||} ]}
    for example, you can register [Fan.Meta.exp] with [exp] and [stru] positions,
    but when you call {[ with {exp:pat} ]} here, first the name of pat will be resolved
    to be [Fan.Meta.pat], then when you parse {[ {| |} ]} in a position, because its
    name is "", so it will first turn to help from [map], then to default *)

let map = ref SMap.empty
let update (pos,(str:Ftoken.name)) =
  map := SMap.add pos str !map

let default_at_pos pos str =  update (pos,str)
(* create a table mapping from  (string_of_tag tag) to default
   quotation expander intentionaly make its value a string to
   be more flexibile to incorporating more tags in the future *)  
(* let fan_default = (`Absolute ["Fan"],"") *)
let default : Ftoken.name option ref = ref None
let set_default s =  default := Some s
let clear_map () =  map := SMap.empty
let clear_default () = default:= None
    
  
(**   The output should be an [`Absolute name] *)
let expander_name  ~pos (name:Ftoken.name) =
  match name with
  | (`Sub [],"") ->
      try Some (SMap.find  pos !map)
      with Not_found -> !default
  | (`Sub _ ,_) -> resolve_name  name
  | (`Absolute _,_) -> Some name  
  
let expanders_table =ref QMap.empty

let add ((domain,n) as name) (tag : 'a FDyn.tag ) (f:  'a expand_fun) =
  let (k,v) = ((name, ExpKey.pack tag ()), ExpFun.pack tag f) in
  let s  =
    try  Hashtbl.find names_tbl domain with
      Not_found -> SSet.empty in
  begin
    Hashtbl.replace names_tbl domain (SSet.add  n s);
    expanders_table := QMap.add k v !expanders_table
  end


(**
  [tag] is used to help find the expander,
  is passed by the parser function at parsing time
 *)
let expand (x:Ftoken.quot) (tag:'a FDyn.tag) : 'a =
  let pos_tag = FDyn.string_of_tag tag in
  let name = x.name in
  (* resolve name when expansion*)
  (* The table is indexed by [quotation name] and [tag] *)
  match expander_name ~pos:pos_tag name with
  | None ->
      FLoc.failf x.loc "DDSL `%s' not found" @@ Ftoken.string_of_name name
  | Some absolute_name ->
      begin 
        let pack =
          try QMap.find (absolute_name, ExpKey.pack tag ()) !expanders_table with
            Not_found -> FLoc.failf x.loc "DDSL expander `%s' at position `%s' not found"
                (Ftoken.string_of_name name) pos_tag  in
        let expander = ExpFun.unpack tag pack in
        let loc = Location_util.join (FLoc.move `start x.shift x.loc) in
        let content =
          String.sub x.content x.shift (String.length x.content - x.retract - x.shift) in 
        expander loc x.meta content
        (* FIXME: control the stack of quotation explosion *)
      end

        
(* [exp_filter] needs an coercion , we can not finish in one step
   by mexp, since 1. the type has to be relaxed not only to ep, since
   [parse_pat] or [parse_exp] could introduce any type.
   2. the context is a bit missing when expand the antiquotation..
   it expands differently when in exp or pat... 
 *)
let add_quotation ~exp_filter ~pat_filter  ~mexp ~mpat name entry  =
  let entry_eoi = Fgram.eoi_entry entry in
  let expand_exp loc loc_name_opt s =
    Ref.protect2 (FConfig.antiquotations,true) (current_loc_name, loc_name_opt)
      (fun _ ->
        Fgram.parse_string entry_eoi ~loc s |> mexp loc |> exp_filter) in
  let expand_stru loc loc_name_opt s =
    let exp_ast = expand_exp loc loc_name_opt s in
    `StExp(loc,exp_ast) in
  let expand_pat _loc loc_name_opt s =
    Ref.protect FConfig.antiquotations true begin fun _ ->
      let ast = Fgram.parse_string entry_eoi ~loc:_loc s in
      let meta_ast = mpat _loc ast in
      let exp_ast = pat_filter meta_ast in
      (* BOOTSTRAPPING *)
      let rec subst_first_loc name (x : FAst.pat) : FAst.pat =
        match x with 
        | `App(loc, `Vrn (_,u), (`Par (_, `Com (_,_,rest)))) ->
            `App(loc, `Vrn(loc,u),(`Par (loc,`Com(loc,`Lid (_loc,name),rest))))
        | `App(_loc,`Vrn(_,u),`Any _) ->
            `App(_loc, `Vrn(_loc,u), `Lid(_loc,name))
        | `App(_loc,a,b) -> `App (_loc, subst_first_loc name a , b)
        | `Constraint(_loc,a,ty) -> `Constraint(_loc,subst_first_loc name a,ty)      
              (* | {| $a $b |} -> {| $(subst_first_loc name a) $b |} *)
        |p -> p  in
      (* fun [{:pat| `a ($loc,b,c)|} -> b] *)
      match loc_name_opt with
      | None -> subst_first_loc (!FLoc.name) exp_ast
      | Some "_" -> exp_ast
      | Some name -> subst_first_loc name exp_ast 
    end in begin
      add name FDyn.exp_tag expand_exp;
      add name FDyn.pat_tag expand_pat;
      add name FDyn.stru_tag expand_stru;
    end

(*****************************************)
(* register function                     *)    
(*****************************************)    


let make_parser ?(lexer=Flex_lib.from_stream) entry =
  fun loc loc_name_opt s  ->
    Ref.protect2
      (FConfig.antiquotations, true)
      (current_loc_name,loc_name_opt)
      (fun _ -> Fgram.parse_string ~lexer (Fgram.eoi_entry entry) ~loc  s);;

  
let of_stru ?lexer ~name  ~entry ()  =
  add name FDyn.stru_tag (make_parser ?lexer entry)

let of_stru_with_filter ?lexer ~name  ~entry  ~filter ()  =
  add name FDyn.stru_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))

let of_pat ?lexer ~name  ~entry  () =
  add name FDyn.pat_tag (make_parser ?lexer  entry)

let of_pat_with_filter ?lexer ~name  ~entry  ~filter ()  =
  add name FDyn.pat_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))

let of_clfield ?lexer ~name  ~entry ()  =
  add name FDyn.clfield_tag (make_parser ?lexer entry)

let of_clfield_with_filter ?lexer ~name  ~entry  ~filter ()  =
  add name FDyn.clfield_tag @@
    fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s)

let of_case ?lexer ~name  ~entry  () =
  add name FDyn.case_tag (make_parser ?lexer entry)

let of_case_with_filter ?lexer ~name  ~entry  ~filter ()=
  add name FDyn.case_tag
    (fun loc  loc_name_opt  s  ->
       filter (make_parser ?lexer entry loc loc_name_opt s))

let of_exp ?lexer ~name  ~entry () =
  let expand_fun = make_parser ?lexer entry in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, expand_fun loc loc_name_opt s) : FAst.stru ) in
  begin
    add name FDyn.exp_tag expand_fun;
    add name FDyn.stru_tag mk_fun
  end

let of_exp_with_filter ?lexer ~name  ~entry  ~filter () =
  let expand_fun loc loc_name_opt s =
    filter (make_parser ?lexer entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    (`StExp (loc, (expand_fun loc loc_name_opt s)) : FAst.stru ) in
  begin
    add name FDyn.exp_tag expand_fun;
    add name FDyn.stru_tag mk_fun
  end
    
(* let () = *)
(*   Printexc.register_printer (function *)
(*   | QuotationError x -> Some (quotation_error_to_string x ) *)
(*   | _ -> None);; *)


(* local variables: *)
(* compile-command: "cd .. && pmake hot_annot/ast_quotation.cmo" *)
(* end: *)
