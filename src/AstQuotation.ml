open Ast;
open LibUtil;
open FanUtil;
open Lib.Meta;
open Format;
(* open StdLib; *)

(* FIXME order is in-correct  *)
{:fans|keep on; (* derive (Print); *) |};

(* {:ocaml| *)
type quotation_error_message =
    [ Finding
    | Expanding
    | ParsingResult of FanLoc.t and string
    | NoName];

(* the first argument is quotation name
   the second argument is the position tag 
 *)
type quotation_error = (string * string * quotation_error_message * exn); 
(* |}; *)



exception QuotationError of quotation_error;

(* +-----------------------------------------------------------------+
   | expand fun accepts [location] and [location label] and string   |
   | to generate an arbitrary value of type ['a]                     |
   +-----------------------------------------------------------------+ *)

type expand_fun 'a = FanLoc.t -> option string -> string -> 'a;
  
module ExpKey = DynAst.Pack(struct  type t 'a = unit; end);

module ExpFun = DynAst.Pack(struct  type t 'a = expand_fun 'a; end);



let current_loc_name = ref None  ;
let stack = Stack.create ();
  
let current_quot () =
  try Stack.pop stack
  with [Stack.Empty -> failwith "it's not in a quotation context"];
    
let dump_file = ref None;



(* create a table mapping from  (string_of_tag tag) to default
   quotation expander intentionaly make its value a string to
   be more flexibile to incorporating more tags in the future
 *)  

let default = ref "";
(* let default = ref None ;   *)
let map = ref SMap.empty  ;

let update (pos,str) =
  map := SMap.add pos str !map;

let translate = ref (fun x -> x);

let clear_map () =  map := SMap.empty;

let clear_default () = default:="";

let default_at_pos pos str =  update (pos,str); 

(*
  The key is [string*ExpKey.pack]
 *)
type key = (string * ExpKey.pack);
module QMap =MapMake (struct type t =key ; let compare = compare; end);  
let expanders_table =ref QMap.empty;  
(* let expanders_table =  (ref [] : ref (list ((string * ExpKey.pack) * ExpFun.pack))); *)
let set_default s =  default := s;

(* If the quotation has a name, it has a higher precedence,
   otherwise the [position table] has a precedence, otherwise
   the default is used 
 *)  
let expander_name ~pos:(pos:string) (name:string) =
  (* let str = DynAst.string_of_tag pos_tag in *)
  let u = !translate name in 
  if u = "" then
    SMap.find_default ~default:(!default)  pos !map
  else u;

let add name (tag :DynAst.tag 'a) (f:expand_fun 'a) =
  let (k,v) = ((name, ExpKey.pack tag ()), ExpFun.pack tag f) in
  expanders_table := QMap.add k v !expanders_table ;

(* called by [expand] *)    
let expand_quotation loc ~expander pos_tag quot =
  let open FanToken in
  let loc_name_opt = if quot.q_loc = "" then None else Some quot.q_loc in
  try expander loc loc_name_opt quot.q_contents with
  [ FanLoc.Exc_located (_, (QuotationError _)) as exc ->
      raise exc
  | FanLoc.Exc_located (iloc, exc) ->
      let exc1 = QuotationError (quot.q_name, pos_tag, Expanding, exc) in
      raise (FanLoc.Exc_located iloc exc1)
  | exc ->
      let exc1 = QuotationError (quot.q_name, pos_tag, Expanding, exc) in
      raise (FanLoc.Exc_located loc exc1) ];

(* The table is indexed by [quotation name] and [tag] *)
let find loc name tag =
  let key = (expander_name ~pos:(DynAst.string_of_tag tag) name, ExpKey.pack tag ()) in
  let try pack = QMap.find key !expanders_table in
  ExpFun.unpack tag pack
  with
    [Not_found ->
      let pos_tag = DynAst.string_of_tag tag in
      if name="" then raise
          (FanLoc.raise loc (QuotationError (name,pos_tag,NoName,Not_found)))
      else
        raise Not_found
  | e -> raise e  ];     

(*
  [tag] is used to help find the expander,
  is passed by the parser function at parsing time
 *)
let expand loc (quotation:FanToken.quotation) tag =
  let open FanToken in
  let pos_tag = DynAst.string_of_tag tag in
  let name = quotation.q_name in
  let try expander = find loc name tag
  and loc = FanLoc.join (FanLoc.move `start quotation.q_shift loc)  in begin
    Stack.push  quotation.q_name stack;
    finally (fun _ -> Stack.pop stack) begin fun _ ->
      expand_quotation ~expander loc pos_tag quotation
    end ()
  end
  with
    [ FanLoc.Exc_located (_, (QuotationError _)) as exc -> raise exc
    | FanLoc.Exc_located (qloc, exc) ->
        raise (FanLoc.Exc_located qloc (QuotationError (name, pos_tag, Finding, exc)))
    | exc ->
        raise (FanLoc.Exc_located loc (QuotationError (name, pos_tag, Finding, exc))) ];

  




let quotation_error_to_string (name, position, ctx, exn) =
  let ppf = Buffer.create 30 in
  let name = expander_name ~pos:position name in 
  let pp x = bprintf ppf "@?@[<2>While %s %S in a position of %S:" x name position in
  let () =
      match ctx with
      [ Finding -> begin
        pp "finding quotation";
        bprintf ppf "@ @[<hv2>Available quotation expanders are:@\n";
        (* List *)QMap.iter begin fun (s,t) _ ->
          bprintf ppf "@[<2>%s@ (in@ a@ position@ of %a)@]@ "
            s ExpKey.print_tag t
        end !expanders_table;
        bprintf ppf "@]";
        end
      | Expanding ->  pp "expanding quotation"
      | ParsingResult (loc, str) -> begin
          pp "parsing result of quotation" ;
          match !dump_file with
          [ Some dump_file ->
            let () = bprintf ppf " dumping result...\n" in
            try
              let oc = open_out_bin dump_file in
              begin
                output_string oc str;
                output_string oc "\n";
                flush oc;
                close_out oc;
                bprintf ppf "%a:" FanLoc.print (FanLoc.set_file_name dump_file loc);
              end
            with _ ->
              bprintf ppf
                "Error while dumping result in file %S; dump aborted"
                dump_file
           | None ->
                bprintf ppf
                  "\n(consider setting variable AstQuotation.dump_file, or using the -QD option)"
          ]
      end
      | NoName -> pp "No default quotation name" ] in
    let () = bprintf ppf "@\n%s@]@." (Printexc.to_string exn)in Buffer.contents ppf;

Printexc.register_printer (fun
  [ QuotationError x -> Some (quotation_error_to_string x )
  | _ -> None]);

let parse_quotation_result parse loc quot pos_tag str =
  let open FanToken in 
  try parse loc str with
  [ FanLoc.Exc_located (iloc, (QuotationError (n, pos_tag, Expanding, exc))) ->
      let ctx = ParsingResult iloc quot.q_contents in
      let exc1 = QuotationError (n, pos_tag, ctx, exc) in
      FanLoc.raise iloc exc1
  | FanLoc.Exc_located (iloc, (QuotationError _ as exc)) ->
      FanLoc.raise iloc exc 
  | FanLoc.Exc_located (iloc, exc) ->
      let ctx = ParsingResult iloc quot.q_contents in
      let exc1 = QuotationError (quot.q_name, pos_tag, ctx, exc) in
      FanLoc.raise iloc exc1 ];

    

let add_quotation ~expr_filter ~patt_filter  ~mexpr ~mpatt name entry  =
  let entry_eoi = Gram.eoi_entry entry in 
  let expand_expr loc loc_name_opt s =
    Ref.protect2 (FanConfig.antiquotations,true) (current_loc_name, loc_name_opt)
      (fun _ ->
        Gram.parse_string entry_eoi ~loc s |> mexpr loc |> expr_filter) in
  let expand_str_item loc loc_name_opt s =
    let exp_ast = expand_expr loc loc_name_opt s in
    {:str_item@loc| $(exp:exp_ast) |} in
  let expand_patt _loc loc_name_opt s =
    Ref.protect FanConfig.antiquotations true begin fun _ ->
      let ast = Gram.parse_string entry_eoi ~loc:_loc s in
      let meta_ast = mpatt _loc ast in
      let exp_ast = patt_filter meta_ast in
      (* BOOTSTRAPPING *)
      let rec subst_first_loc name : patt -> patt =  with patt fun
        [
         `PaApp(loc, `PaVrn (_,u), (`Tup (_, `Com (_,_,rest)))) ->
         `PaApp(loc, `PaVrn(loc,u),(`Tup (loc,`Com(loc,`Id(_loc,`Lid (_loc,name)),rest))))
        | `PaApp(_loc,`PaVrn(_,u),`Any _) ->
            `PaApp(_loc, `PaVrn(_loc,u), `Id(_loc,`Lid(_loc,name)))
        | `PaApp(_loc,a,b) -> `PaApp (_loc, subst_first_loc name a , b)
              
        (* | {| $a $b |} -> {| $(subst_first_loc name a) $b |} *)
        |p -> p ] in

      (* fun [{:patt| `a ($loc,b,c)|} -> b] *)
          
      match loc_name_opt with
      [ None -> subst_first_loc (!FanLoc.name) exp_ast
      | Some "_" -> exp_ast
      | Some name -> subst_first_loc name exp_ast ]
    end in begin
        add name DynAst.expr_tag expand_expr;
        add name DynAst.patt_tag expand_patt;
        add name DynAst.str_item_tag expand_str_item;
    end;

let make_parser entry =
  fun loc loc_name_opt s  -> 
    Ref.protect2
      (FanConfig.antiquotations, true)
      (current_loc_name,loc_name_opt)
      (fun _ -> Gram.parse_string (Gram.eoi_entry entry) ~loc  s);

DEFINE REGISTER(tag) = fun  ~name ~entry -> add name tag (make_parser entry);
DEFINE REGISTER_FILTER(tag) = fun ~name ~entry ~filter -> 
  add name tag (fun loc loc_name_opt s -> filter (make_parser entry loc loc_name_opt s));

  
let of_str_item = REGISTER(DynAst.str_item_tag);
let of_str_item_with_filter = REGISTER_FILTER(DynAst.str_item_tag);  
let of_patt  = REGISTER(DynAst.patt_tag);
let of_patt_with_filter  = REGISTER_FILTER(DynAst.patt_tag);  
let of_class_str_item  = REGISTER(DynAst.class_str_item_tag);
let of_class_str_item_with_filter  = REGISTER_FILTER(DynAst.class_str_item_tag);  
let of_match_case = REGISTER(DynAst.match_case_tag);
let of_match_case_with_filter = REGISTER_FILTER(DynAst.match_case_tag);  
  

(* both [expr] and [str_item] positions are registered *)  
let of_expr ~name ~entry = 
  let expand_fun =  make_parser entry in
  let mk_fun loc loc_name_opt s =
    {:str_item@loc| $(exp:expand_fun loc loc_name_opt s) |} in begin 
      add name DynAst.expr_tag expand_fun ;
      add name DynAst.str_item_tag mk_fun ;
    end ;
  
let of_expr_with_filter ~name ~entry ~filter =     
  let expand_fun =
    fun loc loc_name_opt s -> filter ( make_parser entry loc loc_name_opt s) in
  let mk_fun loc loc_name_opt s =
    {:str_item@loc| $(exp:expand_fun loc loc_name_opt s) |} in begin 
      add name DynAst.expr_tag expand_fun ;
      add name DynAst.str_item_tag mk_fun ;
    end ;
  
module MetaLocQuotation = struct
  let meta_loc_expr _loc loc =
    match !current_loc_name with
    [ None -> {:expr| $(lid:!FanLoc.name) |}
    | Some "here" -> MetaLoc.meta_loc_expr _loc loc
    | Some x -> {:expr| $lid:x |} ];
  let meta_loc_patt _loc _ =  {:patt| _ |}; (* we use [subst_first_loc] *)
end;

let gm () =
  match !FanConfig.compilation_unit with
  [Some "FanAst" -> begin (* eprintf "Compilation unit: FanAst";  *)"" end
  | Some _ -> begin (* eprintf "Compilation unit: %s@." x;  *)"FanAst" end
  | None -> begin (* eprintf "Compilation unit None@." ;  *)"FanAst" end];  
  (* !module_name; *)

let antiquot_expander ~parse_patt ~parse_expr = object
  inherit FanAst.map as super;
  method! patt =
    with patt
    fun
    [`Ant(_loc, {cxt;sep;decorations;content=code}) -> 
      let mloc _loc = MetaLocQuotation.meta_loc_patt _loc _loc in
      let e = parse_patt _loc code in 
      match (decorations,cxt,sep) with
      [("anti",_,_) -> {| `Ant ($(mloc _loc), $e) |}
      |("uid",_,_) -> {|`Uid($(mloc _loc), $e)|}
      |("lid",_,_) -> {|`Lid($(mloc _loc), $e)|}

      (* |("id","ctyp",_) -> {|`Id($(mloc _loc),$e)|} *)
            
      |("tup",_,_) ->  {| `Tup ($(mloc _loc), $e)|}
      |("seq",_,_) -> {| `Seq ($(mloc _loc), $e) |}
      |("flo",_,_) -> {| `Flo($(mloc _loc), $e)|}
      |("int",_,_) -> {| `Int ($(mloc _loc), $e)|}
      |("int32",_,_)-> {| `Int32 ($(mloc _loc),$e)|}
      |("int64",_,_) -> {| `Int64($(mloc _loc),$e)|}
      |("nativeint",_,_) -> {|`NativeInt ($(mloc _loc),$e)|}
      |("chr",_,_) -> {|`Chr($(mloc _loc),$e)|}
      |("str",_,_) -> {|`Str($(mloc _loc),$e)|}
      |("vrn","expr",_) -> {|`ExVrn($(mloc _loc),$e)|}
      |("vrn","patt",_) -> {|`PaVrn($(mloc _loc),$e)|}
      | _ -> super#patt e ]
    | e -> super#patt e];
    method! expr = with expr fun 
      [`Ant(_loc,{cxt;sep;decorations;content=code}) ->
        let mloc _loc = MetaLocQuotation.meta_loc_expr _loc _loc in
        let e = parse_expr _loc code in
        match (decorations,cxt,sep) with
          [ ("anti",_,__) -> {|`Ant($(mloc _loc),$e)|}
          | ("tup",_,_) -> {|`Tup($(mloc _loc),$e)|}
          | ("seq",_,_) -> {|`Seq($(mloc _loc),$e)|}
          | ("vrn","expr",_) -> {|`ExVrn($(mloc _loc),$e)|}
          | ("vrn","patt",_) -> {|`PaVrn($(mloc _loc),$e)|}
          | ("lid",_,_) -> {|`Lid($(mloc _loc),$e)|}
          | ("uid",_,_) -> {|`Uid($(mloc _loc),$e)|}
                
          (* | ("id","ctyp",_) -> {|`Id($(mloc _loc),$e)|} *)
                
          | ("str",_,_) ->  {|`Str($(mloc _loc),$e)|}
          | ("chr",_,_) -> {|`Chr ($(mloc _loc), $e)|} 
          | ("int",_,_) -> {|`Int($(mloc _loc),$e)|}
          | ("int32",_,_) -> {|`Int32($(mloc _loc),$e)|}
          | ("int64",_,_) -> {|`Int64($(mloc _loc),$e)|}
          | ("flo",_,_) -> {|`Flo($(mloc _loc),$e)|}
          | ("nativeint",_,_) -> {|`NativeInt ($(mloc _loc),$e)|}
          | ("`nativeint",_,_) ->
              let e = {| Nativeint.to_string $e |} in
              {| `NativeInt ($(mloc _loc), $e) |}
          | ("`int",_,_) ->
              let e = {|string_of_int $e |} in
              {| `Int ($(mloc _loc), $e) |}
          | ("`int32",_,_) ->
              let e = {|Int32.to_string $e |} in
              {| `Int32 ($(mloc _loc), $e) |}
          | ("`int64",_,_) ->
              let e = {|Int64.to_string $e |} in
              {| `Int64 ($(mloc _loc), $e) |}
          | ("`chr",_,_) ->
                let e = {|Char.escaped $e|} in
                {| `Chr ($(mloc _loc), $e) |}
          | ("`str",_,_) ->
                let e = {|$(uid:gm()).safe_string_escaped $e |} in
                {| `Str ($(mloc _loc), $e) |}
          | ("`flo",_,_) ->
              let e = {| FanUtil.float_repres $e |} in 
              {| `Flo ($(mloc _loc), $e) |}
          | ("`bool",_,_) ->
              let x = {| `Lid ($(mloc _loc), (if $e then "true" else "false" )) |} in
              {| {| $(id:$x)  |} |}

          | ("list","module_expr",_) ->
              {| $(uid:gm()).app_of_list $e |}
          | ("list","module_type",_) ->
              {| $(uid:gm()).mtApp_of_list $e |}
          | ("list","ident",_) -> 
              {| $(uid:gm()).idAcc_of_list $e |}
          | ("list",
             ("binding"|"module_binding"|
              "with_constr"|"class_type"|
              "class_expr"|"ctypand"),_) ->
                {| $(uid:gm()).and_of_list $e |}
          |("list","ctyp*",_) ->
              {| $(uid:gm()).sta_of_list $e |}

          |("list","ctyp|",_)
          |("list","match_case",_) ->
              {| $(uid:gm()).or_of_list $e |}
          |("list","ctyp&",_) ->
              {| $(uid:gm()).amp_of_list $e |}
          |("listlettry","match_case",_) ->
              {| (($(uid:gm()).match_pre)#match_case
                    ($(uid:gm()).or_of_list $e)) |}
          |("antilettry","match_case",_) ->
              {| $(uid:gm()).match_pre#match_case (`Ant ($(mloc _loc), $e)) |}
          |("lettry","match_case",_) ->
              {| $(uid:gm()).match_pre#match_case $e |}
          |("list",("ctyp,"|"patt,"|"expr,"),_) ->
              {| $(uid:gm()).com_of_list $e |}
          |("list",
            ("binding;"|"str_item"
            |"sig_item"|"class_sig_item"
            |"class_str_item"|"rec_binding"
            |"ctyp;"|"patt;"|"expr;"),_) ->
                {| $(uid:gm()).sem_of_list $e |}
          |("list","forall",_) ->
              {| $(uid:gm()).tyVarApp_of_list $e |}
          | _ -> super#expr e]
        | e -> super#expr e];  
  end;


  
