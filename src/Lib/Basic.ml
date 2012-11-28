open Format;
open LibUtil;

(*
   Basic module contains utility functions to manipulate Camlp4Ast
   This module is mainly provided to generate code. For simplicity,
   we don't take care of Location.
 *)
exception Unhandled of Ast.ctyp ;
exception Finished of Ast.expr;

let _loc =FanLoc.ghost ;
  
let unit_literal = {:expr| () |} ;
  
let x ?(off=0) (i:int)    =
  if off > 25 then invalid_arg "unsupported offset in x "
  else
    let base = Char.(code 'a' + off |> chr) in
    String.of_char base ^ string_of_int i;
    
let xid ?(off=0) (i:int) : Ast.ident  =
  {:ident| $(lid:x ~off i) |} ;
  
let allx ?(off=0) i =  "all_" ^x ~off i ;
  
let allxid ?(off=0) i = {:ident| $(lid:allx ~off i) |};
  
let check_valid str =
  let len = String.length str in
  if
    not
      (len > 1 &&
       (not (Char.is_digit str.[1]))
         && (not (String.starts_with str "all_"))) then begin
           eprintf "%s is not a valid name" str;
           eprintf "For valid name its length should be more than 1\n\
             can not be a-[digit], can not start with [all_]";
           exit 2;
         end 
  else ();    

let p_expr f  e =
  pp f "@[%a@]@." AstPrint.expression (Ast2pt.expr e);
(* let p_ident = eprintf "@[%a@]@." opr#ident ;     *)
let p_patt f e =
  pp f "@[%a@]@." AstPrint.pattern (Ast2pt.patt e);
  
let p_str_item f e =
  pp f "@[%a@]@." AstPrint.structure (Ast2pt.str_item e);

(* FIXME allow more interfaces later *)  
(* let p_ident f e = *)
(*   eprintf "@[%a@]@." Pprintast.fmt_longident (Ast2pt.ident e) ;     *)
let p_ctyp f e =
  pp f "@[%a@]@." AstPrint.core_type (Ast2pt.ctyp e) ;

  
let error_report (loc,s) = begin
  prerr_endline (FanLoc.to_string loc);
  let (start_bol,stop_bol,
         start_off, stop_off) =
    FanLoc.( (start_bol loc,
             stop_bol loc,
             start_off loc,
             stop_off loc)
           ) in
  let abs_start_off = start_bol + start_off in
  let abs_stop_off = stop_bol + stop_off in
  let err_location = String.sub s abs_start_off
      (abs_stop_off - abs_start_off + 1) in
  prerr_endline (sprintf "err: ^%s^" err_location);
end ;


let parse_string_of_entry ?(_loc=FanLoc.mk "<string>") entry  s =
  try
    Gram.parse_string entry  _loc s
  with
    [FanLoc.Exc_located(loc, e) -> begin
      eprintf "%s" (Printexc.to_string e);
      error_report (loc,s);
      FanLoc.raise loc e ;
    end ];

let wrap_stream_parser ?(_loc=FanLoc.mk "<stream>") p s =
  try p _loc s
  with
    [FanLoc.Exc_located(loc,e) -> begin
      eprintf "error: %s" (FanLoc.to_string loc) ;
      FanLoc.raise loc e;
    end 
   ];






(** [Fan_quot] is a library which helps to design DSLs  
    This module provides  utlities to
    transform parser to quotation expander *)
(* module MetaLocHere = Ast.Meta.MetaLoc; *)

(* the same as [Meta.MeatLocQuotation] *)
(* module MetaLoc = struct *)
(*   module Ast = Ast; *)
(*   let loc_name = ref None; *)
(*   let meta_loc_expr _loc loc = *)
(*     match !loc_name with *)
(*     [ None -> {:expr| .$lid:Loc.name.val$. |} *)
(*     | Some "here" -> MetaLocHere.meta_loc_expr _loc loc *)
(*     | Some x -> {:expr| .$lid:x$. |} ]; *)
(*   let meta_loc_patt _loc _ = {:patt| _ |}; *)
(* end; *)
(* module MetaAst = Ast.Meta.Make MetaLoc; is the same as [MetaQAst]*)
(* open Camlp4.PreCast; *)
  
(* module Make (MGram:Camlp4.Sig.Grammar.Static *)
(*             with module Loc = Loc *)
(*             and  module Token = Token)  : Fan_sig.Grammar with *)
(*                      type t 'a = MGram.Entry.t 'a *)
(*                      and type loc = MGram.Loc.t = struct *)
(*   type t 'a = MGram.Entry.t 'a; *)
(*   type loc =  MGram.Loc.t ; *)
(*   (\* add an end marker for each parser *\) *)
(*   let eoi_entry entry = do{ *)
(*     let entry_eoi = MGram.Entry.(mk (name entry)) ; *)
(*     EXTEND MGram entry_eoi: *)
(*       [ [ x =entry ; `EOI -> x ]]; *)
(*     END; *)
(*     entry_eoi   *)
(*    }; *)


(*   let parse_quot_string_with_filter entry f loc loc_name_opt s  = do{ *)
(*     let q = Camlp4_config.antiquotations.val ; *)
(*     Camlp4_config.antiquotations.val := True; *)
(*     let res = MGram.parse_string entry loc s ;   *)
(*     Camlp4_config.antiquotations.val := q; *)
(*     MetaLoc.loc_name.val := loc_name_opt ; *)
(*     (\* not sure whether this refactoring is correct *\) *)
(*     f res *)
(*   }; *)

(*   (\* given an string input, apply the parser, return the result *\) *)
(*   let parse_quot_string entry  loc  loc_name_opt s = *)
(*     parse_quot_string_with_filter entry (fun x -> x) loc *)
(*       loc_name_opt s ; *)

(*   (\* here entry [does not need] to handle eoi case, *)
(*     we will add eoi automatically. *)
(*     This add quotation utility is for normal *)
(*     It's tailored for ADT DSL paradigm  *\) *)
(*   let add_quotation ?antiquot_expander name   ~entry *)
(*      ~mexpr ~mpatt  = ( *)
(*     let anti_expr = match antiquot_expander with *)
(*       [ None -> fun x -> x | Some obj -> obj#expr] in *)
(*     let anti_patt = match antiquot_expander with *)
(*       [ None -> fun x -> x | Some obj -> obj#patt] in *)
(*     let entry_eoi = eoi_entry entry in  *)
(*     let expand_expr loc loc_name_opt s =  *)
(*       parse_quot_string entry_eoi loc  loc_name_opt s *)
(*       |> mexpr loc *)
(*       |> anti_expr  in *)
(*     let expand_str_item loc loc_name_opt s = *)
(*       let exp_ast = expand_expr loc loc_name_opt s  in  *)
(*       <:str_item@loc< .$exp:exp_ast$. >> in *)
(*     let expand_patt _loc loc_name_opt s = *)
(*       let exp_ast = *)
(*         parse_quot_string entry_eoi _loc  loc_name_opt s *)
(*         |> mpatt _loc *)
(*         |> anti_patt in  *)
(*       match loc_name_opt with *)
(*       [ None -> exp_ast *)
(*       | Some name -> *)
(*           let rec subst_first_loc = *)
(*           fun *)
(*           [ <:patt@_loc< Ast. .$uid:u$. .$_$. >> *)
(*             -> {:patt| Ast. .$uid:u$. .$lid:name$. |} *)
(*           | <:patt@_loc< .$a$. .$b$. >> *)
(*             -> {:patt| .$subst_first_loc a$. .$b$. |} *)
(*           | p -> p ] in *)
(*         subst_first_loc exp_ast ] in *)
(*     let open Quotation in  *)
(*     do { *)
(*       add name DynAst.expr_tag expand_expr; *)
(*       add name DynAst.patt_tag expand_patt; *)
(*       add name DynAst.str_item_tag expand_str_item; *)
(*     } *)
(* ); *)

(*     let add = Quotation.add; *)
(*     let add_quotation_of_str_item ~name ~entry = *)
(*       add name Quotation.DynAst.str_item_tag *)
(*         (parse_quot_string (eoi_entry entry)); *)
(*     let add_quotation_of_str_item_with_filter ~name ~entry ~filter = *)
(*       add name Quotation.DynAst.str_item_tag *)
(*         (parse_quot_string_with_filter (eoi_entry entry) filter); *)
(*     (\* will register str_item as well  *\)  *)
(*     let add_quotation_of_expr ~name ~entry = begin *)
(*       let expand_fun = parse_quot_string & eoi_entry entry in  *)
(*       let mk_fun loc loc_name_opt s = *)
(*         {:str_item| .$exp:expand_fun loc loc_name_opt s$. |} in  *)
(*       let () = add name Quotation.DynAst.expr_tag expand_fun in  *)
(*       let () = add name Quotation.DynAst.str_item_tag mk_fun in *)
(*       () *)
(*     end ; *)
(*     let add_quotation_of_patt ~name ~entry = *)
(*       add name Quotation.DynAst.patt_tag (parse_quot_string (eoi_entry entry)); *)
(*     let add_quotation_of_class_str_item ~name ~entry = *)
(*      add name Quotation.DynAst.class_str_item_tag (parse_quot_string (eoi_entry entry)); *)
(*     let add_quotation_of_match_case ~name ~entry = *)
(*       add name Quotation.DynAst.match_case_tag *)
(*         (parse_quot_string (eoi_entry entry)); *)
(* end; *)

(* (\** Built in MGram  utilities *\) *)
(* module Fan_camlp4syntax = Make(Gram); *)
(* let (anti_str_item, anti_expr) *)
(*     =  Fan_camlp4syntax.( *)
(*   (eoi_entry Syntax.str_item, *)
(*   eoi_entry Syntax.expr) *)
(*  ) *)
(* ; *)


let is_antiquot_data_ctor s = String.ends_with s "Ant";
    
