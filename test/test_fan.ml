(* open Format; *)
(* #load "fan.cma"; *)
(* <:fan< lang "ocaml"; keep on; show_code on; plugin_add "Print"; >> ; *)
(* << type t = [A of int | B of bool ] ;  >> ; *)
#load_rec "FanLexUtil.cmo";;
#load_rec "FanTop.cma";;
open FanTop;;
open FanTop.P;;
open FanSig;;
open LibUtil;;
open FanUtil;;
open Stream;;
open Grammar;;

(* module Tools = Tools.Make(struct end) ;; *)
(* let f = FanToken.Filter.filter Gram.gram.FanTop.Gram.gfilter;; *)
(* let f = FanToken.Filter.filter Gram.gram.Gram.gfilter;; *)
(* open Lib;; *)
(* module MetaAst = Camlp4Ast.Meta.Make (Meta.MetaLocQuotation );; *)
(* let anti_obj = Expr.antiquot_expander ~parse_expr:Syntax.AntiquotSyntax.parse_expr ~parse_patt:Syntax.AntiquotSyntax.parse_patt ;; *)
(* open Ast;; *)


(* (FanLexer.from_string FanLoc.string_loc "let a = 3") *)
(* |> Stream.take 10 *)
(* |> Tools.keep_prev_loc  *)
(* |> Stream.iter (fun [ (t, {Structure.prev_loc=p;cur_loc=c;prev_loc_only=o}) -> *)
(*     pp f "@[<v0>%a@;prev:%a@;cur:%a@;only:%b@;@]" FanToken.print t FanLoc.print p FanLoc.print c o]); *)

  
(* Tools.keep_prev_loc ((FanLexUtil.clean (FanLexer.from_string FanLoc.string_loc "let a = 3"))) *)
(* |> Stream.iter (fun [ (t, {Structure.prev_loc=p;cur_loc=c;prev_loc_only=o}) -> *)
(*     pp f "@[<v0>%a@;prev:%a@;cur:%a@;only:%b@;@]" FanToken.print t FanLoc.print p FanLoc.print c o]); *)
