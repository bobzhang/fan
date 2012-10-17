(* open Format; *)
(* #load "fan.cma"; *)
(* <:fan< lang "ocaml"; keep on; show_code on; plugin_add "Print"; >> ; *)
(* << type t = [A of int | B of bool ] ;  >> ; *)
#load "FanTop.cma";;
open FanTop;;
open FanTop.P;;
open FanSig;;
open LibUtil;;
open FanUtil;;
open Stream;;
(* let f = FanToken.Filter.filter Gram.gram.FanTop.Gram.gfilter;; *)
(* let f = FanToken.Filter.filter Gram.gram.Gram.gfilter;; *)
(* open Lib;; *)
(* module MetaAst = Camlp4Ast.Meta.Make (Meta.MetaLocQuotation );; *)
(* let anti_obj = Expr.antiquot_expander ~parse_expr:Syntax.AntiquotSyntax.parse_expr ~parse_patt:Syntax.AntiquotSyntax.parse_patt ;; *)
(* open Ast;; *)


















