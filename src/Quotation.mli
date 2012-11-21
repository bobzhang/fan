module type AntiquotSyntax =
  sig
    val parse_expr : FanLoc.t -> string -> Ast.expr
    val parse_patt : FanLoc.t -> string -> Ast.patt
    val parse_ident : FanLoc.t -> string -> Ast.ident
  end
module type S =
  sig
    type 'a expand_fun = FanLoc.t -> string option -> string -> 'a
    val add : string -> 'a DynAst.tag -> 'a expand_fun -> unit
    val default : string ref
    val default_tbl : (string, string) LibUtil.Hashtbl.t
    val default_at_pos : string -> string -> unit
    val parse_quotation_result :
      (FanLoc.t -> string -> 'a) ->
      FanLoc.t -> FanSig.quotation -> string -> string -> 'a
    val translate : (string -> string) ref
    val expand : FanLoc.t -> FanSig.quotation -> 'a DynAst.tag -> 'a
    val dump_file : string option ref
    val add_quotation :
      string ->
      'a Gram.t ->
      (FanLoc.t -> 'a -> Lib.Expr.Ast.expr) ->
      (FanLoc.t -> 'a -> Lib.Expr.Ast.patt) -> unit
    val add_quotation_of_expr : name:string -> entry:Ast.expr Gram.t -> unit
    val add_quotation_of_patt : name:string -> entry:Ast.patt Gram.t -> unit
    val add_quotation_of_class_str_item :
      name:string -> entry:Ast.class_str_item Gram.t -> unit
    val add_quotation_of_match_case :
      name:string -> entry:Ast.match_case Gram.t -> unit
  end
module Make : functor (TheAntiquotSyntax : AntiquotSyntax) -> S
