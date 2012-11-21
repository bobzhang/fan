open Parsetree
open Asttypes
open Longident
class printer =
  object (self : 'self)
    method longident _loc i =
      match i with
      | Lident s -> Ast.IdLid (_loc, s)
      | Ldot (y,s) ->
          Ast.IdAcc (_loc, (self#longident _loc y), (Ast.IdLid (_loc, s)))
      | Lapply (a,b) ->
          Ast.IdApp (_loc, (self#longident _loc a), (self#longident _loc b))
    method constant_expr _loc i =
      match i with
      | Const_int32 i -> Ast.ExInt32 (_loc, (Int32.to_string i))
      | Const_int i -> Ast.ExInt (_loc, (string_of_int i))
      | Const_int64 i -> Ast.ExInt64 (_loc, (Int64.to_string i))
      | Const_float i -> Ast.ExFlo (_loc, i)
      | Const_nativeint i -> Ast.ExNativeInt (_loc, (Nativeint.to_string i))
      | Const_char i -> Ast.ExChr (_loc, (Char.escaped i))
      | Const_string i -> Ast.ExStr (_loc, (Ast.safe_string_escaped i))
  end