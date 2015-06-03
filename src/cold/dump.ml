let pp = Format.fprintf
let pp_exp =
  function
  | f ->
      (function | e -> pp f "@[%a@]@." Ast_print.expression (Ast2pt.exp e))
let pp_pat =
  function
  | f -> (function | e -> pp f "@[%a@]@." Ast_print.pattern (Ast2pt.pat e))
let pp_stru =
  function
  | f ->
      (function | e -> pp f "@[%a@]@." Ast_print.structure (Ast2pt.stru e))
let pp_ctyp =
  function
  | f ->
      (function | e -> pp f "@[%a@]@." Ast_print.core_type (Ast2pt.ctyp e))
let exp_to_string = Formatf.to_string pp_exp
module N =
  struct
    let pp_exp =
      function
      | f -> (function | e -> (e |> (Fill.exp Locf.ghost)) |> (pp_exp f))
    let pp_pat =
      function
      | f -> (function | e -> (e |> (Fill.pat Locf.ghost)) |> (pp_pat f))
    let pp_stru =
      function
      | f -> (function | e -> (e |> (Fill.stru Locf.ghost)) |> (pp_stru f))
    let pp_ctyp =
      function
      | f -> (function | e -> (e |> (Fill.ctyp Locf.ghost)) |> (pp_ctyp f))
  end
