module IdAstLoader = struct
  value name = "Camlp4AstLoader";
  value version = Sys.ocaml_version;
end;

module MakeAstLoader (Ast : FanSig.Ast) : (FanSig.Parser Ast).S= struct
  module Ast = Ast;

  value parse ast_magic ?directive_handler:(_) _loc strm =
    let str =
      let buf = Buffer.create 2047 in
      let () = Stream.iter (Buffer.add_char buf) strm
      in Buffer.contents buf in
    let magic_len = String.length ast_magic in
    let buffer = String.create magic_len in
    do {
      String.blit str 0 buffer 0 magic_len;
      if buffer = ast_magic then ()
      else failwith (Format.sprintf "Bad magic: %S vs %S" buffer ast_magic);
      Marshal.from_string str magic_len;
    };

  value parse_implem = parse FanConfig.camlp4_ast_impl_magic_number;
  value parse_interf = parse FanConfig.camlp4_ast_intf_magic_number;
end;
