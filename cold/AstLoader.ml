module IdAstLoader =
 struct let name = "Camlp4AstLoader"
 let version = Sys.ocaml_version
 end


module MakeAstLoader =
 functor (Ast : FanSig.Ast) ->
  (struct
    module Ast = Ast

    let parse =
     fun ast_magic ->
      fun ?directive_handler:_ ->
       fun _loc ->
        fun strm ->
         let str =
          let buf = (Buffer.create 2047) in
          let () = (Stream.iter ( (Buffer.add_char buf) ) strm) in
          (Buffer.contents buf) in
         let magic_len = (String.length ast_magic) in
         let buffer = (String.create magic_len) in
         (
         (String.blit str 0 buffer 0 magic_len)
         );
         (
         if (buffer = ast_magic) then () 
         else
          (failwith ( (Format.sprintf "Bad magic: %S vs %S" buffer ast_magic)
            ))
         );
         (Marshal.from_string str magic_len)

    let parse_implem = (parse FanConfig.camlp4_ast_impl_magic_number)

    let parse_interf = (parse FanConfig.camlp4_ast_intf_magic_number)

   end : FanSig.Parser(Ast).S)
