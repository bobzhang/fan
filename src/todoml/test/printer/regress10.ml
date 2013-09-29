rule "byte stdlib in mixed mode"
  begin fun env build ->
    let (_ : Command.t) = 3  in
    import_stdlib_contents build ["cmi"];
    Nop
  end;;

