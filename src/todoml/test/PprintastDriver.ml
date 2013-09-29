open Format



let test_file file =
  let chin = open_in file in begin
    let ast =
      try (Parse.implementation (Lexing.from_channel chin))
      with e ->
        prerr_endline
          (sprintf "input file is invalid error:%s" (Printexc.to_string e));
        raise e in
    let str = AstPrint.string_of_structure ast in
    print_endline str;
    let new_ast = Parse.implementation (Lexing.from_string str) in
    (if AstEq.eq_structure (ast, new_ast) then
      prerr_endline "passed."
    else begin
      prerr_endline "failed!!";
    end);
    close_in chin  
  end

let _ =
  let len = Array.length Sys.argv -1 in 
  for i = 1 to len do
    prerr_endline
      (sprintf "..............testing file %s......................" Sys.argv.(i));
    try test_file Sys.argv.(i)
    with e -> prerr_endline
        (sprintf
           ".................failed in %s error %s............"
           Sys.argv.(i)
           (Printexc.to_string e)
        )
  done (* let chin = open_in Sys.argv.(1) in  *)




    















