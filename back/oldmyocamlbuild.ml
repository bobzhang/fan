 let buffer_size = 8192
  let buffer = String.create buffer_size
  let file_copy input_name output_name =
    Unix.(
    let () = Log.dprintf 1 "%s --> %s\n" input_name output_name in 
    let fd_in = openfile input_name [O_RDONLY] 0 in
    let fd_out = openfile output_name [O_WRONLY; O_CREAT; O_TRUNC] 0o666 in
    let rec copy_loop () =
      match read fd_in buffer 0 buffer_size with
      |  0 -> ()
      | r -> ignore (write fd_out buffer 0 r); copy_loop () in
    copy_loop ();
    close fd_in;
    close fd_out)

  let update f path =
    let (//) = Filename.concat in 
    Sys.readdir path |> Array.iter
    (fun x ->
      let x = (if path<>"." then path // x else x ) in 
      if f x  then
        let target =  "_build"  // x in
        try 
          let {Unix.st_mtime=m_x ;_ } = Unix.stat x in
          if Sys.file_exists target then
            let {Unix.st_mtime=m_y;_} = Unix.stat target in
            if m_x > m_y then begin 
              Sys.remove target;
              file_copy x target 
            end 
            else ()
          else begin
            let subdir = "_build"// path in 
            if not (Sys.file_exists subdir) then  (* FIXME  more precise *)
              Unix.(mkdir subdir 0o755)
            else ();
            (file_copy x target)
          end
        with
          e -> begin
            Format.eprintf "%s" (Printexc.to_string e);
            Format.eprintf "ignoreing a file %s" x ;
          end 
    )
