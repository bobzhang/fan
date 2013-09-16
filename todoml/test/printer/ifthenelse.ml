let check x msg =
  if ((start_bol x) < 0 || (stop_bol x) < 0 ||
      (start_off x) < 0 ||  (stop_off x) < 0)
  then begin
    eprintf "*** Warning: (%s) strange positions ***\n%a@\n" msg print x;
    false
  end
  else true
