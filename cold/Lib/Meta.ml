let meta_loc _loc location =
  let (a,b,c,d,e,f,g,h) = FanLoc.to_tuple location in
  `App
    (_loc,
      (`Id
         (_loc,
           (`Dot (_loc, (`Uid (_loc, "FanLoc")), (`Lid (_loc, "of_tuple")))))),
      (`Par
         (_loc,
           (`Com
              (_loc, (`Str (_loc, (String.escaped a))),
                (`Com
                   (_loc,
                     (`Com
                        (_loc,
                          (`Com
                             (_loc,
                               (`Com
                                  (_loc,
                                    (`Com
                                       (_loc,
                                         (`Com
                                            (_loc,
                                              (`Int (_loc, (string_of_int b))),
                                              (`Int (_loc, (string_of_int c))))),
                                         (`Int (_loc, (string_of_int d))))),
                                    (`Int (_loc, (string_of_int e))))),
                               (`Int (_loc, (string_of_int f))))),
                          (`Int (_loc, (string_of_int g))))),
                     (if h
                      then `Id (_loc, (`Lid (_loc, "true")))
                      else `Id (_loc, (`Lid (_loc, "false")))))))))))