let app0 mt1 mt2 =
  match (mt1, mt2) with
  | (`Id (_loc,i1),`Id (_,i2)) -> `Id (_loc, (`IdApp (_loc, i1, i2)))
  | _ -> raise Stream.Failure
let acc0 mt1 mt2 =
  match (mt1, mt2) with
  | (`Id (_loc,i1),`Id (_,i2)) -> `Id (_loc, (`Dot (_loc, i1, i2)))
  | _ -> raise Stream.Failure
let app mt1 mt2 =
  match (mt1, mt2) with
  | (`Id (_loc,i1),`Id (_,i2)) -> `Id (_loc, (`IdApp (_loc, i1, i2)))
  | _ -> invalid_arg "Fan_module_type app"
let acc mt1 mt2 =
  match (mt1, mt2) with
  | (`Id (_loc,i1),`Id (_,i2)) -> `Id (_loc, (`Dot (_loc, i1, i2)))
  | _ -> invalid_arg "ModuleType.acc"