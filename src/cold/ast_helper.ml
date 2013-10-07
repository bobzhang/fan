let rec of_listr f xs =
  match xs with
  | [] -> invalid_arg "of_list empty"
  | t::[] -> t
  | t::ts -> f t (of_listr f ts)