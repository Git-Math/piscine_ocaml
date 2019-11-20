let () =
  print_endline (string_of_int (Watchtower.Watchtower.zero));
  print_endline (string_of_int(Watchtower.Watchtower.add 2 5));
  print_endline (string_of_int(Watchtower.Watchtower.add 3 12));
  print_endline (string_of_int(Watchtower.Watchtower.sub 7 5));
  print_endline (string_of_int(Watchtower.Watchtower.sub 3 (-12)));
  print_endline (string_of_int(Watchtower.Watchtower.add 42 (Watchtower.Watchtower.sub 21 84)))
