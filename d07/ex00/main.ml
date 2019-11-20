let () =
  let p = new People.people ("Susan") in
  print_endline p#to_string;
  p#talk;
  p#die
