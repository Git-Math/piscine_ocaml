let () =
  let p = new People.people ("Susan") in
  let d = new Doctor.doctor "Doctor Who" p 73 in
  print_endline d#to_string;
  d#talk;
  let d1 = d#travel_in_time 2018 2045 in
  print_endline d#to_string;
  print_endline d1#to_string;
  d#use_sonic_screwdriver;
  let d2 = d#lose_hp 58 in
  print_endline d#to_string;
  print_endline d2#to_string;
  let d3 = d2#lose_hp 58 in
  print_endline d2#to_string;
  print_endline d3#to_string
  
