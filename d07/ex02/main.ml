let () =
  let p = new People.people ("Susan") in
  let d = new Doctor.doctor "Doctor Who" p 73
  and da = new Dalek.dalek
  and da1 = new Dalek.dalek in
  print_endline da1#to_string;
  d#talk;
  da#talk;
  da#exterminate p;
  print_endline da#to_string;
  da#talk;
  da#exterminate p;
  print_endline da#to_string;
  d#use_sonic_screwdriver;
  da#die
  
