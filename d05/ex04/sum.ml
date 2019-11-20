let sum f1 f2 =
  f1 +. f2

let () =
  print_float (sum 0.1 0.02);
  print_endline "";
  print_float (sum 17.02 24.98);
  print_endline ""
    
