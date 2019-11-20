let eu_dist a b =
  let l = Array.length a in
  let rec dist i =
    if i >= l then 0.
    else (a.(i) -. b.(i)) ** 2. +. dist (i + 1)
  in sqrt (dist 0)

let () =
  let a0 = [||]
  and b0 = [||]
  and a1 = [|62.|]
  and b1 = [|21.|]
  and a2 = [|1.; 1.|]
  and b2 = [|5.; -1.|]
  and a3 = [|1.2; 2.3; 3.4|]
  and b3 = [|2.1; 3.2; 4.3|]
  and a4 = [|2.; 3.; 5.; 7.|]
  and b4 = [|11.; 13.; 17.; 19.|] in
  print_float (eu_dist a0 b0);
  print_endline "";
  print_float (eu_dist a1 b1);
  print_endline "";
  print_float (eu_dist a2 b2);
  print_endline "";
  print_float (eu_dist a3 b3);
  print_endline "";
  print_float (eu_dist a4 b4);
  print_endline ""
