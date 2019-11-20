let eu_dist a b =
  let l = Array.length a in
  let rec dist i =
    if i >= l then 0.
    else (a.(i) -. b.(i)) ** 2. +. dist (i + 1)
  in sqrt (dist 0)

let examples_of_file file =
  let ic = open_in file in
  let rec ex ic =
    let line = begin try input_line ic with
            | Sys_error err -> ""
            | End_of_file -> ""
            end
    in
    if line = "" then (close_in ic; [])
    else let a_with_end_letter = Array.of_list (String.split_on_char ',' line) in
         let end_letter = a_with_end_letter.(Array.length a_with_end_letter - 1)
         and a_without_end_letter = Array.sub a_with_end_letter 0 (Array.length a_with_end_letter - 1) in
         begin try let a_float = Array.map float_of_string a_without_end_letter in
                   [(a_float, end_letter)] @ ex ic
               with Failure err -> (close_in ic; [])
         end
  in ex ic

type radar = float array * string

let one_nn (rl : radar list) (r : radar) =
  let rec one rl result min_dist =
    match rl with
    | head::tail -> if head <> r && eu_dist (fst head) (fst r) < min_dist then one tail (snd head) (eu_dist (fst head) (fst r)) else one tail result min_dist
    | _ -> result
  in one rl "" max_float

let () =
  print_endline (one_nn (examples_of_file "../ex06/ionosphere.test.csv") (([|1.; 0.; 0.90071; 0.01773; 1.; -0.01773; 0.90071; 0.00709; 0.84752; 0.05674; 1.; 0.03546; 0.97872; 0.01064; 0.97518; 0.03546; 1.; -0.03191; 0.89716; -0.03191; 0.86170; 0.07801; 1.; 0.09220; 0.90071; 0.04610; 0.94305; 0.03247; 0.94681; 0.02482; 1.; 0.01064; 0.93617; 0.04228|], "g")));
  print_endline (one_nn ([([|2.; 2.|], "2"); ([|3.; 3.|], "3"); ([|5.; 5.|], "5"); ([|7.; 7.|], "7")]) (([|5.9; 5.9|], "6")))
