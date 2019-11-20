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

let delete l i_del =
  let rec del l i =
    match l with
    | [] -> []
    | head::tail when i = i_del -> tail
    | head::tail -> head::(del tail (i + 1))
  in del l 0
  
let one_nn (rl : radar list) (r : radar) =
  let rec one l result min_dist i =
    match l with
    | head::tail -> if head <> r && eu_dist (fst head) (fst r) < min_dist then one tail (snd head, delete rl i) (eu_dist (fst head) (fst r)) (i + 1) else one tail result min_dist (i + 1)
    | _ -> result
  in one rl ("", []) max_float 0

let sort_l s1 s2 =
  int_of_char (s1.[0]) - int_of_char (s2.[0])

let find_str res =
  let sort_res = List.sort sort_l res in
  
  let rec find_s sort_res s n_s max_s max_n_s =
    match sort_res with
    | [] -> if n_s > max_n_s then s else max_s
    | head::tail -> if head = s then find_s tail s (n_s + 1) max_s max_n_s else if n_s > max_n_s then find_s tail head 1 s n_s else find_s tail head 1 max_s max_n_s
  in find_s sort_res "" 0 "" 0
        

let k_nn (rl : radar list) k (r : radar) =
  let rec k_n l i res =
    if i >= k then find_str res
    else match l with
    | [] -> ""
    | _ -> match (one_nn l r) with (a, b) -> k_n b (i + 1) (a::res)
  in k_n rl 0 []
   
let () =
  print_endline (k_nn (examples_of_file "../ex06/ionosphere.test.csv") 8 (([|1.; 0.; 0.90071; 0.01773; 1.; -0.01773; 0.90071; 0.00709; 0.84752; 0.05674; 1.; 0.03546; 0.97872; 0.01064; 0.97518; 0.03546; 1.; -0.03191; 0.89716; -0.03191; 0.86170; 0.07801; 1.; 0.09220; 0.90071; 0.04610; 0.94305; 0.03247; 0.94681; 0.02482; 1.; 0.01064; 0.93617; 0.02128|], "g")));
  print_endline (k_nn ([([|2.; 2.|], "g"); ([|3.; 3.|], "g"); ([|5.; 5.|], "b"); ([|7.; 7.|], "b")]) 3 (([|5.9; 5.9|], "g")));
  let rec accur l ok tot =
    match l with
    | [] -> (print_string "accuracy: "; print_float (ok /. tot); print_endline "")
    | (a, b)::tail -> if (k_nn (examples_of_file "../ex06/ionosphere.train.csv") 8 (a, b)) = b then accur tail (ok +. 1.) (tot +. 1.) else accur tail ok (tot +. 1.)
  in accur (examples_of_file "../ex06/ionosphere.train.csv") 0. 0.
