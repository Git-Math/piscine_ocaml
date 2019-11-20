let jokes av =
  let ic = open_in av.(1) in
  let s = input_line ic in
  close_in ic;
  let l = String.split_on_char '@' s in
  let a = Array.make (List.length l) "" in
  for i = 0 to Array.length a - 1 do
    Array.set a i (List.nth l i)
  done;
  Random.self_init ();
  print_endline (a.(Random.int (Array.length a)))

let () =
  if Array.length Sys.argv = 2 then jokes Sys.argv

