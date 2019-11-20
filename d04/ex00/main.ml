let () =
  let rec print = function
    | [] -> ()
    | head::tail -> print_endline ((Color.toString head) ^ ": " ^ (Color.toStringVerbose head)); print tail
  in print Color.all
