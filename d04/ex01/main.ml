let () =
  let rec print = function
    | [] -> ()
    | head::tail -> print_endline ("value int: " ^ (string_of_int (Value.toInt head)) ^ "; value string: " ^ (Value.toString head) ^ "; value string verbose: " ^ (Value.toStringVerbose head) ^ "; next: " ^
                                     (try Value.toString (Value.next head) with invalid_arg -> "invalid arg") ^ "; previous: " ^
                                       try Value.toString (Value.previous head) with invalid_arg -> "invalid arg");
                    print tail
  in print Value.all
