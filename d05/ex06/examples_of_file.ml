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

let () =
  let rec print_array a i =
    if i < Array.length a then
      begin
        print_float a.(i);
        if i = Array.length a - 1 then print_string "|]" else print_string "; ";
        print_array a (i + 1)
      end
  in
  let print l =
    print_char '[';
    let rec print_list = function
      | (a, s)::tail ->
         begin
           print_string "([|";
           print_array a 0;
           print_string (", " ^ s ^ ")");
           if tail <> [] then print_string "; ";
           print_list tail
         end
      | _ -> ()
    in print_list l;
       print_endline "]"     
  in print (examples_of_file "ionosphere.test.csv");
     print (examples_of_file "ionosphere.train.csv")
