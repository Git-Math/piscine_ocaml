module Save =
  struct
    let file = "save.itama"
    let get_save () =
      try let save_in = open_in file in
          try let line = input_line save_in in
              let l = String.split_on_char ';' line in
              if List.length l <> 4 then failwith ""
              else let rec loop = function
                     | [] -> []
                     | head::tail -> let v = int_of_string head in
                                     if v > 0 && v <= 100 then (float_of_int v) /. 100. :: loop tail
                                     else failwith ""
                   in
                   let res = loop l in
                   close_in save_in;
                   res
          with _ -> close_in save_in; [1.; 1.; 1.; 1.]
      with _ -> [1.; 1.; 1.; 1.]
    let save l =
      try let save_out = open_out file in
          let rec s = function
            | [] -> ()
            | head::[] -> output_string save_out (string_of_int (int_of_float (head *. 100.)) ^ "\n")
            | head::next::tail -> output_string save_out (string_of_int (int_of_float (head *. 100.)) ^ ";"); s (next::tail)
          in s l;
             close_out save_out
      with _ -> print_endline "save failed"  
  end
