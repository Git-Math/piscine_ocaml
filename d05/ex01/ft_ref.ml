type 'a ft_ref = { mutable contents : 'a }

let return c =
  { contents = c }

let get r =
  r.contents

let set r c =
  r.contents <- c

let bind r (f : 'a -> 'b ft_ref) =
  f r.contents

let () =
  let r1 = return 42
  in print_int (get r1);
     print_endline "";
     set r1 21;
     print_int (get r1);
     print_endline "";
     print_int (get (bind r1 (fun c -> return (2 * c))));
     print_endline "";
     let r2 = return 'D'
     in print_char (get r2);
     print_endline "";
     set r2 'a';
     print_char (get r2);
     print_endline "";
     print_char (get (bind r1 (fun c -> return 'z')));
     print_endline "";
