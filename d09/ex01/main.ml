let print_proj (p:App.App.project) =
  match p with (p1, p2, p3) -> print_endline ("project: " ^ p1 ^ ", status: " ^ p2 ^ ", grade: " ^ string_of_int p3)

let () =
  print_proj App.App.zero;
  print_proj (App.App.combine ("Oca", "failed", -42) ("ml", "succeed", 125));
  print_proj (App.App.combine ("Day", "failed", 60) ("09", "succeed", 100));
  print_proj (App.App.combine ("Rush", "success", 100) ("01", "succeed", 125));
  print_proj (App.App.fail ("Project_failed", "succeed", 125));
  print_proj (App.App.success App.App.zero)
  
