let rec converges f x n =
	if n < 0 then false
	else if x = (f x) then true
	else if n = 0 then false
	else converges f (f x) (n - 1)

let () =
	print_endline (if converges (( * ) 2) 2 (-42) then "true" else "false");
	print_endline (if converges (fun x -> x / 2) 2 0  then "true" else "false");
	print_endline (if converges (( * ) 2) 2 5 then "true" else "false");
	print_endline (if converges (fun x -> x / 2) 2 3 then "true" else "false");
	print_endline (if converges (fun x -> x / 2) 2 2 then "true" else "false");
