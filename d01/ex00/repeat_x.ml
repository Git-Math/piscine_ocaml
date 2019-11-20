let repeat_x n =
	if n < 0 then "Error"
	else let rec repeat n result =
		if n = 0 then result
		else repeat (n - 1) (result ^ "x")
	in repeat n ""

let () =
	print_endline (repeat_x (-1));
	print_endline (repeat_x 0);
	print_endline (repeat_x 1);
	print_endline (repeat_x 2);
	print_endline (repeat_x 5)