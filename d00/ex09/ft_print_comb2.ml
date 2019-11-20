let ft_print_comb2 () =
	let rec second x y =
		if x < 10 then print_int 0;
		print_int x;
		print_char ' ';
		if y < 10 then print_int 0;
		print_int y;
		if x = 98 && y = 99
		then print_char '\n'
		else (print_char ','; print_char ' ');
		if y < 99 then second x (y + 1)
	in let rec first x y =
		second x y;
		if x < 98 then first (x + 1) (x + 2)
	in first 0 1

let () =
	ft_print_comb2 ()