let ft_print_comb () =
	let rec one x y z =
	print_int x;
	print_int y;
	print_int z;
	if x = 7 && y = 8 && z = 9
	then print_string "\n"
	else print_string ", ";
	if z < 9
	then one x y (z + 1)
	in let rec ten x y z =
		one x y z;
		if y < 8
		then ten x (y + 1) (y + 2)
	in let rec hundred x y z =
	ten x y z;
	if x < 7
	then hundred (x + 1) (x + 2) (x + 3)
	in hundred 0 1 2

let () =
	ft_print_comb ()