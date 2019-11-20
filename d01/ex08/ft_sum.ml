let ft_sum f i_d i_u =
	if i_u < i_d then nan
	else let rec sum i res =
		if i > i_u then res
		else sum (i + 1) (res +. f i)
	in sum i_d 0.0

let () =
	print_float (ft_sum (fun i -> float_of_int (i * i)) 11 10);
	print_char '\n';
	print_float(ft_sum (fun i -> float_of_int (i * i)) 10 10);
	print_char '\n';
	print_float(ft_sum (fun i -> float_of_int (i * i)) 1 10);
	print_char '\n';
	print_float(ft_sum (fun i -> float_of_int (i * i + 2 * i + 3)) 2 4);
	print_char '\n'
