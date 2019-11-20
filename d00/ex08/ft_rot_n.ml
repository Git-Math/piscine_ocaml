let ft_rot_n n str =
	let a_ascii = int_of_char 'a'
	in let cap_a_ascii = int_of_char 'A'
	in let rot_n c =
		if c >= 'a' && c <= 'z'
			then char_of_int (((int_of_char c) - a_ascii + n) mod 26 + a_ascii)
		else if c >= 'A' && c <= 'Z'
			then char_of_int (((int_of_char c) - cap_a_ascii + n) mod 26 + cap_a_ascii)
		else
			c
	in String.map rot_n str

let () =
	print_endline (ft_rot_n 1 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 13 "abcdefghijklmnopqrstuvwxyz");
	print_endline (ft_rot_n 42 "0123456789");
	print_endline (ft_rot_n 2 "OI2EAS67B9");
	print_endline (ft_rot_n 0 "Damned");
	print_endline (ft_rot_n 42 "");
	print_endline (ft_rot_n 1 "NBzlk qnbjr !")