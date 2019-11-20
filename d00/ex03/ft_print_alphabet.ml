let ft_print_alphabet () =
	let a_ascii = int_of_char 'a'
	in let z_ascii = int_of_char 'z'
	in let rec print_letter letter_ascii =
		print_char (char_of_int letter_ascii);
		if letter_ascii < z_ascii
		then print_letter (letter_ascii + 1)
		else print_char '\n'
	in print_letter a_ascii

let () =
	ft_print_alphabet ()