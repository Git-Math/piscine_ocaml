let ft_string_all func str =
	let len = String.length str
	in let rec string_all i =
		if i < len then func (String.get str i) && string_all (i + 1) else true
	in string_all 0

let is_digit c =
	c >= '0' && c <= '9'

let () =
	print_endline (if ft_string_all is_digit "" then "true" else "false");
	print_endline (if ft_string_all is_digit "0123456789" then "true" else "false");
	print_endline (if ft_string_all is_digit "2EAS67B" then "true" else "false");
	print_endline (if ft_string_all is_digit "a123456789" then "true" else "false");
	print_endline (if ft_string_all is_digit "012345678z" then "true" else "false")