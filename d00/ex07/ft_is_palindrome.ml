let ft_is_palindrome str =
	let len = String.length str
	in let rec is_palindrome i =
		if i < (len - 1 - i) then String.get str i = String.get str (len - 1 - i) && is_palindrome (i + 1) else true
	in is_palindrome 0

let () =
	print_endline (if ft_is_palindrome "radar" then "true" else "false");
	print_endline (if ft_is_palindrome "madam" then "true" else "false");
	print_endline (if ft_is_palindrome "car" then "true" else "false");
	print_endline (if ft_is_palindrome "" then "true" else "false")