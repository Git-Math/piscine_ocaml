let ft_print_rev str =
	let len = String.length str
	in let rec print_rev str i =
		if i < (len - 1) then print_rev str (i + 1);
		if len > 0 then print_char (String.get str i)
	in print_rev str 0;
	print_char '\n'

let () =
	ft_print_rev "Hello world !";
	ft_print_rev "";
	ft_print_rev "e"