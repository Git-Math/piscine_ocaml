let sequence n =
	if n <= 0 then ""
	else let rec list_to_string l str =
		match l with
			| [] -> str
			| head::tail -> list_to_string tail (str ^ (string_of_int head))
	in let rec loop l count =
		match l with
			| [] -> []
			| head::next::tail when head = next -> loop (next::tail) (count + 1)
			| head::tail -> count::head::(loop tail 1)
	in let rec seq n l =
		if n = 1 then list_to_string l ""
		else seq (n - 1) (loop l 1)
	in seq n [1]
		
let () =
	print_endline (sequence (-1));
	print_endline (sequence 0);
	print_endline (sequence 1);
	print_endline (sequence 2);
	print_endline (sequence 3);
	print_endline (sequence 4);
	print_endline (sequence 5);
	print_endline (sequence 6);
	print_endline (sequence 7)