let encode l =
	let rec loop l count =
		match l with
			| [] -> []
			| head::next::tail when head = next -> loop (next::tail) (count + 1)
			| head::tail -> (count, head)::(loop tail 1)
	in loop l 1

let () =
	let rec print_int_list = function
		| [] -> print_endline ""
		| head::[] -> print_int head; print_int_list []
		| head::tail -> print_int head; print_string "; "; print_int_list tail
	in let rec print_int_int_list = function
		| [] -> print_endline ""
		| (x, y)::[] -> print_char '('; print_int x; print_string ", "; print_int y; print_char ')'; print_int_int_list []
		| (x, y)::tail -> print_char '('; print_int x; print_string ", "; print_int y; print_char ')'; print_string "; "; print_int_int_list tail
	in let l0 = []
	and l1 = [1]
	and l2 = [1; 2; 2; 3; 3; 3]
	and l3 = [1; 2; 2; 3; 3; 3; 1; 1; 4; 4; 3; 2; 42]
	and l4 = [1; 1; 1; 2; 2; 2]
	in
	print_int_list l0;
	print_int_int_list (encode l0);
	print_int_list l1;
	print_int_int_list (encode l1);
	print_int_list l2;
	print_int_int_list (encode l2);
	print_int_list l3;
	print_int_int_list (encode l3);
	print_int_list l4;
	print_int_int_list (encode l4)
