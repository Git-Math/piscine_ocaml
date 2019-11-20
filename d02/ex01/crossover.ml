let crossover l1 l2 =
	let rec second l v =
		match l with
			| [] -> []
			| head::tail when head = v -> [v]
			| head::tail -> second tail v
	in let rec first l res_l =
		match l with
			| [] -> res_l
			| head::tail -> first tail (res_l @ second l2 head)
	in first l1 []

let () =
	let rec print_int_list = function
		| [] -> print_endline ""
		| head::[] -> print_int head; print_int_list []
		| head::tail -> print_int head; print_string "; "; print_int_list tail
	in let l0 = []
	and l1 = [2; 3; 5; 7; 11; 13; 17; 19; 23; 29]
	and l2 = [1; 2; 3; 4; 5; 6; 7; 8; 9; 10; 11; 12; 13; 14; 15; 16; 17]
	in
	print_int_list l0;
	print_int_list l1;
	print_int_list (crossover l0 l1);
	print_int_list l2;
	print_int_list l0;
	print_int_list (crossover l1 l0);
	print_int_list l1;
	print_int_list l2;
	print_int_list (crossover l1 l2);
	print_int_list l2;
	print_int_list l1;
	print_int_list (crossover l2 l1)