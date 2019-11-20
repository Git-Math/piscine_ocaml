let gray n =
	let rec print_list = function
        | [] -> print_endline ""
        | head::[] -> print_string head; print_list []
        | head::tail -> print_string head; print_string " "; print_list tail
	in let rec rev_list l1 l2 =
		match l1 with
			| [] -> l2
			| head::tail -> rev_list tail (head::l2)
	in let rec add_to_list l n =
		match l with
			| [] -> []
			| head::tail -> (n ^ head)::(add_to_list tail n)
	in let rec concat_list l1 l2 =
			match l1 with
				| [] -> l2
				| head::tail -> concat_list tail (head::l2)
	in let l = ["0"; "1"]
	in
	if n <= 0 then print_endline ""
	else if n = 1 then print_list l
	else let rec g l n =
		let rev_l = rev_list l []
		in let new_l = concat_list (rev_list (add_to_list l "0") []) (add_to_list rev_l "1")
		in if n > 2 then g new_l (n - 1)
		else print_list new_l
	in g l n

let () =
	gray (-1);
	gray 0;
	gray 1;
	gray 2;
	gray 3