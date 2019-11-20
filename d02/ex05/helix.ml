type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
	p : phosphate;
	d : deoxyribose;
	n : nucleobase
}

type helix = nucleotide list

let generate_helix n =
	Random.self_init ();
	let rec gen n =
		if n <= 0 then []
		else {
			p = "phosphate";
			d = "deoxyribose";
			n = match (Random.int 4) with
				| 0 -> A
				| 1 -> T
				| 2 -> C
				| _ -> G
		} :: gen (n - 1)
	in gen n

let rec helix_to_string l =
	match l with
		| [] -> ""
		| head::tail -> (match head.n with
			| A -> "A"
			| T -> "T"
			| C -> "C"
			| G -> "G"
			| None -> "None") ^ (helix_to_string tail)

let rec complementary_helix l =
	match l with
		| [] -> []
		| head::tail -> {
			p = "phosphate";
			d = "deoxyribose";
			n = match head.n with
				| A -> T
				| T -> A
				| C -> G
				| G -> C
				| None -> None	
		} :: (complementary_helix tail)

let () =
	let h0 = generate_helix (-1)
	in print_endline (helix_to_string h0);
	print_endline (helix_to_string (complementary_helix h0));
	let h1 = generate_helix 0
	in print_endline (helix_to_string h1);
	print_endline (helix_to_string (complementary_helix h1));
	let h2 = generate_helix 1
	in print_endline (helix_to_string h2);
	print_endline (helix_to_string (complementary_helix h2));	
	let h3 = generate_helix 7
	in print_endline (helix_to_string h3);
	print_endline (helix_to_string (complementary_helix h3));
	let h4 = generate_helix 13
	in print_endline (helix_to_string h4);
	print_endline (helix_to_string (complementary_helix h4))
