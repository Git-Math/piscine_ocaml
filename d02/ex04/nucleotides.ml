type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | None
type nucleotide = {
	p : phosphate;
	d : deoxyribose;
	n : nucleobase
} 

let generate_nucleotide c =
	{
		p = "phosphate";
		d = "desoxyribose";
		n = match c with
			| 'A' -> A
			| 'T' -> T
			| 'C' -> C
			| 'G' -> G
			| _ -> None
	}

let () =
	let print_nucleotide nuc =
		print_endline nuc.p;
		print_endline nuc.d;
		print_endline (match nuc.n with
			| A -> "A"
            | T -> "T"
            | C -> "C"
            | G -> "G"
            | None -> "None")
	in
	print_nucleotide (generate_nucleotide 'A');
	print_nucleotide (generate_nucleotide 'T');
	print_nucleotide (generate_nucleotide 'C');
	print_nucleotide (generate_nucleotide 'G');
	print_nucleotide (generate_nucleotide 'B')