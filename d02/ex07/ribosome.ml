type phosphate = string
type deoxyribose = string
type nucleobase = A | T | C | G | U | None
type nucleotide = {
    p : phosphate;
    d : deoxyribose;
    n : nucleobase
}

type helix = nucleotide list

type rna = nucleobase list

type aminoacid = Stop | Ala | Arg | Asn | Asp | Cys | Gln | Glu | Gly | His | Ile | Leu | Lys | Met | Phe | Pro | Ser | Thr | Trp | Tyr | Val

type protein = aminoacid list

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
            | U -> "U"
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
                | _ -> None
        } :: (complementary_helix tail)

let rec generate_rna l =
    match l with
        | [] -> []
        | head::tail -> (match head.n with
            | A -> U
            | T -> A
            | C	-> G
            | G -> C
            | _ -> None) :: (generate_rna tail)

let rec generate_bases_triplets l =
	match l with
		| first::second::third::tail -> (first, second, third) :: generate_bases_triplets tail
		| _ -> []

let rec string_of_protein l =
	match l with
		| [] -> ""
		| head::tail -> (match head with
			| Stop -> "End of translation"
			| Ala -> "Alanine"
			| Arg -> "Arginine"
			| Asn -> "Asparagine"
			| Asp -> "Aspartique"
			| Cys -> "Cysteine"
			| Gln -> "Glutamine"
			| Glu -> "Glutamique"
			| Gly -> "Glycine"
			| His -> "Histidine"
			| Ile -> "Isoleucine"
			| Leu -> "Leucine"
			| Lys -> "Lysine"
			| Met -> "Methionine"
			| Phe -> "Phenylalanine"
			| Pro -> "Proline"
			| Ser -> "Serine"
			| Thr -> "Threonine"
			| Trp -> "Tryptophane"
			| Tyr -> "Tyrosine"
			| Val -> "Valine") ^ (if tail = [] then "" else "; ") ^ (string_of_protein tail)

let decode_arn l =
	let rec loop l =
		match l with
			| (U, A, A)::tail | (U, A, G)::tail | (U, G, A)::tail -> Stop::[]
			| (G, C, A)::tail | (G, C, C)::tail | (G, C, G)::tail | (G, C, U)::tail -> Ala::(loop tail)
			| (A, G, A)::tail | (A, G, G)::tail | (C, G, A)::tail | (C, G, C)::tail | (C, G, G)::tail | (C, G, U)::tail -> Arg::(loop tail)
			| (A, A, C)::tail | (A, A, U)::tail -> Asn::(loop tail)
			| (G, A, C)::tail | (G, A, U)::tail -> Asp::(loop tail)
			| (U, G, C)::tail | (U, G, U)::tail -> Cys::(loop tail)
			| (C, A, A)::tail | (C, A, G)::tail -> Gln::(loop tail)
			| (G, A, A)::tail | (G, A, G)::tail -> Glu::(loop tail)
			| (G, G, A)::tail | (G, G, C)::tail | (G, G, G)::tail | (G, G, U)::tail -> Glu::(loop tail)
			| (C, A, C)::tail | (C, A, U)::tail -> His::(loop tail)
			| (A, U, A)::tail | (A, U, C)::tail | (A, U, U)::tail -> Ile::(loop tail)
			| (C, U, A)::tail | (C, U, C)::tail | (C, U, G)::tail | (C, U, U)::tail | (U, U, A)::tail | (U, U, G)::tail -> Leu::(loop tail)
			| (A, A, A)::tail | (A, A, G)::tail -> Lys::(loop tail)
			| (A, U, G)::tail -> Met::(loop tail)
			| (U, U, C)::tail | (U, U, U)::tail -> Phe::(loop tail)
			| (C, C, C)::tail | (C, C, A)::tail | (C, C, G)::tail | (C, C, U)::tail -> Pro::(loop tail)
			| (U, C, A)::tail | (U, C, C)::tail | (U, C, G)::tail | (U, C, U)::tail | (A, G, U)::tail | (A, G, C)::tail -> Ser::(loop tail)
			| (A, C, A)::tail | (A, C, C)::tail | (A, C, G)::tail | (A, C, U)::tail -> Thr::(loop tail)
			| (U, G, G)::tail -> Trp::(loop tail)
			| (U, A, C)::tail | (U, A, U)::tail -> Tyr::(loop tail)
			| (G, U, A)::tail | (G, U, C)::tail | (G, U, G)::tail | (G, U, U)::tail -> Val::(loop tail)
			| _ -> []
	in loop (generate_bases_triplets l)

let () =
	let rec rna_to_string l =
		match l with
			| [] -> ""
			| head::tail -> (match head with
			| A -> "A"
			| T -> "T"
			| C -> "C"
			| G -> "G"
			| U-> "U"
			| None -> "None") ^ (rna_to_string tail)
	in let h = generate_helix 113
	in print_endline (helix_to_string h);
	let r = generate_rna h
	in print_endline (rna_to_string r);
	print_endline (string_of_protein (decode_arn r))