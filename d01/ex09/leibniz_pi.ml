let leibniz_pi min_delta =
	if min_delta <= 0. then (-1)
	else let pi = 4. *. atan 1.
	in let f i =
		(if (i mod 2) = 0 then 1. else -1.) /. (2. *. (float_of_int i) +. 1.)
	in let rec loop i cumul =
		let delta = pi -. (4. *. cumul)
		in let abs_delta = if delta > 0. then delta else -.delta
		in if abs_delta <= min_delta then i
		else loop (i + 1) (cumul +. f i)
	in loop 0 0.

let () =
	print_int (leibniz_pi (-1.));
	print_char '\n';
	print_int (leibniz_pi 1.);
	print_char '\n';
	print_int (leibniz_pi 0.9);
	print_char '\n';
	print_int (leibniz_pi 0.8);
	print_char '\n';
	print_int (leibniz_pi 0.7);
	print_char '\n';
	print_int (leibniz_pi 0.6);
	print_char '\n';
	print_int (leibniz_pi 0.5);
	print_char '\n';
	print_int (leibniz_pi 0.4);
	print_char '\n';
	print_int (leibniz_pi 0.3);
	print_char '\n';
	print_int (leibniz_pi 0.2);
	print_char '\n';
	print_int (leibniz_pi 0.1);
	print_char '\n';
	print_int (leibniz_pi 0.01);
	print_char '\n'