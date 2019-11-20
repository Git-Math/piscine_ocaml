let repeat_string ?(str = "x") n =
	if n < 0 then "Error"
    else let rec repeat n result =
        if n = 0 then result
        else repeat (n - 1) (result ^ str)
    in repeat n ""

let () =
	print_endline (repeat_string (-1));
	print_endline (repeat_string 0);
	print_endline (repeat_string ~str:"Toto" 1);
	print_endline (repeat_string 2);
	print_endline (repeat_string ~str:"a" 5);
	print_endline (repeat_string ~str:"what" 3)