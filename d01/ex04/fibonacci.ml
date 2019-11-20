let fibonacci n =
	let rec fib n prev curr =
		if n < 0 then (-1)
		else if n = 0 then 0
		else if n = 1 then curr
		else fib (n - 1) curr (prev + curr)
	in fib n 0 1

let () =
	print_int (fibonacci (-42));
	print_char '\n';
	print_int (fibonacci 0);
    print_char '\n';
	print_int (fibonacci 1);
    print_char '\n';
	print_int (fibonacci 3);
    print_char '\n';
	print_int (fibonacci 6);
    print_char '\n';
	print_int (fibonacci 42);
    print_char '\n'