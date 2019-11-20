let rec ft_countdown count =
	if count <= 0
	then
		begin
			print_int (0);
			print_char '\n'
		end
	else
		begin
			print_int count;
			print_char '\n';
			ft_countdown (count - 1)
		end

let () =
	ft_countdown (-1);
	ft_countdown 0;
	ft_countdown 1;
	ft_countdown 3;
	ft_countdown 10