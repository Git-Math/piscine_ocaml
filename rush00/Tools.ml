let rec list_car ch = 
	match ch with
		| "" -> []
		| ch -> (String.get ch 0 ) :: (list_car (String.sub ch 1 ( (String.length ch) - 1)))

let stringIsInt str =
	if str = "" then
		false
	else
		let rec isInt lst =
			match lst with
				| h::q -> 
					if int_of_char(h) < 58 && int_of_char(h) > 47 then
						isInt q
					else 
						false
				| [] -> true

		in isInt (list_car str)
