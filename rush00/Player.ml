type coord = {l: int ; c: int}

let coordToPos lst length =
	let position l len  =
		match l with
			| h::n::q ->
				{l = ((h - 1) / len + ((n - 1) /len ) * len); c = ((h - 1) mod len + ((n - 1) mod len) * len)}
			| _ -> {l = 1 ; c = 1}
	in
	position lst length

let rec gameBoardSize () = 
	let strValue = read_line ()
	in
	if Tools.stringIsInt(strValue) then
		begin
			let v = int_of_string(strValue)
			in
			if v > 2 && v < 10 then
				v * v
			else
				begin
					print_endline "Invalid size (>2 && <10).";
					gameBoardSize ()
				end
		end
	else
		begin
			print_endline "Invalid number.";
			gameBoardSize ()
		end

let placeChar c gameBoard coor pName =
	let rec getMorpion lst res i =
		match lst with
			| h::q -> 
				if i = coor.l then
					getMorpion q (res @ [(Morpion.buildNewMorpionStr h c coor.c pName coor.l)]) (i + 1)
				else
					getMorpion q (res @ [h]) (i + 1)
			| [] -> res
	in
	let grid = (getMorpion gameBoard [] 0) in
	let rec whoWin lst p res =
		match lst with
			| h::q -> 
				if h = (Morpion.SoloMorpion.big_circle (int_of_float(sqrt (float_of_int (List.length grid))))) then
					whoWin q p (res ^ (String.make 1 'O'))
				else if h = (Morpion.SoloMorpion.big_cross  (int_of_float(sqrt (float_of_int (List.length grid))))) then
					whoWin q p (res ^ (String.make 1 'X'))
				else
					whoWin q p (res ^ (String.make 1 '-'))
			| _ ->
				let rec aa str index =
					if index < (String.length str) && ((String.get str index) = 'O' ||  (String.get str index) = 'X') then
						if (Morpion.isDraw res) || (Morpion.hasWin res 'X' index (int_of_float(sqrt (float_of_int (String.length res)))))|| (Morpion.hasWin res 'O' index (int_of_float(sqrt (float_of_int (String.length res))))) then
							begin
								print_endline (Morpion.printMultiMorpion grid);
								print_endline (pName ^ " YOU WIN BITCH");
								Morpion.buildEmptyMultiMorpion (String.length res)
							end
						else

							aa str (index + 1)
					else if index < (String.length str) && (String.get str index) = '-' then
						aa str (index + 1)
					else
						grid


				in
				aa res 0


	in
	whoWin grid c ""


let rec validNamePlayer1 p1 =
	if p1 = "" then
		begin
			print_endline "Player 1(O) name can't be empty.";
			validNamePlayer1 (read_line ());
		end
	else
		p1

let rec validNamePlayer2 p1 p2 =
	if p1 = p2 then
		begin
			print_endline "Player 2 (X) name is invalid.";
			validNamePlayer2 p1 (read_line ());
		end
	else if p2 = "" then
		begin
			print_endline "Player 2(X) name can't be empty.";
			validNamePlayer2 p1 (read_line ());
		end
	else
		p2

let rec coordStringValidity lst res gameBoard =
	match lst with
		| h::q ->
			if (Tools.stringIsInt h) && (int_of_string h) > 0 && (int_of_string h) <= (List.length gameBoard) then
					coordStringValidity (q) (res @ [(int_of_string h)]) gameBoard
			else
				begin
					print_endline "Invalid format.";
					{l = -1 ; c = -1}
				end
		| [] -> 
			if (List.length res) <> 2 then
				begin
					print_endline "Invalid format.";
					{l = -1 ; c = -1}
				end
			else
				coordToPos res (int_of_float(sqrt (float_of_int (List.length gameBoard))))



let play str gameBoard =
	let coord = (String.split_on_char ' ' str)
	in
	let rec coordCanBePlayed position board i =
		match board with
			| h::q -> 
				if i = position.l && h <> (Morpion.SoloMorpion.big_cross (int_of_float(sqrt (float_of_int (String.length h))))) && h <> (Morpion.SoloMorpion.big_circle (int_of_float(sqrt (float_of_int (String.length h))))) then
					if (String.get h position.c) = '-' then
						true
					else
						false
				else
					coordCanBePlayed position q (i + 1)
			| [] -> false
	in
	coordCanBePlayed (coordStringValidity coord [] gameBoard) gameBoard 0
