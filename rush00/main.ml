let () =
	print_endline "Chose the board size.";
	let gameBoard = Morpion.buildEmptyMultiMorpion (Player.gameBoardSize ())
	in
	print_endline "Player 1 name.";
	let player1 = Player.validNamePlayer1 (read_line ())
	in
	print_endline "Player 2 name.";
	let player2 = Player.validNamePlayer2 player1 (read_line ())
	in
	let rec runGame gameBoard p1 p2 turn displayBoard =
		if displayBoard = true then
			print_string(Morpion.printMultiMorpion gameBoard);
		if turn = true then
			print_endline ("\n" ^ p1 ^ "'s turn to play.")
		else
			print_endline ("\n" ^ p2 ^ "'s turn to play.");
		let coor = (read_line ())
		in
		let recordCoord = (Player.coordStringValidity (String.split_on_char ' ' coor) [] gameBoard)
		in
		let test = {Player.l = -1 ; Player.c = -1} in
		if test = recordCoord then
			if turn = true then
				runGame gameBoard player1 player2 true false
			else
				runGame gameBoard player1 player2 false false
		else if (Player.play coor gameBoard) then
			if turn = true then				
				runGame (Player.placeChar 'O' gameBoard recordCoord p1) player1 player2 false true
			else
				runGame (Player.placeChar 'X' gameBoard recordCoord p2) player1 player2 true true		
		else
			if turn = true then
				runGame gameBoard player1 player2 true false
			else
				runGame gameBoard player1 player2 false false;
	in
	runGame gameBoard player1 player2 true true
