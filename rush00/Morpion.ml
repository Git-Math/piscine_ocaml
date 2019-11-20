module SoloMorpion = struct

	type sMorpion = string

	let buildSoloMorpion size =
		let rec build len str =
			if len < size then
				build (len + 1) (str ^ "-")
			else
				str
		in
		build 0 ""

	let rec lineToString pos n str res =
		if pos < n then
			lineToString (pos + 1) n str (res ^ (String.make 1 (String.get str pos)) ^ " ")
		else
			res

	let big_circle size =
		let rec circle i =
			if i = size * size then ""
			else match (i / size) with
			| 0 -> (match (i mod size) with
				   | 0 -> "/" ^ circle (i + 1)
				   | s when s = size - 1 -> "\\" ^ circle (i + 1)
				   | _ -> "-" ^ circle (i + 1))
			| s when s = size - 1 -> (match (i mod size) with
				   | 0 -> "\\" ^ circle (i + 1)
				   | s when s = size - 1-> "/" ^ circle (i + 1)
				   | _ -> "-" ^ circle (i + 1))
			| _ -> match (i mod size) with
				   | 0 -> "|" ^ circle (i + 1)
				   | s when s = size - 1 -> "|" ^ circle (i + 1)
				   | _ -> " " ^ circle (i + 1)
		in circle 0
  
	let big_cross size =
		let rec cross i =
			if i = size * size then ""
			else if i mod (size + 1) = 0 then
			if size mod 2 = 1 && i = size * size / 2 then "X" ^ cross (i + 1)
			else "\\" ^ cross (i + 1)
			else if i mod (size - 1) = 0 then "/" ^ cross (i + 1)
			else " " ^ cross (i + 1)
		in cross 0
  
	let big size c =
		match c with
			| 'O' -> big_circle size
			| 'X' -> big_cross size
			| _ -> ""

end

type multiMorpion = string list

let buildEmptyMultiMorpion size =
	let rec build len lst =
		if len < size then
			build (len + 1) (lst @ [(SoloMorpion.buildSoloMorpion size)])
		else
			lst
	in
	build 0 []

let hasWin str c index size =
	let max_size = size * size
	in let haswin_direction x y =
		let rec haswin_min start =
			let t = start - x - y * size
				in if t >= 0 && t < max_size && (x = 0 || t mod size <> size - 1) then haswin_min t
				   else start
		in let start = (haswin_min index)
		   in let rec check_win count i first =
				if i < 0 || i >= max_size || (first = false && x <> 0 && i mod size = 0) then count
					else if (String.get str i) = c then check_win (count + 1) (i + x + y * size) false
					else count
				in size = (check_win 0 start true)
		in (haswin_direction 0 1) || (haswin_direction 1 0) || (haswin_direction 1 1) || (haswin_direction 1 (-1))

let isDraw str =
	let rec checkAllChar index =
		if index < (String.length str) then
			if (String.get str index) = '-' then
				false
			else
				checkAllChar (index + 1)
		else
			true
	in
	checkAllChar 0

let buildNewMorpionStr oldStr charToPlace index pName gridNb =
	let rec build i  res =
		if i < (String.length oldStr) && i <> index then
			build (i + 1) (res ^ (String.make 1 (String.get oldStr i)))
		else if i = index then
			build (i + 1) (res ^ String.make 1 charToPlace)
		else if (isDraw res) then
			(SoloMorpion.big  (int_of_float(sqrt (float_of_int (String.length res)))) charToPlace)
		else if (hasWin res charToPlace index (int_of_float(sqrt (float_of_int (String.length res))))) then
			(SoloMorpion.big  (int_of_float(sqrt (float_of_int (String.length res)))) charToPlace)
		else		
			res
	in
	build 0 ""

let printMultiMorpion elem =
	let length = List.length elem
	in
	let lineSize = int_of_float(sqrt (float_of_int (List.length elem)))
	in
	let rec printGrid i res =
		if i < ((length * 2) + lineSize) then
			printGrid (i + 1) (res ^ "-")
		else
			res ^ "\n"
	in
	let rec splitByLine elem n cnt lst res =
		let rec getMultiMorpionLine l x y s  =
			match l with
				| h::q ->
					if x < lineSize then
						begin
							let line = SoloMorpion.lineToString (y * lineSize) ((y + 1) * lineSize) h ""
							in
							if (x + 1) < lineSize then
								getMultiMorpionLine q (x + 1) y (s ^ line ^ "| ")
							else
								getMultiMorpionLine q (x + 1) y (s ^ line ^ "\n")
						end
					else
						s
				| [] ->
					if y < (lineSize - 1) then
						getMultiMorpionLine lst 0 (y + 1) s
					else
						s
		in
		match elem with
			| h::q ->
				if n < lineSize then
					splitByLine q (n + 1) cnt (lst @ [h]) res
				else if cnt < (lineSize ) then						
					splitByLine q 1 (cnt + 1) ([] @ [h]) (res ^ (getMultiMorpionLine lst 0 0 "") ^ (printGrid 0 ""))
				else
					res
			| [] ->	res ^ getMultiMorpionLine lst 0 0 ""
	in
	splitByLine elem 0 0 [] ""
