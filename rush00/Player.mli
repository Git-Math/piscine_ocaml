type coord = {l: int ; c: int}

val coordToPos : int list -> int -> coord
val gameBoardSize : unit -> int

val placeChar : char -> string list -> coord -> string -> string list

val validNamePlayer1 : string -> string
val validNamePlayer2 : string -> string -> string

val coordStringValidity : string list -> int list -> string list -> coord
val play : string -> string list -> bool