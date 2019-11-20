module SoloMorpion : sig

	type sMorpion = string

	val buildSoloMorpion : int -> sMorpion
	val lineToString : int -> int -> sMorpion -> string -> string
	val big_circle : int -> string
	val big_cross : int -> string
	val big : int -> char -> string

end

val buildNewMorpionStr : string -> char -> int -> string -> int -> string
val buildEmptyMultiMorpion : int -> string list
val printMultiMorpion : string list -> string
val hasWin : string -> char -> int -> int -> bool
val isDraw : string -> bool

