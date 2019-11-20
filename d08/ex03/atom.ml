class virtual atom name symbol atomic_number =
object
  method name = name
  method symbol = symbol
  method atomic_number = atomic_number
  method to_string = "atom: " ^ name ^ ", symbol: " ^ symbol ^ ", atomic_number: " ^ string_of_int atomic_number
  method equals (a:atom) = atomic_number = a#atomic_number
end

class hydrogen =
object
  inherit atom "hydrogen" "H" 1
end

class carbon =
object
  inherit atom "carbon" "C" 6
end

class oxygen =
object
  inherit atom "oxygen" "O" 8
end

class helium =
object
  inherit atom "helium" "He" 2
end

class lithium =
object
  inherit atom "lithium" "Li" 3
end

class beryllium =
object
  inherit atom "beryllium" "Be" 4
end

class boron =
object
  inherit atom "boron" "B" 5
end

class nitrogen =
object
  inherit atom "nitrogen" "N" 7
end

class fluorine =
object
  inherit atom "fluorine" "F" 9
end

class neon =
object
  inherit atom "neon" "Ne" 10
end

class sodium =
object
  inherit atom "sodium" "Na" 11
end
