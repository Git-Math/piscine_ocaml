class virtual molecule name (formula:Atom.atom list) =
object (this)
  method name = name
  method formula = let l = List.sort (fun x y -> match (x#symbol, y#symbol) with
                                         | (a, b) when a = b -> 0
                                         | (_, "C") -> 1
                                         | ("C", _) -> -1
                                         | (_, "H") -> 1
                                         | ("H", _) -> -1
                                         | (a, b) -> compare a b) formula
                   in let rec string_formula l str count =
                        match l with
                        | [] -> str ^ if count > 1 then string_of_int count else ""
                        | head::tail -> if head#symbol = str then string_formula tail str (count + 1)
                                        else if count > 0 then str
                                                               ^ begin if count > 1 then string_of_int count else "" end
                                                               ^ string_formula tail head#symbol 1
                                        else string_formula tail head#symbol 1
                      in string_formula l "" 0
  method to_string = "molecule: " ^ name ^ ", formula: " ^ this#formula                 
end

class water =
object
  inherit molecule "water" [new Atom.hydrogen; new Atom.oxygen; new Atom.hydrogen]
end

class dioxyde =
object
  inherit molecule "dioxyde" [new Atom.oxygen; new Atom.oxygen; new Atom.carbon]
end

class hydroperoxyl =
object
  inherit molecule "hydroperoxyl" [new Atom.oxygen; new Atom.oxygen; new Atom.hydrogen]
end

class molecular_oxygen =
object
  inherit molecule "molecular oxygen" [new Atom.oxygen; new Atom.oxygen]
end

class diatomic_carbon =
object
  inherit molecule "diatomic carbon" [new Atom.carbon; new Atom.carbon]
end

class acetylene =
object
  inherit molecule "acetylene" [new Atom.carbon; new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen]
end

class methane =
object
  inherit molecule "methane" [new Atom.carbon; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen; new Atom.hydrogen]
end
