class virtual alkane n =
object
  inherit Molecule.molecule
            ([|"meth";"eth";"prop";"but";"pent";"hex";"hept";"oct";"non"; "dec"; "undec"; "dodec"|].(n - 1) ^ "ane")
            ( (Array.to_list (Array.make n new Atom.carbon)) @ (Array.to_list (Array.make (2 * n + 2) new Atom.hydrogen))  )
end

class methane =
object
  inherit alkane 1
end

class ethane =
object
  inherit alkane 2
end

class propane =
object
  inherit alkane 3
end

class butane =
object
  inherit alkane 4
end

class pentane =
object
  inherit alkane 5
end

class hexane =
object
  inherit alkane 6
end

class heptane =
object
  inherit alkane 7
end

class octane =
object
  inherit alkane 8
end

class nonane =
object
  inherit alkane 9
end

class decane =
object
  inherit alkane 10
end

class undecane =
object
  inherit alkane 11
end

class dodecane =
object
  inherit alkane 12
end
