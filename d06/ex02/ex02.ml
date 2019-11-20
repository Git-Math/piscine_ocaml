module type PAIR = sig val pair : (int * int) end
module type VAL = sig val x : int end

module type MAKEPROJECTION =
  functor (Pair : PAIR) ->
  VAL

module MakeFst : MAKEPROJECTION =
  functor (Pair : PAIR) ->
  struct
    module Pair : PAIR = Pair
    let x = match Pair.pair with
      | (x1,_) -> x1
  end

module MakeSnd : MAKEPROJECTION =
  functor (Pair : PAIR) ->
  struct
    module Pair : PAIR = Pair
    let x = match Pair.pair with
      | (_,x2) -> x2
  end

module Pair : PAIR = struct let pair = ( 21, 42 ) end

module Fst : VAL = MakeFst (Pair)
module Snd : VAL = MakeSnd (Pair)

let () = Printf.printf "Fst.x = %d, Snd.x = %d\n" Fst.x Snd.x
