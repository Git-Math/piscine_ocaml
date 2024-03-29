module Color =
  struct
    type t = Spade | Heart | Diamond | Club

    let all = [Spade; Heart; Diamond; Club]

    let toString = function
      | Spade -> "S"
      | Heart -> "H"
      | Diamond -> "D"
      | Club -> "C"

    let toStringVerbose = function
      | Spade -> "Spade"
      | Heart -> "Heart"
      | Diamond -> "Diamond"
      | Club -> "Club"
  end

module Value =
  struct
    type t = T2 | T3 | T4 | T5 | T6 | T7 | T8 | T9 | T10 | Jack | Queen | King | As

    let all = [T2; T3; T4; T5; T6; T7; T8; T9; T10; Jack; Queen; King; As]

    let toInt = function
      | T2 -> 1
      | T3 -> 2
      | T4 -> 3
      | T5 -> 4
      | T6 -> 5
      | T7 -> 6
      | T8 -> 7
      | T9 -> 8
      | T10 -> 9
      | Jack -> 10
      | Queen -> 11
      | King -> 12
      | As -> 13

    let toString = function
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "J"
      | Queen -> "Q"
      | King -> "K"
      | As -> "A"

    let toStringVerbose = function
      | T2 -> "2"
      | T3 -> "3"
      | T4 -> "4"
      | T5 -> "5"
      | T6 -> "6"
      | T7 -> "7"
      | T8 -> "8"
      | T9 -> "9"
      | T10 -> "10"
      | Jack -> "Jack"
      | Queen -> "Queen"
      | King -> "King"
      | As -> "As"

    let next = function
      | T2 -> T3
      | T3 -> T4
      | T4 -> T5
      | T5 -> T6
      | T6 -> T7
      | T7 -> T8
      | T8 -> T9
      | T9 -> T10
      | T10 -> Jack
      | Jack -> Queen
      | Queen -> King
      | King -> As
      | As -> invalid_arg "Last card"

    let previous = function
      | T2 -> invalid_arg "First card"
      | T3 -> T2
      | T4 -> T3
      | T5 -> T4
      | T6 -> T5
      | T7 -> T6
      | T8 -> T7
      | T9 -> T8
      | T10 -> T9
      | Jack -> T10
      | Queen -> Jack
      | King -> Queen
      | As -> King
  end

type t = (Value.t * Color.t)

let newCard v c =
  (v, c)

let allSpades = List.map (fun x -> newCard x Color.Spade) Value.all
let allHearts = List.map (fun x -> newCard x Color.Heart) Value.all
let allDiamonds = List.map (fun x -> newCard x Color.Diamond) Value.all
let allClubs = List.map (fun x -> newCard x Color.Club) Value.all
let all = allSpades @ allHearts @ allDiamonds @ allClubs

let getValue = function
  | (v, _) -> v

let getColor = function
  | (_, c) -> c

let toString = function
  | (v, c) -> (Value.toString v) ^ (Color.toString c)

let toStringVerbose = function
  | (v, c) -> "Card(" ^ (Value.toStringVerbose v) ^ ", " ^ (Color.toStringVerbose c) ^ ")"

let compare c1 c2 =
  (Value.toInt (getValue c1)) - (Value.toInt (getValue c2))

let max c1 c2 =
  if (Value.toInt (getValue c1)) >= (Value.toInt (getValue c2)) then c1 else c2

let min c1 c2 =
  if (Value.toInt (getValue c1)) <= (Value.toInt (getValue c2)) then c1 else c2

let best = function
  | [] -> invalid_arg "empty list"
  | head::tail -> List.fold_left max head tail

let isOf c co =
  (getColor c) = co

let isSpade c =
  isOf c Color.Spade

let isHeart c =
  isOf c Color.Heart

let isDiamond c =
  isOf c Color.Diamond

let isClub c =
  isOf c Color.Club
