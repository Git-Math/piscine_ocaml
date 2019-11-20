module type WATCHTOWER =
  sig
    type hour = int
    val zero : hour
    val add : hour -> hour -> hour
    val sub : hour -> hour -> hour
  end

module Watchtower:WATCHTOWER =
  struct
    type hour = int
    let zero = 0
    let add h1 h2 = match (h1 + h2) mod 12 with
      | h when h < 0 -> h + 12
      | h -> h
    let sub h1 h2 = add h1 (-h2)
  end
