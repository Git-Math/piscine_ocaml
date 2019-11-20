module type MONOID =
  sig
    type element
    val zero1 : element
    val zero2 : element
    val mul : element -> element -> element
    val add : element -> element -> element
    val div : element -> element -> element
    val sub : element -> element -> element
  end

module type CALC =
  functor (M : MONOID) ->
  sig
    val add : M.element -> M.element -> M.element
    val sub : M.element -> M.element -> M.element
    val mul : M.element -> M.element -> M.element
    val div : M.element -> M.element -> M.element
    val power : M.element -> int -> M.element
    val fact : M.element -> M.element
  end

module INT:(MONOID with type element = int) =
  struct
    type element = int
    let zero1 = 0
    let zero2 = 1
    let add = ( + )
    let sub = ( - )
    let mul = ( * )
    let div x1 x2 = try x1 / x2 with Division_by_zero -> print_endline "you can't divide by 0"; x1
  end

module FLOAT:(MONOID with type element = float) =
  struct
    type element = float
    let zero1 = 0.
    let zero2 = 1.
    let add = ( +. )
    let sub = ( -. )
    let mul = ( *. )
    let div x1 x2 = try x1 /. x2 with Division_by_zero -> print_endline "you can't divide by 0"; x1
  end

module Calc:CALC =
  functor (M : MONOID) ->
  struct
    let add x1 x2 = M.add x1 x2
    let sub x1 x2 = M.sub x1 x2
    let mul x1 x2 = M.mul x1 x2
    let div x1 x2 = M.div x1 x2
    let power x p =
      let rec pow = function
        | p when p > 1 -> M.mul x (pow (p - 1))
        | _ -> x
      in
      match p with
      | po when po < 0 -> print_endline "power must be positive"; x
      | 0 -> x
      | _ -> pow p
    let fact = function
      | x when x < M.zero1 -> print_endline "fact must be positive"; x
      | x when x = M.zero1 -> M.zero2
      | x -> let rec f = function
               | x when x <= M.zero2 -> (if x = M.zero2 then x else (print_endline "fact must be integer"; M.zero1))
               | x -> M.mul x (f (M.sub x M.zero2))
             in f x
  end
