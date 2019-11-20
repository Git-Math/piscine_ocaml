module type FIXED =
  sig
    type t
    val of_float : float -> t
    val of_int : int -> t
    val to_float : t -> float
    val to_int : t -> int
    val to_string : t -> string
    val zero : t
    val one : t
    val succ : t -> t
    val pred : t -> t
    val min : t -> t -> t
    val max : t -> t -> t
    val gth : t -> t -> bool
    val lth : t -> t -> bool
    val gte : t -> t -> bool
    val lte : t -> t -> bool
    val eqp : t -> t -> bool
    val eqs : t -> t -> bool
    val add : t -> t -> t
    val sub : t -> t -> t
    val mul : t -> t -> t
    val div : t -> t -> t
    val foreach : t -> t -> (t -> unit) -> unit
  end

module type FRACTIONNAL_BITS =
  sig
    val bits : int
  end

module type MAKE =
  functor (F:FRACTIONNAL_BITS) -> FIXED

module Make : MAKE =
  functor (F:FRACTIONNAL_BITS) ->
  struct
    type t = int
    let of_float f = int_of_float (floor (f *. 2. ** float_of_int F.bits +. 0.5))
    let of_int i = i lsl F.bits
    let to_float t = float_of_int t /. (2. ** float_of_int F.bits)
    let to_int t = t lsr F.bits
    let to_string t = string_of_float (to_float t)
    let zero = 0
    let one = 1 lsl F.bits
    let succ t = t + 1
    let pred t = t - 1
    let min t1 t2 = if t1 <= t2 then t1 else t2
    let max t1 t2 = if t1 >= t2 then t1 else t2
    let gth t1 t2 = t1 > t2
    let lth t1 t2 = t1 < t2
    let gte t1 t2 = t1 >= t2
    let lte t1 t2 = t1 <= t2
    let eqp t1 t2 = t1 == t2
    let eqs t1 t2 = t1 = t2
    let add t1 t2 = t1 + t2
    let sub t1 t2 = t1 - t2
    let mul t1 t2 = of_float (to_float t1 *. to_float t2)
    let div t1 t2 = of_float (to_float t1 /. to_float t2)
    let foreach t1 t2 f =
      let rec loop t =
        f t;
        if t < t2 then loop (t + 1)
      in loop t1
  end
  
module Fixed4 : FIXED = Make (struct let bits = 4 end)
module Fixed8 : FIXED = Make (struct let bits = 8 end)
                      
let () =
  let x8 = Fixed8.of_float 21.10 in
  let y8 = Fixed8.of_float 21.32 in
  let r8 = Fixed8.add x8 y8 in
  print_endline (Fixed8.to_string r8);
  Fixed4.foreach (Fixed4.zero) (Fixed4.one) (fun f -> print_endline (Fixed4.to_string f));
  print_endline (Fixed8.to_string (Fixed8.of_int 42));
  print_endline (string_of_float (Fixed8.to_float x8));
  print_endline (string_of_int (Fixed8.to_int y8));
  print_endline (Fixed8.to_string (Fixed8.succ x8));
  print_endline (Fixed8.to_string (Fixed8.pred y8));
  print_endline (Fixed8.to_string (Fixed8.min x8 y8));
  print_endline (Fixed8.to_string (Fixed8.max x8 y8));
  print_endline (string_of_bool (Fixed8.gth x8 y8));
  print_endline (string_of_bool (Fixed8.lth x8 y8));
  print_endline (string_of_bool (Fixed8.gte x8 y8));
  print_endline (string_of_bool (Fixed8.lte x8 y8));
  print_endline (string_of_bool (Fixed8.eqp x8 y8));
  print_endline (string_of_bool (Fixed8.eqp x8 x8));
  print_endline (string_of_bool (Fixed8.eqs x8 y8));
  print_endline (string_of_bool (Fixed8.eqs y8 y8));
  print_endline (Fixed8.to_string (Fixed8.sub x8 y8));
  print_endline (Fixed8.to_string (Fixed8.mul x8 y8));
  print_endline (Fixed8.to_string (Fixed8.div x8 y8))
