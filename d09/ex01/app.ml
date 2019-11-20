module type APP =
  sig
    type project = string * string * int
    val zero : project
    val combine : project -> project -> project
    val fail : project -> project
    val success : project -> project
  end

module App:APP =
  struct
    type project = string * string * int
    let zero = ("", "", 0)
    let combine p1 p2 = match (p1, p2) with ((p1_1, _, p1_3), (p2_1, _, p2_3)) ->
        let moyenne = (p1_3 + p2_3) / 2 in
        (p1_1 ^ p2_1,
         (if moyenne >= 80 then "succeed" else "failed"),
         moyenne)
    let fail p = match p with (p1, _, _) -> (p1, "failed", 0)
    let success p = match p with (p1, _, _) -> (p1, "succeed", 80)
  end
