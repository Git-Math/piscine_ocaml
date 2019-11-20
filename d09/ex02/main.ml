module Calc_int = Calc.Calc(Calc.INT)
module Calc_float = Calc.Calc(Calc.FLOAT)
                  
let () =
  print_endline (string_of_int (Calc_int.power 3 3));
  print_endline (string_of_float (Calc_float.power 3.0 3));
  print_endline (string_of_int (Calc_int.mul (Calc_int.add 20 1) 2));
  print_endline (string_of_float (Calc_float.mul (Calc_float.add 20.0 1.0) 2.0));
  print_endline (string_of_float (Calc_float.div (Calc_float.sub 20.0 1.0) 2.0));
  ignore (Calc_int.div 3 0);
  print_endline (string_of_float (Calc_float.div 3.0 0.0));
  print_endline (string_of_int (Calc_int.fact 3));
  print_endline (string_of_float (Calc_float.fact 7.0));
  ignore (Calc_float.fact (-1.));
  print_endline (string_of_float (Calc_float.fact 0.));
  ignore (Calc_float.fact 2.1);
  ignore (Calc_float.fact 0.1)
