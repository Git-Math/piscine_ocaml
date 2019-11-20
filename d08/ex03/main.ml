let () =
  let la = [new Alkane.methane; new Alkane.ethane; new Alkane.propane; new Alkane.butane; new Alkane.pentane; new Alkane.hexane; new Alkane.heptane; new Alkane.octane; new Alkane.nonane; new Alkane.decane; new Alkane.undecane; new Alkane.dodecane]
  in let rec print_alkane = function
       | [] -> ()
       | head::tail -> print_endline head#to_string; print_alkane tail
     in print_alkane la
