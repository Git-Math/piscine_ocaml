let () =
  let la = [new Atom.hydrogen; new Atom.carbon; new Atom.oxygen; new Atom.helium; new Atom.lithium; new Atom.beryllium; new Atom.boron; new Atom.nitrogen; new Atom.fluorine; new Atom.neon; new Atom.sodium]
  in let rec print_atom = function
       | [] -> ()
       | head::tail -> print_endline head#to_string; print_atom tail
     in print_atom la;
        let a1 = new Atom.hydrogen
        and a2 = new Atom.carbon
        in
        print_endline (string_of_bool (a1#equals a2));
        print_endline (string_of_bool (a1#equals a1))
