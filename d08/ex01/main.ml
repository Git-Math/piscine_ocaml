let () =
  let la = [new Molecule.water; new Molecule.dioxyde; new Molecule.hydroperoxyl; new Molecule.molecular_oxygen; new Molecule.diatomic_carbon; new Molecule.acetylene; new Molecule.methane]
  in let rec print_molecule = function
       | [] -> ()
       | head::tail -> print_endline head#to_string; print_molecule tail
     in print_molecule la
