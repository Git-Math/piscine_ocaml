let () =
  let a = Array.make 5 ""
  in Array.set a 0 "C'est l'histoire d'un têtard qui pensait qu'il était en retard mais en fait il était dans les temps.";
     Array.set a 1 "Pourquoi un chasseur emmène-t-il son fusil aux toilettes ?\nPour tirer la chasse.";
     Array.set a 2 "Pourquoi est-ce qu'on met tous les crocos en prison ?\nParce que les crocos dealent.";
     Array.set a 3 "Que se passe-t-il quand 2 poissons s'énervent ?\nLe thon monte";
     Array.set a 4 "Qu'est ce qu'une carotte dans une flaque d'eau ?\nUn bonhomme de neige en été";
     Random.self_init ();
     print_endline (a.(Random.int 5))
