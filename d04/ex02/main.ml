let () =
  let c = Card.newCard Card.Value.T7 Card.Color.Spade
  in let rec print = function
    | [] -> ()
    | head::tail -> print_endline ("getValue: " ^ Card.Value.toString (Card.getValue head)
                                   ^ "; getColor: " ^ Card.Color.toString (Card.getColor head)
                                   ^ "; toString: " ^ Card.toString head
                                   ^ "; toStringVerbose: " ^ Card.toStringVerbose head
                                   ^ "; compare with " ^ Card.toString c ^ ": " ^ string_of_int (Card.compare head c)
                                   ^ "; max with " ^ Card.toString c ^ ": " ^ Card.toString (Card.max head c)
                                   ^ "; min with " ^ Card.toString c ^ ": " ^ Card.toString (Card.min head c)
                                   ^ "; isSpade: " ^ (if Card.isSpade head then "true" else "false")
                                   ^ "; isHeart: " ^ (if Card.isHeart head then "true" else "false")
                                   ^ "; isDiamond: " ^ (if Card.isDiamond head then "true" else "false")
                                   ^ "; isClub: " ^ (if Card.isClub head then "true" else "false"));
                    print tail
     in print Card.all;
        print_endline (try Card.toString (Card.best []) with invalid_arg -> "best: invalid arg");
        print_endline ("best: " ^ Card.toString (Card.best Card.all))
