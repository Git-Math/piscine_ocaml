class dalek =
  object
    val _name = Random.self_init ();
                let a_ascii = int_of_char 'a'
                and ac_ascii = int_of_char 'A' in
                "Dalek" ^ String.make 1 (char_of_int (ac_ascii + Random.int 26)) ^ String.make 1 (char_of_int (a_ascii + Random.int 26)) ^ String.make 1 (char_of_int (a_ascii + Random.int 26))
    val _hp = 100
    val mutable _shield = true
                     
    method to_string = "dalek " ^ _name ^ ": " ^ string_of_int _hp ^ "hp, shield: " ^ string_of_bool _shield
    method talk = Random.self_init ();
                  print_endline (match Random.int 4 with
                  | 0 -> "Explain! Explain!"
                  | 1 -> "Exterminate! Exterminate!"
                  | 2 -> "I obey!"
                  | _ -> "You are the Doctor! You are the enemy of the Daleks!")
    method exterminate (p:People.people) = _shield <- not _shield;
                                         p#die
    method die = print_endline "Emergency Temporal Shift!"

    initializer print_endline ("creation of dalek " ^ _name)
  end
