class doctor ?(hp = 100) name sidecick age =
  object (self)
    initializer print_endline ("creation of doctor " ^ name)

    val _name = name
    val _age = age
    val _sidecick:People.people = sidecick
    val _hp = hp

    method to_string = "doctor " ^ _name ^ ": " ^ string_of_int _age ^ " years old, sidecick: " ^ _sidecick#to_string ^ ", " ^ string_of_int _hp ^ "hp"
    method talk = print_endline "Hi! I’m the Doctor!"
    method travel_in_time (start:int) (arrival:int) = print_endline "        ___\n_______(_@_)_______\n| POLICE      BOX |\n|_________________|\n | _____ | _____ |\n | |###| | |###| |\n | |###| | |###| |\n | _____ | _____ |\n | || || | || || |\n | ||_|| | ||_|| |\n | _____ |$_____ |\n | || || | || || |\n | ||_|| | ||_|| |\n | _____ | _____ |\n | || || | || || |\n | ||_|| | ||_|| |\n |       |       |\n *****************"; new doctor _name _sidecick (_age + arrival - start)
    method use_sonic_screwdriver = print_endline "Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii Whiiiiwhiiiwhiii"
    method private regenerate = new doctor _name _sidecick _age

    (* those methods are for test purpose *)
    method lose_hp l = if _hp - l <= 0 then self#regenerate else new doctor ~hp:(_hp - l) _name _sidecick _age
  end
