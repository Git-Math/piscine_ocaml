let () =
  let p = new People.people "People" in
  let rec print_army = function
    | [] -> print_endline ""
    | head::tail -> print_endline (head#to_string); print_army tail                    
  and add_people i l =
    match i with
    | i when i >= 5 -> l
    | _ -> let l_next = l#add (new People.people ("People " ^ string_of_int i)) in
           begin
             print_army l_next#get_l;
             add_people (i + 1) l_next
           end
  and add_doctor i l =
    match i with
    | i when i >= 5 -> l
    | _ -> let l_next = l#add (new Doctor.doctor ("Doctor Who " ^ string_of_int i) p 73) in
           begin
             print_army l_next#get_l;
             add_doctor (i + 1) l_next
           end
  and add_dalek i l =
    match i with
    | i when i >= 5 -> l
    | _ -> let l_next = l#add (new Dalek.dalek) in
           begin
             print_army l_next#get_l;
             add_dalek (i + 1) l_next
           end
  and del i l =
    match i with
    | i when i >= 5 -> ()
    | _ -> let l_next = l#delete in
           begin
             print_army l_next#get_l;
             del (i + 1) l_next
           end
  in
  let army_people = new Army.army [new People.people "People 0"]
  and army_doctor = new Army.army [new Doctor.doctor "Doctor Who 0" p 73]
  and army_dalek = new Army.army [new Dalek.dalek] in
  let army_people_full = add_people 1 army_people
  and army_doctor_full = add_doctor 1 army_doctor
  and army_dalek_full = add_dalek 1 army_dalek in
  del 0 army_people_full;
  del 0 army_doctor_full;
  del 0 army_dalek_full
