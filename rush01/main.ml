let delay_decr_health = 1000
let delay_check = 50


let progress_timeout pbar  vbox () =
	let new_val =
		let v = pbar#fraction -. 0.01 in
		if v < 0. then
			begin
					 0.
			end
			else v in
		pbar#set_fraction new_val;
	  (* As this is a timeout function, return true so that it
	   * continues to get called *)
	  true

let progress_eat pbar pbarEnergy pbarHygiene pbarHappiness image () =
	image#set_file "groot_eat.jpeg" ;
	let new_val =
		let v = pbar#fraction +. 0.25 in
		if v > 1. then 1. else v
		in
	 	pbar#set_fraction new_val ;

	let new_valEnergy =
		let v2 = (float_of_string (string_of_float pbarEnergy#fraction)) -. 0.1 in
		if (v2 < 0. || v2 > 1. ) then 0.
			else v2
	in
	pbarEnergy#set_fraction new_valEnergy ;

	let new_valHigyene =
		let v2 = (float_of_string (string_of_float pbarHygiene#fraction)) -. 0.2 in
		if (v2 < 0. || v2 > 1. ) then 0.
			else v2
	in
	pbarHygiene#set_fraction new_valHigyene ;
	let new_valHappiness =
		let v2 = (float_of_string (string_of_float pbarHappiness#fraction)) +. 0.05 in
		if v2 > 1.  then 1.
			else v2
	in
	pbarHappiness#set_fraction new_valHappiness


let progress_energy pbar pbarEnergy pbarHygiene pbarHappiness image () =
	image#set_file "groot_thunder.png" ;
	let new_val =
		let v2 = (float_of_string (string_of_float pbar#fraction)) -. 0.2 in
		if (v2 < 0. || v2 > 1. ) then 0.
		else v2
		in
	 	pbar#set_fraction new_val ;

	let new_valEnergy =
		let v = pbarEnergy#fraction +. 0.25 in
		if v > 1. then 1. else v

	in
	pbarEnergy#set_fraction new_valEnergy ;

	let new_valHappiness =
		let v2 = (float_of_string (string_of_float pbarHappiness#fraction)) -. 0.2 in
		if (v2 < 0. || v2 > 1. ) then 0.
		else v2
	in
	pbarHappiness#set_fraction new_valHappiness


	let progress_bath pbar pbarEnergy pbarHygiene pbarHappiness image () =
		image#set_file "groot_bath.png" ;
		let new_val =
			let v2 = (float_of_string (string_of_float pbar#fraction)) -. 0.2 in
			if (v2 < 0. || v2 > 1. ) then 0.
			else v2
			in
		 	pbar#set_fraction new_val ;

		let new_valEnergy =
			let v2 = (float_of_string (string_of_float pbarEnergy#fraction)) -. 0.1 in
			if (v2 < 0. || v2 > 1. ) then 0.
				else v2
		in
		pbarEnergy#set_fraction new_valEnergy ;

		let new_valHigyene =
			let v = pbarHygiene#fraction +. 0.25 in
			if v > 1. then 1. else v
		in
		pbarHygiene#set_fraction new_valHigyene ;
		let new_valHappiness =
			let v2 = (float_of_string (string_of_float pbarHappiness#fraction)) +. 0.05 in
			if v2 > 1.  then 1.
				else v2
		in
		pbarHappiness#set_fraction new_valHappiness

	let progress_kill pbar pbarEnergy pbarHygiene pbarHappiness image () =
			image#set_file "groot_kill.jpeg" ;
			let new_val =
				let v2 = (float_of_string (string_of_float pbar#fraction)) -. 0.2 in
				if (v2 < 0. || v2 > 1. ) then 0.
				else v2
				in
			 	pbar#set_fraction new_val ;

			let new_valEnergy =
			let v2 = (float_of_string (string_of_float pbarEnergy#fraction)) -. 0.1 in
			if (v2 < 0. || v2 > 1. ) then 0.
				else v2

			in
			pbarEnergy#set_fraction new_valEnergy ;
			let new_valHappiness =
				let v2 = (float_of_string (string_of_float pbarHappiness#fraction)) +. 0.2 in
				if v2 > 1.  then 1.
					else v2
			in
			pbarHappiness#set_fraction new_valHappiness

			let progress_check list_target vbox () =
				let rec loop_check l ret = match l with
				| h::t -> loop_check t (if h#fraction = 0. then 0 else ret)
				| [] -> ret
				in
				if (loop_check list_target 1) = 0 then
					begin
						if (List.length vbox#all_children) = 2 then
							begin
							vbox#remove (List.hd vbox#all_children) ;
							let image  =  GMisc.image ~width:500 ~height:500  ~packing:vbox#add () in
							image#set_file "groot-dies.jpeg" ;
							ignore(GMisc.label ~text:"Game over" ~packing:vbox#add ())
							end
					end ;
				true

	let progress_restart pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2 list_to_check () =
			let rec loop l = match l with
			| h::t -> vboxInit#remove h ; loop t
			| [] -> ()
			in
			loop vboxInit#all_children ;
			vboxInit#add vbox#coerce  ;
			vboxInit#add vbox2#coerce  ;
			image#set_file "groot_baby.jpg" ;

			pbar#set_fraction 1. ;
			pbarEnergy#set_fraction 1. ;
			pbarHygiene#set_fraction 1. ;
			pbarHappiness#set_fraction 1. ;
			ignore(GMain.Timeout.add delay_check (progress_check list_to_check  vboxInit))

	let progress_save pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2 list_to_check () =
			Save.Save.save [pbar#fraction;pbarEnergy#fraction;pbarHygiene#fraction;pbarHappiness#fraction ]

	let progress_load pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2 list_to_check () =
			progress_restart pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2 list_to_check () ;
			let list_data = Save.Save.get_save () in
			pbar#set_fraction (List.nth list_data 0) ;
			pbarEnergy#set_fraction (List.nth list_data 1) ;
			pbarHygiene#set_fraction (List.nth list_data 2) ;
			pbarHappiness#set_fraction (List.nth list_data 3)








let destroy_progress () =

  GMain.Main.quit ()


let main () =
	ignore(GtkMain.Main.init ()) ;
	let window = GWindow.window ~border_width:10
															~width:1000
															~height:1000
															~title:"Wibbily wobbly timey wimey creature"	() in

		let vboxInit = GPack.vbox ~border_width:10 ~packing:window#add () in
		let vbox = GPack.vbox ~border_width:10 ~packing:vboxInit#add () in
		let vbox2 = GPack.vbox ~border_width:10 ~packing:vboxInit#add () in


	  (* Create a centering alignment object *)

		let boxStats = GPack.hbox  ~homogeneous:true ~spacing:10 ~packing:vbox#add () in
	  let align = GBin.alignment ~xalign:0.5 ~yalign:0.5
	    ~xscale:0.0 ~yscale:0.0 ~packing:boxStats#add ()
	  in
		let alignEnergy = GBin.alignment ~xalign:0.5 ~yalign:0.5
		~xscale:0.0 ~yscale:0.0 ~packing:boxStats#add ()
		in
		let alignHygiene = GBin.alignment ~xalign:0.5 ~yalign:0.5
		~xscale:0.0 ~yscale:0.0 ~packing:boxStats#add ()
		in
		let alignHappiness = GBin.alignment ~xalign:0.5 ~yalign:0.5
		~xscale:0.0 ~yscale:0.0 ~packing:boxStats#add ()
		in



	  (* Create the progressbar *)
		let pbar = GRange.progress_bar  ~pulse_step:0.01 ~packing:align#add () in
		pbar#set_text "Health";
		pbar#set_fraction 1.;

		let pbarEnergy = GRange.progress_bar  ~pulse_step:0.1 ~packing:alignEnergy#add () in
		pbarEnergy#set_text "Energy";
		pbarEnergy#set_fraction 1.;

		let pbarHygiene = GRange.progress_bar  ~pulse_step:0.1 ~packing:alignHygiene#add () in
		pbarHygiene#set_text "Hygiene";
		pbarHygiene#set_fraction 1.;

		let pbarHappiness = GRange.progress_bar  ~pulse_step:0.1 ~packing:alignHappiness#add () in
		pbarHappiness#set_text "Happiness";
		pbarHappiness#set_fraction 1.;

		let image  =  GMisc.image ~width:500 ~height:500  ~packing:vbox#add () in
		image#set_file "groot.jpeg" ;

		let boxActionButton = GPack.hbox  ~homogeneous:true ~spacing:10 ~packing:vbox#add () in
		let buttonEat = GButton.button ~label:"EAT" ~packing:boxActionButton#add () in
		ignore(buttonEat#connect#clicked ~callback:(progress_eat pbar pbarEnergy pbarHygiene pbarHappiness image));
		let buttonEnergy = GButton.button ~label:"THUNDER" ~packing:boxActionButton#add () in
		ignore(buttonEnergy#connect#clicked ~callback:(progress_energy pbar pbarEnergy pbarHygiene pbarHappiness image));

		let buttonHygiene = GButton.button ~label:"BATH" ~packing:boxActionButton#add () in
		ignore(buttonHygiene#connect#clicked ~callback:(progress_bath pbar pbarEnergy pbarHygiene pbarHappiness image));
		let buttonHappiness = GButton.button ~label:"KILL" ~packing:boxActionButton#add () in
		ignore(buttonHappiness#connect#clicked ~callback:(progress_kill pbar pbarEnergy pbarHygiene pbarHappiness image));

		let boxControlButton = GPack.hbox  ~homogeneous:true ~spacing:10 ~packing:vbox2#add () in
		let save_btn = GButton.button ~label:"Save" ~packing:boxControlButton#add () in
		ignore(save_btn#connect#clicked ~callback:(progress_save pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2  [pbar;pbarEnergy;pbarHygiene;pbarHappiness]));
		let load_btn = GButton.button ~label:"Load" ~packing:boxControlButton#add () in
		ignore(load_btn#connect#clicked ~callback:(progress_load pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2  [pbar;pbarEnergy;pbarHygiene;pbarHappiness]));
		let retstart = GButton.button ~label:"Restart" ~packing:boxControlButton#add () in
		ignore(retstart#connect#clicked ~callback:(progress_restart pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2  [pbar;pbarEnergy;pbarHygiene;pbarHappiness]));
		let quit_btn = GButton.button ~label:"Quit" ~packing:boxControlButton#add () in
		ignore(quit_btn#connect#clicked ~callback:destroy_progress);


		progress_load pbar pbarEnergy pbarHygiene pbarHappiness image vboxInit vbox vbox2  [pbar;pbarEnergy;pbarHygiene;pbarHappiness] ();
		(* vbox#remove (List.hd vbox#all_children) ; *)

	(* ignore(button#connect#clicked ~callback:window#destroy); *)
	ignore(GMain.Timeout.add delay_decr_health (progress_timeout pbar vbox));
	ignore(GMain.Timeout.add delay_check (progress_check [pbar;pbarEnergy;pbarHygiene;pbarHappiness] vboxInit));


	ignore(window#connect#destroy ~callback:destroy_progress );
	window#show ();
	GMain.Main.main ()

let () =
	main ()
