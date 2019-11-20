type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_square x y size =
  let l = x - size / 2
  and r = x + size / 2
  and u = y - size / 2
  and d = y + size / 2
  in
  Graphics.moveto l u;
  Graphics.lineto r u;
  Graphics.lineto r d;
  Graphics.lineto l d;
  Graphics.lineto l u 

let draw_tree_node node =
  match node with
  | Nil-> draw_square 200 225 100
  | Node (x,_,_) -> draw_square 200 225 100;
                    draw_square 400 150 100;
                    Graphics.moveto 350 300;
                    Graphics.lineto 250 225;
                    Graphics.lineto 350 150;
                    Graphics.moveto 185 225;
                    Graphics.draw_string (string_of_int x);
	                draw_square 400 300 100;
                    Graphics.moveto 400 300;
                    Graphics.draw_string "Nil";
                    Graphics.moveto 400 150;
                    Graphics.draw_string "Nil"

let main () =
  Graphics.open_graph "";
  draw_tree_node (Node (42, Nil, Nil));
  let rec dirty () =
	dirty ()
  in dirty ()

let _ = main ()
