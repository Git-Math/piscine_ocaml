let my_sleep () = Unix.sleep 1

let () =
  let av = Sys.argv
  in let n = try int_of_string av.(1) with
             | Failure err | Invalid_argument err -> 0
     in for i = n downto 1 do
          my_sleep ()
        done
