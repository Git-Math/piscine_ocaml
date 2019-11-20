module type TRY =
  sig
    type 'a t = Success of 'a | Failure of exn
    val return : 'a -> 'a t
    val bind : 'a t -> ('a -> 'b t) -> 'b t
    val recover : 'a t -> (exn -> 'a t) -> 'a t
    val filter : 'a t -> ('a -> bool) -> 'a t
    val flatten : 'a t t -> 'a t
  end

module Try:TRY =
  struct
    type 'a t = Success of 'a | Failure of exn
    let return x = Success x
    let bind x f = match x with
      | Success s -> (try f s with e -> Failure e)
      | Failure e -> Failure e
    let recover x f = match x with
      | Failure e -> f e
      | _ -> x
    let filter x f = match x with
      | Success s when (f s) = false -> Failure (failwith "Try.filter exn")
      | _ -> x
    let flatten = function
      | Success a -> begin
          match a with
          | Success _ -> a
          | _ -> Failure (failwith "Try.flatten exn")
        end
      | _ -> Failure (failwith "Try.flatten exn")
  end

let () =
  let print_try = function
    | Try.Success a -> print_endline ("Success " ^ a)
    | Try.Failure _ -> print_endline "Failure"
  in
  let f x =
    if x = "fail" then (failwith "fail")
    else Try.Success (x ^ " success")
  in
  print_try (Try.return "42");
  let fa = Try.bind (Try.return "fail") f in
  print_try fa;
  print_try (Try.bind (Try.return "42") f);
  print_try (Try.recover (Try.return "42") (fun x -> Success "failure"));
  print_try (Try.recover fa (fun x -> Success "failure"));
  print_try (Try.filter (Try.return "42") (fun x -> x = "42"));
  try  print_try (Try.flatten (Try.Success fa)) with Failure e -> print_endline e
