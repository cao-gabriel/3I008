let f () =
  let _ =
    print_string "foo\n"
  in 1 + 1

let g =
  let _ =
    print_string "bar\n"
  in
  fun () -> 1 + 1

let gen =
  let cpt = ref 0 in
  function () ->
    cpt := !cpt + 1; !cpt

let (ngen, init) =
  let cpt = ref 0 in
  ((function () -> cpt := !cpt +1; !cpt),
  (function () -> cpt := 0))
