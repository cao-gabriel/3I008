open Tableur

let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1) ;
  ()

let test3 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  assert(cycle grille grille.(1).(0) = true)

let test4 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  assert(eval_expr grille.(0).(1) grille = REntier(1))

let test5 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(0) = Erreur(Cycle_detecte))

let test6 () =
  let g  = cree_grille 4 4 in
  g.(0).(0) <- Entier 1;
  g.(0).(1) <- Case(0,0);
  g.(1).(0) <- Case(1,1);
  g.(1).(1) <- abs (Case(1,0));
  let r_grille = eval_grille g in
  assert(r_grille.(1).(1) = Erreur(Cycle_detecte))


let test7 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(0) = Erreur(Cycle_detecte))

let test8 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(0) = Erreur(Cycle_detecte))

let test9 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(0) = Erreur(Cycle_detecte))

let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2); ("cycle grille    ", test3)
    ;("eval_expr grille", test4); ("eval_grille    ", test5);
     ("cycle grille hard", test6)(* ... *)]
  in
  List.iteri
    (fun i (nom_test, f_test) ->
      Format.printf "Test #%d - %s:\t" (i + 1) nom_test ;
      try
        f_test () ;
        Format.printf "\027[32mOk\n\027[39m"
      with exn ->
        Format.printf "\027[31mErreur - %s\n\027[39m" (Printexc.to_string exn))
    liste_tests

(* Main *)
let () = run_tests ()
