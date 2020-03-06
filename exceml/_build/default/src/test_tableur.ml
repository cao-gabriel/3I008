open Tableur

(*Test de la création de la grille*)
let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

(*Test pour l'affectation de la grille*)
let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1) ;
  ()

(*Test pour la conversion de'une expression en chaine*)
let test3 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  grille.(0).(1) <- Chaine "salut";
  grille.(1).(0) <- Vide;
  grille.(1).(1) <- Flottant 3.0;
  grille.(2).(0) <- Case(1,0);
  assert (expr_to_string grille.(0).(0) = "1");
  assert (expr_to_string grille.(0).(1) = "salut");
  assert (expr_to_string grille.(1).(0) = "");
  assert (expr_to_string grille.(1).(1) = "3.");
  assert (expr_to_string grille.(2).(0) =  "@(1,0)")


(*Test pour l'affichage de la grille*)
let test4 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1 ;
  grille.(0).(1) <- Chaine "salut";
  grille.(1).(0) <- Vide;
  grille.(1).(1) <- Flottant 3.0;
  grille.(2).(0) <- Case(1,0);
  Format.printf "\n";
  affiche_grille grille

(*Test présence de cycle*)
let test5 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- Case(1,1);
  grille.(1).(1) <- Case(1,0);
  assert(cycle grille grille.(0).(0) = false);
  assert(cycle grille grille.(0).(1) = false);
  assert(cycle grille grille.(1).(0) = true);
  assert(cycle grille grille.(1).(1) = true)

(*Test de l'evalutation d'une expression*)
let test6 () =
  let g  = cree_grille 4 4 in
  g.(0).(0) <- Entier 1;
  g.(0).(1) <- Case(0,0);
  g.(1).(0) <- Case(2,0);
  g.(1).(1) <- Case(1,2);
  g.(1).(2) <- Case (5,6);
  g.(2).(0) <- Case(1,0);
  assert(eval_expr g.(0).(0) g = REntier 1);
  assert(eval_expr g.(0).(1) g = REntier 1);
  assert(eval_expr g.(1).(0) g = Erreur Cycle_detecte);
  assert(eval_expr g.(2).(0) g = Erreur Cycle_detecte);
  assert(eval_expr g.(1).(1) g = Erreur (CaseRefError(1,2)));
  assert(eval_expr g.(1).(2) g = Erreur (Mauvais_indice (5,6)))


(*Test de l'evaluation d'une grille*)
let test7 () =
  let g  = cree_grille 4 4 in
  g.(0).(0) <- Entier 1;
  g.(0).(1) <- Case(0,0);
  g.(1).(0) <- Case(2,0);
  g.(1).(1) <- Case(1,2);
  g.(1).(2) <- Case (5,6);
  g.(2).(0) <- Case(1,0);
  let g = eval_grille g in
  assert( g.(0).(0) = REntier 1);
  assert( g.(0).(1) = REntier 1);
  assert( g.(1).(0)= Erreur Cycle_detecte);
  assert( g.(2).(0)= Erreur Cycle_detecte);
  assert( g.(1).(1) = Erreur (CaseRefError(1,2)));
  assert( g.(1).(2)= Erreur (Mauvais_indice (5,6)))


(*Affichage de la grille resultat*)
let test8 () =
  let g  = cree_grille 4 4 in
  g.(0).(0) <- Entier 1;
  g.(0).(1) <- Case(0,0);
  g.(1).(0) <- Case(2,0);
  g.(1).(1) <- Case(1,2);
  g.(1).(2) <- Case (5,6);
  g.(2).(0) <- Case(1,0);
  let g = eval_grille g in
  print_newline ();
  affiche_grille_resultat g

(*Test de la presence de cycle avec opérations*)
let test9 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier 1;
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- (Case(1,1));
  grille.(1).(1) <- abs (Case(1,0));
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(0) = Erreur(Cycle_detecte))

(*Test de l'evaluation des expresion avec operation*)
let test10 () =
  let grille  = cree_grille 4 4 in
  grille.(0).(0) <- Entier (-1);
  grille.(0).(1) <- Case(0,0);
  grille.(1).(0) <- (Case(1,1));
  grille.(1).(1) <- abs (Case(0,0));
  grille.(2).(0) <- add grille.(0).(0) grille.(1).(1);
  grille.(3).(0) <- somme (0,0) (1,1);
  let r_grille = eval_grille grille in
  assert(r_grille.(1).(1) = REntier 1);
  assert(r_grille.(2).(0) = REntier 0);
  assert(r_grille.(3).(0) = REntier 0)


let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2);
     ("chaine expression", test3); ("affichage grille", test4);
     ("cycle grille", test5); ("evaluation expr", test6);
     ("evaluation grille", test7); ("affiche grille result", test8);
     ("cycle grille hard", test9); ("eval expr hard", test10)]
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
