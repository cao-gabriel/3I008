open Tableur

let g = cree_grille 10 10
let _ =
  g.(0).(0) <- Vide;
  g.(0).(1) <- Chaine "salut";
  g.(0).(2) <- Entier 9 ;
  g.(0).(3) <- Flottant 3.0;
  g.(0).(4) <- Case(1,0);
  g.(0).(5) <- myabs (Entier (-8));
  g.(0).(6) <- add (Entier 1) (Entier 2);
  g.(0).(7) <- somme (0,0) (0,1);
  g.(0).(8) <- Case(5,0);
  g.(0).(9) <- Case(2,0);
  g.(1).(0) <- Case(1,2);
  g.(1).(1) <- Case(5,1323);
  g.(1).(2) <- Case(1,0);
  g.(1).(3) <- myabs (Case (1,0));
  g.(1).(4) <- add (Case(1,0)) (Case(3,0));
  g.(1).(5) <- somme (0,0) (1,1)

(*Test de la création de la grille*)
let test1 () =
  let grille = cree_grille 10 10 in
  ignore grille

(*Test pour l'affectation de la grille*)
let test2 () =
  let grille = cree_grille 10 10 in
  grille.(0).(0) <- Entier 1 ;
  assert (grille.(0).(0) = Entier 1)

(*Test pour la conversion de'une expression en chaine*)
let test3 () =
  assert (expr_to_string g.(0).(0) = "");
  assert (expr_to_string g.(0).(1) = "salut");
  assert (expr_to_string g.(0).(2) = "9");
  assert (expr_to_string g.(0).(3) = "3.");
  assert (expr_to_string g.(0).(4) =  "@(1,0)");
  assert (expr_to_string g.(0).(5) = "op_u(-8)");
  assert (expr_to_string g.(0).(6) = "op_b(1,2)");
  assert (expr_to_string g.(0).(7) = "op_r([0,0]:[0,1])")

(*Test pour l'affichage de la grille*)
let test4 () =
  Format.printf "\nAffichage de la grille : \n";
  affiche_grille g

(*Test présence de cycle*)
let test5 () =
  (*Vérifie que les cycles sont détectés correctement*)
  assert(cycle g g.(0).(5) = false);
  (*Vérifie que les cycles sont détectés avec une référence sur une autre case*)
  assert(cycle g g.(0).(4) = true);
  (*Vérifie que les cycles dans les opérateurs unaires sont détectés*)
  assert(cycle g g.(1).(3) = true);
  (*Verifie que les cycles dans les opérateur binaires sont détectés*)
  assert(cycle g g.(1).(4) = true);
  (*Verification que les cycles dans un opérateur de réduction sont signalés*)
  assert(cycle g g.(1).(5) = true)

(*Test de l'evalutation d'une expression*)
let test6 () =
  (*Verifie que les expressions basiques sont correctement évaluées*)
  assert(eval_expr g.(0).(0) g = RVide);
  assert(eval_expr g.(0).(1) g = RChaine "salut");
  assert(eval_expr g.(0).(2) g = REntier 9);
  assert(eval_expr g.(0).(3) g = RFlottant 3.0);
  assert(eval_expr g.(0).(4) g = Erreur Cycle_detecte);
  assert(eval_expr g.(0).(5) g = REntier 8);
  assert(eval_expr g.(0).(6) g = REntier 3);
  assert(eval_expr g.(0).(7) g = Erreur Argument_non_valide);
  assert(eval_expr g.(0).(8) g = RVide);
  assert(eval_expr g.(1).(0) g = Erreur Cycle_detecte);
  assert(eval_expr g.(1).(1) g = Erreur (Mauvais_indice (5,1323)));
  assert(eval_expr g.(1).(3) g = Erreur Cycle_detecte);
  assert(eval_expr g.(1).(4) g = Erreur Cycle_detecte);
  (*(0,0)=> 0 (0,1) => Erreur arg (1,0) => Cycle (1,1) => Cycle*)
  assert(eval_expr g.(1).(5) g = Erreur Cycle_detecte)


(*Affichage de la grille resultat*)
let test7 () =
  let g = eval_grille g in
  print_endline "\nAffiche de la grille évalué : \n";
  affiche_grille_resultat g


let run_tests () =
  let liste_tests =
    [("création grille", test1); ("affectation grille", test2);
     ("chaine expression", test3); ("affichage grille", test4);
     ("cycle grille", test5); ("evaluation expr", test6);
     ("affiche grille result", test7)
    ]
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
