(** Types *)
exception Out_of_bounds of int * int
type resultat =
  | RVide
  | RChaine of string
  | REntier of int
  | RFlottant of float
  | Erreur of erreur

and erreur =
  (*Erreur retourné lorque l'expression pointe vers une case non valide*)
  |Mauvais_indice of (int * int)
  (*Erreur retourné lorsque un cycle est détécté lors de l'évaluation de l'expression*)
  | Cycle_detecte
  (*Erreur retourné lorque l'expression entrée par l'utilisateur n'est pas valide*)
  | Argument_non_valide
  (*Erreur affiché lorsque l'évaluation de la case référencé donne une expression non valide*)
  | CaseRefError of int * int


type expr =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Case of int * int
  | Unaire of op_unaire
  | Binaire of op_binaire
  | Reduction of op_reduction
and op_unaire = {app1 : resultat -> resultat ; operande : expr}
and op_binaire ={app2 : resultat -> resultat -> resultat;
                 gauche : expr;
                 droite : expr}
and op_reduction = {app : resultat -> resultat -> resultat ;
                    init : resultat;
                    case_debut : int * int;
                    case_fin : int * int}



type grille = expr array array

(** Fonctions *)

let cree_grille (i : int) (j : int) : grille =
  Array.make_matrix i j Vide

let result_to_string (e : resultat) : string =
  match e with
  | RVide -> ""
  | RChaine(str) -> str
  | REntier(x) -> string_of_int x
  | RFlottant(f) -> string_of_float f
  | Erreur(Mauvais_indice(i,j)) ->
    "IndexE#" ^ string_of_int  i ^ "," ^ string_of_int j
  | Erreur(Cycle_detecte) ->
    "CycleE#"
  | Erreur(Argument_non_valide) ->
    "ArgsE#"
  | Erreur(CaseRefError(i,j)) ->
    "CRefE#"^ string_of_int  i ^ "," ^ string_of_int j

let rec expr_to_string (e : expr) : string =
  match e with
  | Vide -> ""
  | Chaine(str) -> str
  | Entier(x) -> string_of_int x
  | Flottant(f) -> string_of_float f
  | Case(i,j) -> "@("^string_of_int i^","^string_of_int j^")"
  (*op_u(@(1,2))*)
  | Unaire(ou) -> "op_u("^(expr_to_string ou.operande)^")"
  (*op_b(1,1)*)
  | Binaire(ob) -> "op_b("^(expr_to_string ob.gauche)^","^ (expr_to_string ob.droite)^")"
  (*op_r([1,2]:[3,4])*)
  | Reduction(ored) ->
    let (i,j) = ored.case_debut in
    let (k,l) = ored.case_fin in
    "op_r(["^string_of_int i^","^string_of_int j^"]:"
    ^"["^string_of_int k^","^string_of_int l^"])"


let affiche_grille (g : grille) : unit =
  Array.iter
    (fun line ->
       begin
         (Array.iter (fun e -> Format.printf "|%20s" (expr_to_string e)) line);
         Format.print_string "|\n"
       end)
    g



let (cycle, free_cycle) =
  let cache = ref [] in
  (let rec loop g e =
     match e with
     | Vide -> false
     | Chaine(_) -> false
     | Entier _ -> false
     | Flottant _ -> false
     | Case (i,j) ->

       if List.mem (i,j) !cache
       then true
       else
         (try
            (cache := (i,j)::(!cache); loop g g.(i).(j))
          with
          (*if out of bounds happen then throw exception
           *with the index of the current loop*)
          | Invalid_argument _ -> raise (Out_of_bounds (i,j))
          | Out_of_bounds (_)-> raise (Out_of_bounds (i,j)))
     | Unaire op_unaire ->
       loop g op_unaire.operande
     | Binaire op_binaire ->
       loop g op_binaire.droite || loop g op_binaire.gauche
     | Reduction op_reduction ->
       let exist_cycle = ref false in
       let (is, js) = op_reduction.case_debut in
       let (ie, je) = op_reduction.case_fin in
       let (i_s, j_s) = (min is ie, min js je) in
       let (i_f, j_f) = (max is ie, max js je) in
       for i = i_s to i_f do
         for j = j_s to j_f do
           exist_cycle := !exist_cycle || loop g g.(i).(j)
         done;
       done; !exist_cycle
   in
   loop , (fun () -> cache := []))

(*
let cycle (g : grille) (e : expr) : bool =
  let cache = ref [] in
  let rec loop e =
    match e with
    | Vide -> false
    | Chaine(_) -> false
    | Entier _ -> false
    | Flottant _ -> false
    | Case (i,j) ->

      if List.mem (i,j) !cache
      then true
      else
        (try
           (cache := (i,j)::(!cache); loop g.(i).(j))
         with
         (*if out of bounds happen then throw exception
          *with the index of the current loop*)
         | Invalid_argument _ -> raise (Out_of_bounds (i,j))
         | Out_of_bounds (_)-> raise (Out_of_bounds (i,j)))
    | Unaire op_unaire ->
      loop op_unaire.operande
    | Binaire op_binaire ->
      loop op_binaire.droite || loop op_binaire.gauche
    | Reduction op_reduction ->
      let exist_cycle = ref false in
      let (is, js) = op_reduction.case_debut in
      let (ie, je) = op_reduction.case_fin in
      let (i_s, j_s) = (min is ie, min js je) in
      let (i_f, j_f) = (max is ie, max js je) in
      for i = i_s to i_f do
        for j = j_s to j_f do
          exist_cycle := !exist_cycle || loop g.(i).(j)
        done;
      done; !exist_cycle
  in
  loop e
*)

(**)
(*Evaluation d'une expression par recherche récursive dans une table de hachage
 *de l'expression*)
(*let eval_expr  =
  let cache = Hashtbl.create 50 in
  let rec loop (e : expr) (g : grille): resultat =
    try
      Hashtbl.find cache e
    with
    | Not_found ->
      begin
        try
          let result =
            match e with
            | Vide -> RVide
            | Chaine(s) -> RChaine(s)
            | Entier(x) -> REntier(x)
            | Flottant(f) -> RFlottant(f)
            | Case(i,j) ->
              if i >= Array.length g || i < 0 || j < 0 || j >= Array.length g.(0)
              then Erreur (Mauvais_indice(i,j))
              else if cycle g e then Erreur(Cycle_detecte)
              else (loop (g.(i).(j)) g)
            | Unaire(ou) ->
              if cycle g ou.operande then Erreur(Cycle_detecte)
              else ou.app1 (loop ou.operande g)
            | Binaire(ob) ->
              if cycle g ob.gauche || cycle g ob.droite then Erreur(Cycle_detecte)
              else ob.app2 (loop ob.gauche g) (loop ob.droite g)
            | Reduction(ored) ->
              (let (i_start, j_start) = ored.case_debut in
               let (i_end, j_end) = ored.case_fin in
               let res = ref ored.init in
               for i = i_start to i_end do
                 for j = j_start to j_end do
                   if cycle g g.(i).(j) then raise Not_found
                   else res := ored.app !res (loop g.(i).(j) g)
                 done;
               done;
               !res)
          in
          (*Sauvegarde dans le cache*)
          Hashtbl.add cache e result; result
        with
        | Out_of_bounds (i,j) -> Erreur (CaseRefError (i,j))
        | Not_found -> Erreur Cycle_detecte
      end
  in loop*)

let( eval_expr, free_eval)  =
  let cache = Hashtbl.create 50 in
  (let rec loop (e : expr) (g : grille): resultat =
     try
       Hashtbl.find cache e
     with
     | Not_found ->
       begin
         try
           let result =
             match e with
             | Vide -> RVide
             | Chaine(s) -> RChaine(s)
             | Entier(x) -> REntier(x)
             | Flottant(f) -> RFlottant(f)
             | Case(i,j) ->
               if i >= Array.length g || i < 0 || j < 0 || j >= Array.length g.(0)
               then Erreur (Mauvais_indice(i,j))
               else if cycle g e then Erreur(Cycle_detecte)
               else (loop (g.(i).(j)) g)
             | Unaire(ou) ->
               if cycle g ou.operande then Erreur(Cycle_detecte)
               else ou.app1 (loop ou.operande g)
             | Binaire(ob) ->
               if cycle g ob.gauche || cycle g ob.droite then Erreur(Cycle_detecte)
               else ob.app2 (loop ob.gauche g) (loop ob.droite g)
             | Reduction(ored) ->
               (let (i_start, j_start) = ored.case_debut in
                let (i_end, j_end) = ored.case_fin in
                let res = ref ored.init in
                for i = i_start to i_end do
                  for j = j_start to j_end do
                    if cycle g g.(i).(j) then raise Not_found
                    else res := ored.app !res (loop g.(i).(j) g)
                  done;
                done;
                !res)
           in
           (*Sauvegarde dans le cache*)
           Hashtbl.add cache e result; result
         with
         | Out_of_bounds (i,j) -> Erreur (CaseRefError (i,j))
         | Not_found -> Erreur Cycle_detecte
       end
   in loop, fun () -> Hashtbl.clear cache)




let eval_grille (g : grille) : resultat array array =
  (*clear the memory of eval and cycle if the user modified the table*)
  free_cycle (); free_eval ();
  Array.map (fun line -> Array.map (fun e -> eval_expr e g) line) g



let affiche_grille_resultat (rg : resultat array array) : unit =
  Array.iter
    (fun line ->
       (Array.iter (fun e -> Format.printf "|%20s" (result_to_string e)) line);
       Format.print_string "|\n") rg


let rec puissance_exp x n =
  if n = 0 then 1
  else
    let p = n / 2 in
    let np = puissance_exp x p in
    if n mod 2 = 0 then np * np
    else np * np * x


let rec puissance_exp_float (x : float) (n : int) : float =
  if n = 0 then 1.
  else
    let p = n / 2 in
    let np = puissance_exp_float x p in
    if n mod 2 = 0 then np *. np
    else np *. np *. x

(**************UNARY OPERATION***************)
(*La valeur absolu d'un entier ou d'un flottant*)
let absolu (res : resultat) : resultat =
  match res with
  | RVide | RChaine(_) | Erreur _-> Erreur(Argument_non_valide)
  | REntier(x) -> REntier(abs x)
  | RFlottant(f) -> RFlottant (abs_float f)



(*L'oppose d'un entier ou d'un flottant*)
let oppose (res : resultat) : resultat =
  match res with
  | RVide | RChaine(_) | Erreur _-> Erreur(Argument_non_valide)
  | REntier(x) -> REntier(- x)
  | RFlottant(f) -> RFlottant (-. f)




(*L'inverse d'un entier ou d'un flottant*)
let inverse (res : resultat) : resultat =
  match res with
  | RVide | RChaine(_) | Erreur _-> Erreur(Argument_non_valide)
  | REntier(x) -> REntier(1 / x)
  | RFlottant(f) -> RFlottant (1. /. f)






(**************BINARY OPERATION***************)
(*Addition de 2 entiers et de 2 flottants posssibles*)
let addition (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (x + y)
  | RFlottant x, RFlottant y -> RFlottant (x +. y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Addition de 2 entiers et de 2 flottants posssibles*)
let soustraction (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (x - y)
  | RFlottant x, RFlottant y -> RFlottant (x -. y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Multiplication de 2 entiers et de 2 flottants posssibles*)
let multiplication (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (x * y)
  | RFlottant x, RFlottant y -> RFlottant (x *. y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Division de 2 entiers et de 2 flottants posssibles*)
let division (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (x / y)
  | RFlottant x, RFlottant y -> RFlottant (x /. y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*puissance de 2 entiers et de 2 flottants posssibles*)
let puissance (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (puissance_exp x y)
  | RFlottant x, REntier y -> RFlottant (puissance_exp_float x y)
  | RFlottant x, RFlottant y -> RFlottant (x ** y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Maximum de 2 entiers et de 2 flottants posssibles*)
let maximum (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (max x y)
  | RFlottant x, RFlottant y -> RFlottant (max x y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Maximum de 2 entiers et de 2 flottants posssibles*)
let minimum (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (min x y)
  | RFlottant x, RFlottant y -> RFlottant (min x y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide

(*Moyenne de 2 entiers et de 2 flottants posssibles*)
let moyenne (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier ((x + y) / 2)
  | RFlottant x, RFlottant y -> RFlottant ((x +. y) /. 2.)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide




(*********UNARY EXPRESSION************)

let myabs (e : expr) : expr =
  Unaire({app1 = absolu; operande = e})

let opp (e : expr) : expr =
  Unaire({app1 = oppose; operande = e})

let inv (e : expr) : expr =
  Unaire({app1 = inverse; operande = e})




(*********BINARY EXPRESSION************)
let add (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = addition; gauche = e1; droite = e2})
let sous (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = soustraction; gauche = e1; droite = e2})
let mult (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = multiplication; gauche = e1; droite = e2})
let div (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = division; gauche = e1; droite = e2})
let puis (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = puissance; gauche = e1; droite = e2})
let mymax (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = maximum; gauche = e1; droite = e2})
let mymin (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = minimum; gauche = e1; droite = e2})
let mymoy (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = moyenne; gauche = e1; droite = e2})



(*********REDUCTION EXPRESSION************)
let somme case_debut case_fin : expr =
  Reduction({app = addition; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
let produit case_debut case_fin : expr =
  Reduction({app = multiplication; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
let max_reduce case_debut case_fin : expr =
  Reduction({app = maximum; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
let min_reduce case_debut case_fin : expr =
  Reduction({app = minimum; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
let moy_reduce case_debut case_fin : expr =
let ((i,j),(k,l)) = (case_debut, case_fin) in
let nb_case = (abs (i - j)) * (abs (k - l)) in
let moy_red (res1 : resultat) (res2 : resultat) : resultat=
  match res1,res2 with
  | REntier x, REntier y -> REntier (x / nb_case + y)
  | RFlottant x, RFlottant y -> RFlottant (x /. (float_of_int nb_case) +. y)
  (*non valide*)
  | RFlottant _, _ | _, RFlottant _
  | RChaine _, _ | _, RChaine _
  | REntier _, _ | _, REntier _
  | RVide, _ | _, RVide
  | Erreur _, _ -> Erreur Argument_non_valide
in
  Reduction({app = moy_red; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
