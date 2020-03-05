let rec somme_tab_aux tab l acc =
  try
    match l with
    | i::tl ->
      somme_tab_aux tab tl (acc + tab.(i))
    | [] -> acc
  with
  | Invalid_argument _ ->
    Printf.fprintf stderr "out of bounds !" ;
    exit 1

let somme_tab tab l = somme_tab_aux tab l 0
(*Fonction non récursive terminale car existence du try with qui augmente la pile à chaque appel*)


(*A NE PAS EXECUTER*)
let rec lines fp =
  try
    input_line fp :: lines fp
  with
  | End_of_file -> []

(*L'ordre de l'evaluation des arguments est inconnu
 *cette fonction peut donc faire une boucle infini
 *en evaluant l'expression à gauche de l'opérateur cons*)

let rec lines fp =
  try
    let line = input_line fp in
    line :: lines fp
  with
  | End_of_file -> []
(*En forcant la lecture du fichier avant l'appel  récursif,
 *plus de risque de boucle infinie*)

let rec lines fp =
  try
    input_line fp ::lines fp
  with
  | _ -> []
(*Ici, on refait une boucle infinie ce qui va se traduire par
 *une consommation importante de la pile. L'exception StackOverFlow
 *sera donc levée, or ici on attrape toutes les exceptions donc
 * on renvoie une liste vide*)
