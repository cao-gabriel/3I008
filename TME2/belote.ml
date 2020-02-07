type couleur = Pique | Coeur | Carreau | Trefle

type carte = As of couleur | Roi of couleur | Dame of couleur| Valet of couleur
           | Autre of (couleur * int)

let valeur (couleur_atout : couleur) (cr : carte) =
  match cr with
  | As _  -> 11
  | Roi _ -> 4
  | Dame _ -> 3
  | Valet c -> if c = couleur_atout then 20 else 2
  | Autre(_, 10) -> 10
  | Autre(c, 9) -> if c = couleur_atout then 14 else 0
  | _ -> 0

let valeur_jeu (l : carte list) (couleur_atout : couleur) =
  let rec loop l acc =
    match l with
    | [] -> acc
    | x::xs -> loop xs (valeur couleur_atout x + acc)
  in
  loop l 0

let jeu = [Valet(Carreau);
           Autre(Coeur, 7);
           Autre(Trefle, 10);
           Autre(Carreau, 8);
           Autre(Pique, 9)]

let couleur_atout = Carreau
