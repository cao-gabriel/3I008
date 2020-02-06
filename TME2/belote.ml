type couleur = Pique | Coeur | Carreau | Trefle
type carte = As of couleur
           | Roi of couleur
           | Dame of couleur
           | Valet of couleur
           | Autre of couleur * int

let valeur (couleur_atout : couleur) (cr : carte) : int =
  match cr with
  | As _ -> 11
  | Roi _ -> 4
  | Dame _ -> 3
  | Valet c -> if c = couleur_atout then 20 else 2
  | Autre (_, 10) -> 10
  | Autre (c, 9) -> if c = couleur_atout then 14 else 0
  | _ -> 0

let rec valeur_jeu (couleur_atout : couleur) (liste_carte : carte list) : int =
  match liste_carte with
  | [] -> 0
  | x::xs -> (valeur couleur_atout x) + (valeur_jeu couleur_atout xs)

let jeu = (Valet(Carreau)::(Autre(Trefle, 10)::(Autre(Trefle,7)::(Autre(Carreau, 8)::(Autre(Pique,9)::[])))))
