let rec cherche_car (chaine : string) (c : char) : int =
  if String.length chaine = 0 then -1
  else if (String.get chaine 0) = c then 0
  else 1 + (cherche_car (String.sub chaine 1 (String.length chaine - 1)) c)


let rec ajout_espace (chaine : string) : string =
  if String.length chaine = 0 then ""
  else
    let c = String.sub chaine 0 1 in
    if c = "."  then ". " ^ ajout_espace (String.sub chaine 1 (String.length chaine - 1))
    else if c = ","  then ", " ^ ajout_espace (String.sub chaine 1 (String.length chaine - 1))
    else c ^  ajout_espace (String.sub chaine 1 (String.length chaine - 1))
