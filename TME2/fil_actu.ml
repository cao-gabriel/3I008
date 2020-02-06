type utilisateur = string * (string list)
type cible = Tout_le_monde | Amis | Amis_Spec of string list
type publication = cible * string * string

let affiche_publication ((c, aut, message) : publication) : unit =
  print_string aut ; print_string " : " ; print_string message

let rec acces_autorise (per : utilisateur) (list_pub : publication list) : publication list =
  match list_pub with
  | [] -> []
  | v::xs -> let (x, aut, message) = v in
    match x with
    | Tout_le_monde -> v::(acces_autorise per xs)
    | Amis ->
      let (user, amis) = per in
      if List.exists (fun x -> x=aut) amis then  v::(acces_autorise per xs)
      else (acces_autorise per xs)
    | Amis_Spec(l) ->
      let (user, amis) = per in
      if List.exists (fun x -> x=user) l then  v::(acces_autorise per xs)
      else (acces_autorise per xs)

let rec categoriser (publications : publication list) : publication list * publication list =
  match publications with
  | [] -> ([], [])
  | (x, aut, message)::xs ->
    begin
      match x with
      | Tout_le_monde -> let (pub, priv) = categoriser xs in ((x, aut, message)::pub, priv)
      | _ -> let (pub, priv) = categoriser xs in (pub, (x, aut, message)::priv)
    end
