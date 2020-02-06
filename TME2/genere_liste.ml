(* Calcule le nombre de chiffre identique au début d'une liste*)
let rec calcul_prefixe (l : int list) : int =
  match l with
  | [] -> 0
  | x::y::xs ->
    if x = y then 1 + (calcul_prefixe (y::xs))
    else 1
  | [x] -> 1

let rec genere_liste l =
  let rec f  l i =
    if i = 0 then l
    else match l with
      | [] -> []
      | x::xs -> f xs (i - 1)
  in
  let pref = calcul_prefixe l in
  match l with
  | [] -> []
  | [x] ->  pref::(x::[])
  |x::xs -> pref::(x::(genere_liste (f l pref) ))

let rec genere (k : int) : (int list) list =
  if k = 0 then [[]]
  else if k = 1 then [[1]]
  else let suiv = genere (k - 1) in
    match suiv with
    | x::xs -> (genere_liste x)::suiv
    | _ -> suiv
