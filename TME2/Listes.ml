let calcul_prefixe  (l : 'a list) =
  let rec loop (l : 'a list) (cpt : int) (va : 'a)  : int=
    match l with
    | x::xs when x = va -> loop xs (cpt+1) va
    | _ -> cpt
  in match l with
  | [] -> 0
  | x::xs -> loop l 0 x

let rec avance_index (l : 'a list) (n : int) =
  if (n = 0) then l
  else match l with
  | [] -> []
  | x::xs -> avance_index xs (n - 1)

let genere_liste (l : 'a list) =
  let rec loop (l : 'a list) (acc : 'a list) : 'a list=
    match l with
    | x::_ -> let index = calcul_prefixe l in loop (avance_index l index) (x::index::acc)
    | _ -> acc
  in
  List.rev (loop l [])


let genere (k : int) =
  let rec loop (k : int) (acc : 'a list list) =
    if k = 0 then acc
    else loop (k -1) ((genere_liste (List.hd acc))::acc)
  in
  if k = 0 then []
  else List.rev (loop k [[1]])
