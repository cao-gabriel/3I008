let cherche_car (car : char) (str : string) =
  let rec loop indice =
    if String.length str = indice then -1
    else
      begin
        if String.get str indice = car then indice
        else loop (indice + 1)
      end
  in
  loop 0


let ajout_espace (str : string) : string=
  let rec loop (start : int) (indice : int) (acc : string list) : string list=
    if (String.length str = indice) then (String.sub str start (indice - start))::acc
    else
    (if (String.get str indice = ',' || String.get str indice = '.')
    then loop (indice) (indice+1) ((String.sub str start (indice - start))::acc)
    else loop start (indice+1) acc)
  in
  String.concat " " (List.rev (loop 0 0 []))
