type noeud = Lettre of (char * bool * arbre_lex)
and arbre_lex = noeud list
exception Deja_defini of string


let existe (mot : string) (dico : arbre_lex) : bool =
  let rec loop (indice : int) (dico : arbre_lex) =
    match dico with
    |[] -> false
    |Lettre(c, b, fils)::xs ->
      begin
        if (c = String.get mot indice)
        then
          begin
            if (String.length mot = indice + 1)
            then b
            else loop (indice + 1) fils
          end
        else loop indice xs
      end
  in
  loop 0 dico

let ajoute (mot : string) (dico : arbre_lex) : arbre_lex =
  let rec loop (indice : int) (dico : arbre_lex) : arbre_lex =
    if (indice = String.length mot)
    then dico
    else
      match dico with
      |[] -> [Lettre(String.get mot indice, indice + 1= String.length mot,
                     loop (indice + 1) dico)]
      |Lettre(c, b, fils)::xs ->
        begin
          if (c = String.get mot indice)
          then
            begin
              if (indice + 1 = String.length mot) then Lettre(c, true, (loop (indice + 1) fils))::xs
              else Lettre(c, b, (loop (indice + 1) fils))::xs
            end
          else Lettre(c,b, fils)::(loop indice xs)
        end

  in
  if existe mot dico then raise(Deja_defini(mot))
  else loop 0 dico


let construit (l : string list) : arbre_lex =
  let rec loop (l : string list) (acc: arbre_lex) =
    match l with
    |[] -> acc
    | x::xs -> loop xs (ajoute x acc)
  in
  loop l []

let list_de_dict (dico : arbre_lex) : string list =
  let rec loop (dico : arbre_lex) (acc: string list) (current_word : string)=
    match dico with
    |[] -> acc
    |Lettre(c, fin, fils)::xs ->
      let new_str = (current_word^(Char.escaped c)) in
      if fin then loop fils (new_str::acc) new_str @ (forest_list xs current_word)
      else loop fils acc new_str @ (forest_list xs current_word)
  and forest_list dico current_word =
    match dico with
    |[] -> []
    |Lettre(c, fin, fils)::xs ->
      let new_str = (current_word^(Char.escaped c)) in
      if fin then loop fils [new_str] new_str @ forest_list xs current_word
      else loop fils [] new_str @ forest_list xs current_word
  in loop dico [] ""

let affiche (dico : arbre_lex) : unit =
  let l = list_de_dict dico in
  List.iter (print_endline) l

let enregistre (file : string) (dico : arbre_lex): unit =
  let out = open_out file in
  List.iter (fun x -> output_string out x ; output_string out "\n") (list_de_dict dico) ;
  close_out out

let ouvre (file : string) : arbre_lex =
  let rec lines fp =
    try
      let line = input_line fp in
      line ::lines fp
    with
    | End_of_file -> []
  in
  let file_in = open_in file in
  let dico = construit (lines file_in) in
  close_in file_in ; dico



let rec main =
  let dico = ref [] in
  fun () ->
    (print_string "> ") ;
    let input = String.split_on_char ' ' (read_line ()) in
    match input with
    |action::mot::[] ->
      begin
        match action with
        |"existe" ->
          (if existe mot !dico then print_endline "oui"
           else print_endline "non" ); main ()
        |"ajoute" -> dico := ajoute mot !dico; main ()
        |"enregistre" -> enregistre mot !dico ; main ()
        |"ouvre" -> dico := (ouvre mot) ; main ()
        |_ -> print_endline "Erreur : commande inconnu
    ou mal ecrite." ; main ()
      end
    |action::[] ->
      begin
        match action with
        |"affiche" -> affiche !dico ; main()
        |"quitte" -> ()
        |_ -> print_endline "Erreur : commande inconnu
    ou mal ecrite." ; main ()
      end
    |_ -> print_endline "Erreur : commande inconnu
ou mal ecrite." ; main ()

let start () = print_string "Bienvenue !"; main ()
