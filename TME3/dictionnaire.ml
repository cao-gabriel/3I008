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

let dico = [Lettre('c', false, [Lettre('a', false, [Lettre('t', true, [])])])]

let ajoute (mot : string) (dico : arbre_lex) : arbre_lex =
  let rec loop (indice : int) (dico : arbre_lex) : arbre_lex =
    if (indice = String.length mot)
    then []
    else
      match dico with
      |[] -> [Lettre(String.get mot indice, indice + 1= String.length mot,
                     loop (indice + 1) dico)]
      |Lettre(c, b, fils)::xs ->
        begin
          if (c = String.get mot indice) then Lettre(c, b, (loop (indice + 1) fils))::xs
          else Lettre(c,b, fils)::(loop indice xs)
        end

  in
  try
    if existe mot dico then raise(Deja_defini(mot))
    else loop 0 dico
  with
    Deja_defini(mot) -> dico

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
      if fin then loop fils ((current_word^(Char.escaped c))::acc) (current_word ^ (Char.escaped c)) @ (forest_list xs current_word)
      else loop fils acc (current_word ^ Char.escaped c) @ (forest_list xs current_word)
  and forest_list dico current_word =
    match dico with
    |[] -> []
    |Lettre(c, fin, fils)::xs ->
      if fin then loop fils [current_word ^ Char.escaped c] (current_word ^ Char.escaped c) @ forest_list xs current_word
      else loop fils [] (current_word ^ Char.escaped c) @ forest_list xs current_word
  in loop dico [] ""

let affiche (dico : arbre_lex) : unit =
  let l = list_de_dict dico in
  List.iter (print_endline) l

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
        |_ -> raise (Invalid_argument "ezfn")
      end
    |action::[] ->
      begin
        match action with
        |"affiche" -> affiche !dico ; main()
        |"quitte" -> ()
        |_ -> raise (Invalid_argument "vzfz")
      end
    |_ -> raise (Invalid_argument "vzirg")
