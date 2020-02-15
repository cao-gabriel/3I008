type utilisateur = string * string list
type cible = ToutLeMonde | Amis | AmisSpe of string list
type publication = cible * string * string

let afficher_publication (pub : publication) : unit=
  let (cib, aut, mess) = pub in
  print_string (aut ^ " : ") ;
  print_string mess ;
  print_endline ""

let acces_autorise (uti : utilisateur) (pub : publication) : bool =
  let (nom_uti, uti_amis) = uti in
  let (cib_p, aut_p, _) = pub in
  match cib_p with
  | ToutLeMonde -> true
  | Amis ->
    begin
      let rec loop (l : string list) : bool=
        match l with
        | [] -> false
        | x::xs -> if x = aut_p then true else loop xs
      in
      loop uti_amis
    end
  | AmisSpe(l) ->
    begin
      let rec loop (l : string list) : bool =
        match l with
        | [] -> false
        | x::xs -> if x = nom_uti then true else loop xs
      in
      loop l
    end

let filtre_publications (uti : utilisateur) (lpub : publication list) : (publication list)=
  List.filter (acces_autorise uti) lpub

let categoriser (lpub : publication list) : publication list * publication list =
  ((List.filter
    (fun (cib_p, aut_p, _) -> match cib_p with
    | ToutLeMonde -> true
    | _ -> false) lpub)
  ,
  (List.filter
     (fun (cib_p, aut_p, _) -> match cib_p with
        | ToutLeMonde -> false
        | _ -> true) lpub))

let u1 = ("Durand", ["Human"; "Romain"; "Gabriel"])
let u2= ("Michel", ["Romain";"Human"])
let u3 =("Romain", ["Durand";"Michel"; "Gabriel"])
let u4 =("Gabriel", ["Romain";"Durand"])
let u5 = ("Human", ["Durand";"Michel"])

let p1 = (ToutLeMonde, "Human", "Je suis Human")
let p2 = (Amis, "Gabriel", "Je suis Gabriel")
let p3 = (AmisSpe(["Human"; "Durand"]), "Gabriel", "Je suis Gabriel mes chers amis")
