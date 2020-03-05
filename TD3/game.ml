type classe = Archer | Barbare | Magicien
type objet = Piece | Poulet | Eponge
type personnage = {classe : classe ; exp : int; sac : (objet * int )list}
type race = Golem | Sanglier | Moustique of int
type monstre = {race : race; item : objet}

let ramasse sac (item: objet) =
  let rec loop sac new_sac =
    match sac with
    | [] -> (item,1)::new_sac
    | (it, nb)::xs ->
      begin
        if(it = item)
        then new_sac@((it, nb+1)::xs)
        else loop xs ((it, nb)::new_sac)
      end
  in
  loop sac []

let affiche_sac sac =
  List.iter
    (fun (it, nb) ->
    match it with
    | Piece -> Printf.printf "%d pieces\n" nb
    | Poulet -> Printf.printf "%d poulet\n" nb
    | Eponge -> Printf.printf "%d eponges\n" nb) sac

let gen_monstre () : monstre =
  let rand_nb_mous = Random.int 10 + 2 in
  let races = [Golem; Sanglier; Moustique(rand_nb_mous)] in
  let items = [Piece; Poulet ; Eponge] in
  let rand_race = Random.int (List.length races) in
  let rand_item = Random.int (List.length items) in
  {race = (List.nth races rand_race); item = (List.nth items rand_item)}

let frappe cls =
  match cls with
  | Barbare -> 10
  | Magicien -> 4
  | Archer -> 5

let frappe_monstre r =
  match r with
  | Golem -> 4
  | Sanglier -> 2
  | Moustique(x) -> x/2

exception Mort

let combat p m =
  let rec loop hpp hpm =
    if hpp <= 0 then raise (Mort)
    else if hpm <= 0 then {p with exp = p.exp + 1 ; sac = ramasse p.sac m.item}
    else loop (hpp - (frappe_monstre m.race)) (hpm - (frappe p.classe))
  in
  loop 20 20

let monstre_to_string mob =
  match mob with
  | Golem -> "un terrRRrrible golem"
  | Sanglier -> "un énorme sanglier"
  | Moustique(x) -> "une armée de " ^ string_of_int x ^ " moustiques !"


let malheureuse_rencontre p =
  let mob = gen_monstre () in
  print_endline ("Vous tombez nez à nez avec " ^ monstre_to_string mob.race);
  combat p mob

let main ()=
  Random.self_init () ;
  let nb_combat = Random.int 5 + 3 in
  let player = ref {classe = Archer ; exp = 0; sac = []} in
  let rec loop combat_left =
    if combat_left = 0 then
      begin
        print_endline "Bravo, vous avez vaincu !" ;
        print_endline "Voici votre butin :" ;
        affiche_sac !player.sac
      end
    else
    begin
      player := malheureuse_rencontre !player;
      loop (combat_left - 1)
    end
  in
  try
    loop nb_combat
  with
  | Mort -> print_endline "Vous êtes mort !."
