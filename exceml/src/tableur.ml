(** Types *)
exception Out_of_bounds of int * int
type resultat =
  | RVide
  | RChaine of string
  | REntier of int
  | RFlottant of float
  | Erreur of erreur

and erreur =
  (*Erreur retourné lorque l'expression pointe vers une case non valide*)
  |Mauvais_indice of (int * int)
  (*Erreur retourné lorsque un cycle est détécté lors de l'évaluation de l'expression*)
  | Cycle_detecte
  (*Erreur retourné lorque l'expression entrée par l'utilisateur n'est pas valide*)
  | Argument_non_valide
  (*Erreur affiché lorsque l'évaluation de la case référencé donne une expression non valide*)
  | CaseRefError of int * int


type expr =
  | Vide
  | Chaine of string
  | Entier of int
  | Flottant of float
  | Case of int * int
  | Unaire of op_unaire
  | Binaire of op_binaire
  | Reduction of op_reduction
and op_unaire = {app1 : resultat -> resultat ; operande : expr}
and op_binaire ={app2 : resultat -> resultat -> resultat;
                 gauche : expr;
                 droite : expr}
and op_reduction = {app : resultat -> resultat -> resultat ;
                    init : resultat;
                    case_debut : int * int;
                    case_fin : int * int}



type grille = expr array array

(** Fonctions *)

let cree_grille (i : int) (j : int) : grille =
  Array.make_matrix i j Vide

let result_to_string (e : resultat) : string =
  match e with
  | RVide -> ""
  | RChaine(str) -> str
  | REntier(x) -> string_of_int x
  | RFlottant(f) -> string_of_float f
  | Erreur(Mauvais_indice(i,j)) ->
    "IndexE#" ^ string_of_int  i ^ "," ^ string_of_int j
  | Erreur(Cycle_detecte) ->
    "CycleE#"
  | Erreur(Argument_non_valide) ->
    "ArgsE#"
  | Erreur(CaseRefError(i,j)) ->
    "CRefE#"^ string_of_int  i ^ "," ^ string_of_int j

let rec expr_to_string (e : expr) : string =
  match e with
  | Vide -> ""
  | Chaine(str) -> str
  | Entier(x) -> string_of_int x
  | Flottant(f) -> string_of_float f
  | Case(i,j) -> "@("^string_of_int i^","^string_of_int j^")"
  (*op_u(@(1,2))*)
  | Unaire(ou) -> "op_u("^(expr_to_string ou.operande)^")"
  (*op_b(1,1)*)
  | Binaire(ob) -> "op_b("^(expr_to_string ob.gauche)^","^ (expr_to_string ob.droite)^")"
  (*op_r([1,2]:[3,4])*)
  | Reduction(ored) ->
    let (i,j) = ored.case_debut in
    let (k,l) = ored.case_fin in
    "op_r(["^string_of_int i^","^string_of_int j^"]:"
    ^"["^string_of_int k^","^string_of_int l^"])"


let affiche_grille (g : grille) : unit =
  Array.iter
    (fun line ->
       begin
         (Array.iter (fun e -> Format.printf "|%20s" (expr_to_string e)) line);
         Format.print_string "|\n"
       end)
    g


let  cycle (g : grille) (e : expr) : bool =
  let cache = ref [] in
  let rec loop e =
    match e with
    | Vide -> false
    | Chaine(_) -> false
    | Entier _ -> false
    | Flottant _ -> false
    | Case (i,j) ->

      if List.mem (i,j) !cache
      then true
      else
        (try
           (cache := (i,j)::(!cache); loop g.(i).(j))
         with
         (*if out of bounds happen then throw exception
          *with the index of the current loop*)
         | Invalid_argument _ -> raise (Out_of_bounds (i,j))
         | Out_of_bounds (_)-> raise (Out_of_bounds (i,j)))
    | Unaire op_unaire ->
      loop op_unaire.operande
    | Binaire op_binaire ->
      loop op_binaire.droite || loop op_binaire.gauche
    | Reduction op_reduction ->
      let exist_cycle = ref false in
      let (i_s, j_s) = op_reduction.case_debut in
      let (i_f, j_f) = op_reduction.case_fin in
      let (i_s, j_s) = (min i_s i_f, min j_s j_f) in
      let (i_f, j_f) = (max i_s i_f, max j_s j_f) in
      for i = i_s to i_f do
        for j = j_s to j_f do
          exist_cycle := !exist_cycle || loop g.(i).(j)
        done;
      done; !exist_cycle
  in

  loop e

(**)

let rec eval_expr (e : expr) (g : grille): resultat =
  try
    match e with
    | Vide -> RVide
    | Chaine(s) -> RChaine(s)
    | Entier(x) -> REntier(x)
    | Flottant(f) -> RFlottant(f)
    | Case(i,j) ->
      if i >= Array.length g || i < 0 || j < 0 || j >= Array.length g.(0)
      then (print_int i; print_int j; print_newline ();Erreur (Mauvais_indice(i,j)))
      else if cycle g e then Erreur(Cycle_detecte)
      else (eval_expr (g.(i).(j)) g)
    | Unaire(ou) ->
      if cycle g ou.operande then Erreur(Cycle_detecte)
      else ou.app1 (eval_expr ou.operande g)
    | Binaire(ob) ->
      if cycle g ob.gauche || cycle g ob.droite then Erreur(Cycle_detecte)
      else ob.app2 (eval_expr ob.gauche g) (eval_expr ob.droite g)
    | Reduction(ored) ->
      (let (i_start, j_start) = ored.case_debut in
       let (i_end, j_end) = ored.case_fin in
       let res = ref ored.init in
       for i = i_start to i_end do
         for j = j_start to j_end do
           if cycle g g.(i).(j) then raise Not_found
           else res := ored.app !res (eval_expr g.(i).(j) g)
         done;
       done;
       !res)
  with
  | Out_of_bounds (i,j) -> Erreur (CaseRefError (i,j))
  | Not_found -> Erreur Cycle_detecte





let eval_grille (g : grille) : resultat array array =
  Array.map (fun line -> Array.map (fun e -> eval_expr e g) line) g



let affiche_grille_resultat (rg : resultat array array) : unit =
  Array.iter
    (fun line ->
       (Array.iter (fun e -> Format.printf "|%20s" (result_to_string e)) line);
       Format.print_string "|\n") rg

let abs (e : expr) : expr =
  let app res =
    match res with
    | RVide -> RVide
    | RChaine(_) -> Erreur(Argument_non_valide)
    | REntier(x) -> REntier(abs x)
    | RFlottant(f) -> RFlottant (abs_float f)
    | Erreur(_) -> res
  in
  Unaire({app1 = app; operande = e})

let addition (res1 : resultat) (res2 : resultat) : resultat=
  match res1 with
  | RVide ->
    begin
      match res2 with
      | RVide -> REntier(0)
      | RChaine(_) -> Erreur(Argument_non_valide)
      | REntier(x) -> REntier(x)
      | RFlottant(f) -> RFlottant (f)
      | Erreur(_) -> res2
    end
  | RChaine(_) -> Erreur(Argument_non_valide)
  | REntier(x) ->
    begin
      match res2 with
      | RVide -> REntier(x)
      | RChaine(_) -> Erreur(Argument_non_valide)
      | REntier(y) -> REntier(x + y)
      | RFlottant(f) -> RFlottant (f +. float_of_int x)
      | Erreur(_) -> res2
    end
  | RFlottant(f) ->
    begin
      match res2 with
      | RVide -> RFlottant(f)
      | RChaine(_) -> Erreur(Argument_non_valide)
      | REntier(y) -> RFlottant(f +. float_of_int y)
      | RFlottant(f1) -> RFlottant (f +. f1)
      | Erreur(_) -> res2
    end
  | Erreur(_) -> res1

let add (e1 : expr) (e2 : expr) : expr =
  Binaire({app2 = addition; gauche = e1; droite = e2})

let somme case_debut case_fin : expr =
  Reduction({app = addition; init = REntier(0); case_debut = case_debut;
             case_fin = case_fin})
