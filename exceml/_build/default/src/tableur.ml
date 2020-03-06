(** Types *)
type resultat =
  | RVide
  | RChaine of string
  | REntier of int
  | RFlottant of float
  | Erreur of erreur

and erreur = Mauvais_indice of (int * int) | Cycle_detecte | Argument_non_valide | CaseRefError of int * int

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
                    init : expr;
                    case_debut : int * int;

                    case_fin : int * int}



type grille = expr array array

(** Fonctions *)

let cree_grille (i : int) (j : int) : grille =
  Array.make_matrix i j Vide

let rec expr_to_string (e : expr) : string =
  match e with
  | Vide -> ""
  | Chaine(str) -> str
  | Entier(x) -> string_of_int x
  | Flottant(f) -> string_of_float f
  | Case(i,j) -> "@("^string_of_int i^","^string_of_int j^")"
  | Unaire(ou) -> "u"^(expr_to_string ou.operande)
  | Binaire(ob) -> "b"^(expr_to_string ob.gauche)^";"^ (expr_to_string ob.droite)
  | Reduction(ored) -> "r "^(expr_to_string ored.init)

let affiche_grille (g : grille) : unit =
  Array.iter
    (fun line -> (Array.iter (fun e -> Format.printf "|%8s" (expr_to_string e)) line); Format.print_string "|\n") g

let  cycle (g : grille) (e : expr) : bool =
  let cache = ref [] in
  let rec loop e =
    match e with
    | Vide -> false
    | Chaine(_) -> false
    | Entier _ -> false
    | Flottant _ -> false
    | Case (i,j) ->
      begin
        if List.mem (i,j) !cache
        then true
        else
          begin
            cache := (i,j)::(!cache);
            loop g.(i).(j)
          end
      end
    | Unaire op_unaire ->
      loop op_unaire.operande
    | Binaire op_binaire ->
      loop op_binaire.droite && loop op_binaire.gauche
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
(*
  let rec loop list_pos (i,j) =
    match g.(i).(j) with
    | Case(k,l) ->
      begin
        if List.mem (k,l) list_pos then true
        else loop ((k,l)::list_pos) (k,l)
      end
    | Unaire(ou) ->
      begin
      match ou.operande with
      | Case(m,n) -> loop ((m,n)::list_pos) (m,n)
      | _ -> false
      end

    | Binaire(ob) -> cycle g ob.gauche && cycle g ob.droite
    | Reduction(ored) ->
    let (i_start, j_start) = ored.case_debut in
    let (i_end, j_end) = ored.case_fin in
    let res = ref false in
    for i = i_start to i_end do
      for j = j_start to j_end do
        res := !res && cycle g g.(i).(j)
      done;
    done;
    !res
    | _ -> false
  in
  match e with
  | Case(i,j) -> loop [(i,j)] (i,j)
  | Unaire(ou) -> cycle g ou.operande
  | Binaire(ob) -> cycle g ob.gauche && cycle g ob.droite
  | Reduction(ored) ->
    let (i_start, j_start) = ored.case_debut in
    let (i_end, j_end) = ored.case_fin in
    let res = ref false in
    for i = i_start to i_end do
      for j = j_start to j_end do
        res := !res && cycle g g.(i).(j)
      done;
    done;
    !res
  | _ -> false*)

let rec eval_expr (e : expr) (g : grille): resultat =
  match e with
  | Vide -> RVide
  | Chaine(s) -> RChaine(s)
  | Entier(x) -> REntier(x)
  | Flottant(f) -> RFlottant(f)
  | Case(i,j) ->
    begin
      if i >= Array.length g || i < 0 || j < 0 || j >= Array.length g.(0)
      then Erreur (Mauvais_indice(i,j))
      else
        begin
          try
            if cycle g e
            then Erreur(Cycle_detecte)
            else eval_expr (g.(i).(j)) g
          with
          | Invalid_argument _ -> Erreur(CaseRefError(i,j))
        end

    end
  | Unaire(ou) ->

    begin
    if cycle g ou.operande then Erreur(Cycle_detecte)
    else ou.app1 (eval_expr ou.operande g)
    end
  | Binaire(ob) -> ob.app2 (eval_expr ob.gauche g) (eval_expr ob.droite g)
  | Reduction(ored) ->
    let (i_start, j_start) = ored.case_debut in
    let (i_end, j_end) = ored.case_fin in
    let res = ref (eval_expr ored.init g) in
    for i = i_start to i_end do
      for j = j_start to j_end do
        res := ored.app !res (eval_expr g.(i).(j) g)
      done;
    done;
    !res

let eval_grille (g : grille) : resultat array array =
  Array.map (fun line -> Array.map (fun e -> eval_expr e g) line) g

let result_to_string (e : resultat) : string =
  match e with
  | RVide -> ""
  | RChaine(str) -> str
  | REntier(x) -> string_of_int x
  | RFlottant(f) -> string_of_float f
  | Erreur(Mauvais_indice(i,j)) -> "Ind#" ^ string_of_int  i ^ "," ^ string_of_int j
  | Erreur(Cycle_detecte) -> "Cycle#"
  | Erreur(Argument_non_valide) -> "Args#"
  | Erreur(CaseRefError(i,j)) -> "CRef#"^ string_of_int  i ^ "," ^ string_of_int j

let affiche_grille_resultat (rg : resultat array array) : unit =
Array.iter
  (fun line -> (Array.iter (fun e -> Format.printf "|%8s" (result_to_string e)) line); Format.print_string "|\n") rg

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

let addition res1 res2 =
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

let somme (i_start, j_start) (i_end, j_end) : expr =
  let app_red acc res_e : resultat =
    addition acc res_e
  in
  Reduction({app = app_red; init = Entier(0); case_debut = (i_start, j_start);
            case_fin = (i_end, j_end)})
