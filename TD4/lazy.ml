type 'a sequence =
  | Nil (*Fin de l'itérateur*)
  | Cons of 'a * 'a next
  (* Value rcourante et générateur de la prochaine valeur*)

and 'a next = unit -> 'a sequence

let rec sequence_of_list (l : 'a list) : 'a sequence =
  match l with
  | [] -> Nil
  | x::xs -> Cons(x, (fun () -> sequence_of_list xs))
