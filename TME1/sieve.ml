(*Crible d'Erasthostène*)

let rec print_int_list (l : int list) : unit =
  match l with
  | [] -> ()
  | x::xs -> print_int x ; print_string "\n"; print_int_list xs

let rec interval (n : int) (m : int) : int list =
  if (n = m) then [m]
  else if ( n > m) then []
  else n::(interval (n + 1) m)

let rec filter_out (p : 'a -> bool) (l : 'a list) : 'a list =
  match l with
  | [] -> []
  | x::xs ->
    if (p x) then filter_out p xs
    else x::(filter_out p xs)

let is_multiple (m :int) (x : int) : bool =
  m mod x = 0

let remove_multiple_of (n : int) (l : int list) : int list =
  filter_out (fun x -> is_multiple x n) l

let sieve (max : int) : int list =
  let var = interval 2 max in
  let rec loop (l : int list ) : int list =
    match l with
    | [] -> []
    | x::xs ->
      if x * x > max then l
      else x::(loop (remove_multiple_of x xs))
  in
  loop var

let _ = print_int_list (sieve (int_of_string (Array.get Sys.argv 1)))
