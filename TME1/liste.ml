
let rec for_all (f : 'a -> bool) (l : 'a list) : bool =
  match l with
  | [] -> true
  | x::xs -> (f x) && (for_all f xs)

let rec map2 (f : 'a -> 'b -> 'c) (l1 : 'a list) (l2 : 'b list) : 'c list =
  match (l1, l2) with
  | ([],[]) -> []
  | (x::xs, y::ys) -> (f x y)::(map2 f xs ys)
  | (x::xs, []) -> []
  | ([], y::ys) -> []

let rec combine (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  match (l1, l2) with
  | ([],[]) -> []
  | (x::xs, y::ys) -> (x, y)::(combine xs ys)
  | (x::xs, []) -> []
  | ([], y::ys) -> []
