type 'a ref = {mutable contents : 'a}

let ref (e : 'a) : 'a ref =
  {contents = e}

let (:=) (old_ref : 'a ref) (value : 'a) =
  old_ref.contents <- value

let (!) (reference : 'a ref) : 'a =
  reference.contents

let array_fold_left (f : 'a -> 'b -> 'a) (x : 'a ) (a : 'b array) =
  let indice = ref 1 in
  let cur = ref a.(!indice) in
  let acc = ref a.(0) in
  while !indice < Array.length a do
    acc := f !acc !cur ;
    indice := !indice + 1;
    cur := a.(!indice)
  done;
  !acc
