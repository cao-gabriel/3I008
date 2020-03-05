let rec fact n =
  let rec loop n acc =
    if n = 0 then acc
    else loop (n -1) (acc * n)
  in
  loop n 1

let init_test () =
  let tb = Hashtbl.create 10 in
  for i = 0 to 4 do
    Hashtbl.add tb i (fact i)
  done;
  tb
(*Type : unit -> (int, int) Hashtbl.t*)

let cree_fact_memo =
  let tb = Hashtbl.create 10 in
  let rec fact_memo n =
    try
      (*Cherchons dans la table*)
      if n = 0 then 1
      else
        Hashtbl.find tb n
    with
      (*Si on ne le trouve pas dans la table*)
    | Not_found ->
      let value = n * fact_memo (n - 1) in
      Hashtbl.add tb n value ;
      value
  in
  fact_memo

let memoise f =
  let tb = Hashtbl.create 10 in
  let f_memo arg0 =
    try
      (*Si on trouve la valeur c'est cool*)
      Hashtbl.find tb arg0
    with
      (*On se retape tout le calcul*)
    | Not_found -> f arg0
  in
  f_memo
