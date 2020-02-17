type 'a queue = {debut : 'a list; fin : 'a list}

exception Empty_queue
let create () : 'a queue =
  {debut = []; fin = []}

let push (e : 'a) (q : 'a queue) : 'a queue =
  {q with fin = e::q.fin}

let pop (q : 'q queue) : 'a * 'a queue =
  match q.debut with
  | [] -> raise Empty_queue
  | x::xs -> (x, {q with debut = xs})

let to_list (file : 'a queue) : 'a list =
  let rec loop (file : 'a queue) (acc : 'a list) =
  try
    let (e, q) = pop file in
    loop q (e::acc)
  with
  | Empty_queue -> (List.rev file.fin)@acc
  in
  loop file []
