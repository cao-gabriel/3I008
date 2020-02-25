open Graphics
type world = bool array array
let window_size = 900
let world_size = 30
let case_size = window_size / world_size
let init = Array.make_matrix world_size world_size false
let _ =
  Random.self_init () ;
  Array.iteri
    (fun ind_line line ->
       Array.iteri (fun ind_col case -> init.(ind_line).(ind_col)<- Random.bool ()) line)
    init





let neighbours (line : int) (column : int) (w : world) =
  let cpt = ref 0 in
  if (line + 1) < (Array.length w) && w.(line + 1).(column) then cpt := !cpt + 1;
  if (line - 1) >= 0 && w.(line - 1).(column) then cpt := !cpt + 1;
  if (column + 1) < (Array.length w.(0)) && w.(line).(column + 1) then cpt := !cpt + 1;
  if (column - 1) >= 0 && w.(line).(column - 1) then cpt := !cpt + 1;
  !cpt

let next_gen (w : world) : world =
  Array.mapi
    (fun ind_line line ->
       Array.mapi (fun ind_col case -> neighbours ind_line ind_col w = 3) line)
    w


let init_graph () =
    open_graph "" ; resize_window window_size window_size

let draw_case x y case =
  if case then set_color Graphics.blue
  else set_color Graphics.red ;
  fill_rect (y * case_size) ((world_size - x - 1) * case_size)  case_size case_size

let draw_gen (w : world) =
Array.iteri
  (fun ind_line line ->
     Array.iteri (fun ind_col case -> draw_case ind_line ind_col case) line)
  w

let continue () =
  let st = wait_next_event [Key_pressed] in
  st.key != 'q'

let main (w : world)=
  let rec loop w not_finish=
    if not_finish
    then
      begin
      clear_graph () ;
      let w1 = next_gen w in
      draw_gen w1 ; loop w1 (continue ())
    end
    else
      exit 1
  in
  init_graph () ;loop w true

let _ = main init
