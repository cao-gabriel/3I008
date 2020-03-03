let to_list (str : string) : char list =
  let rec loop cpt acc =
    if cpt = String.length str then acc
    else loop (cpt + 1) ((String.get str cpt)::acc)
  in
  loop 0 []


type 'a tree = Empty | Tree of ('a * 'a tree * 'a tree)


module type OrderedType =
sig
  type t
  val compare : t -> t -> int
end

module type S =
sig
  type key
  type 'a t
  val create : unit -> 'a t
  val add : 'a t -> key -> 'a t -> 'a t
  val find : 'a t -> key -> 'a
end

(* module Make : functor (Ord: OrderedType) -> S with type key = Ord.t *)

module Make (Ord: OrderedType) = struct

  type key = Ord.t

  type 'a t = Empty | Tree of ('a * 'a t * 'a t)

  let create () = Empty

  let rec add m k v =
    match m with
    | Empty -> Tree ((k, v), Empty, Empty)
    | Tree ((k', v'), l, r) when Ord.compare k k' < 0 -> Tree ((k', v'), add l k v, r)
    | Tree ((k', v'), l, r) when Ord.compare k k' > 0 -> Tree ((k', v'), l, add r k v)
    | Tree _ -> m

  let rec find m k =
    match m with
    | Empty -> raise Not_found
    | Tree ((k', v), l, r) ->
      if Ord.compare k k' = 0 then v
      else if Ord.compare k k' < 0 then find l k
      else find r k

end

module MapString = Make (String)



type fmark = Bold | Italic | Underlined
type fchar = (char * fmark list)
type fline = fchar list

let set_mark = List.cons
let unset_mark a = List.filter ((<>) a)
let present_mark = List.mem

let change_mark a l =
  if present_mark a l then
    unset_mark a l
  else set_mark a l


module type INPUT = sig
  type param
  type t
  exception End
  val create : param -> t
  val line : t -> fline
  val close : t -> unit
end

module type OUTPUT = sig
  type param
  type t
  val create : param -> t
  val line : t -> fline -> unit
  val close : t -> unit
end

module type PROC = sig
  type input_param
  type output_param
  val process : input_param -> output_param -> unit
end

module Processor (Input : INPUT)
    (Output : OUTPUT) : (PROC with type input_param = Input.param
                               and type output_param = Output.param) =
struct
  type input_param = Input.param
  type output_param = Output.param
  let process input_param output_param =
    let input = Input.create input_param in
    let output = Output.create output_param in
    try
      while true do
        Output.line output (Input.line input)
      done
    with
    | Input.End ->
      Input.close input ;
      Output.close output
end

module VerbatimFileInput : INPUT with type param = string = struct
  type param = string
  type t = in_channel
  exception End

  let create = open_in
  let close = close_in

  let line c =
    match input_line c with
    | exception End_of_file -> raise End
    | s -> List.fold_left (fun acc c -> List.cons (c, []) acc) [] @@  to_list s
end

module VerbatimFileOutput : OUTPUT with type param = string = struct
  type param = string
  type t = out_channel

  let create = open_out
  let close = close_out

  let line chan line =
    List.iter (fun (c, _) -> output_char chan c) line;
    output_string chan "\n"

end

module VerbatimProcessor = Processor (VerbatimFileInput)(VerbatimFileOutput)

(*let () = VerbatimProcessor.process "tme5.ml" "td5.ml"*)


module MarkdownConsoleInput : INPUT with type param = unit =
struct
  type param = unit
  type t = unit
  exception End
  let create () = ()
  let line () =
    try
      let s = input_line stdin in
      let s_list = to_list s in
      let rec loop s_list marks acc =
        match s_list with
        | [] -> acc
        | x::xs ->
          begin
            match x with
            | '*' -> loop xs (change_mark Bold marks) acc
            | '/' -> loop xs (change_mark Italic marks) acc
            | '_' -> loop xs (change_mark Underlined marks) acc
            | c -> loop xs marks ((c, marks)::acc)
          end
      in
    loop s_list [] []
    with
    | End_of_file -> raise(End)
  let close () = close_in stdin
end

module HTMLConsoleOutput =
struct
  type param = unit
  type t = unit
  let create () = ()
  let line () fl =
    let rec aux fl =
      match fl with
      | [] -> output_char stdout '\n'
      | (c, marks)::xs ->
        begin
        List.iter
          (fun x ->
          begin
            match x with
            | Bold -> output_string stdout "<b>"
            | Underlined -> output_string stdout "<u>"
            | Italic -> output_string stdout "<i>"
          end) marks ;
        output_char stdout c ;
        List.iter
          (fun x ->
          begin
            match x with
            | Bold -> output_string stdout "<b>"
            | Underlined -> output_string stdout "<u>"
            | Italic -> output_string stdout "<i>"
          end) (List.rev marks) ; aux xs
        end
      in
      aux fl ; flush stdout
  let close () =
    close_out stdout
end


module MarkdownToHtmlProcessor = Processor (MarkdownConsoleInput) (HTMLConsoleOutput)
(*let _ = MarkdownToHtmlProcessor.process () ()*)

module type INPUT_SOURCE =
sig
  type param
  type t
  exception End
  val create : param -> t
  val read : t -> string
  val close : t -> unit
end

module type INPUT_ANALYZER =
sig
  val transform : string -> fline
end

module Input (Is : INPUT_SOURCE) (Ia : INPUT_ANALYZER)
  : INPUT with type param = Is.param
           and type t = Is.t
  =
struct
  type param = Is.param
  type t = Is.t
  exception End
  let create (p : param) : t = Is.create p
  let line (ch : t) : fline = Ia.transform (Is.read ch)
  let close (ch : t) : unit = Is.close ch
end

module File : INPUT_SOURCE with type param = string
                            and type t = in_channel =
struct
  exception End
  type param = string
  type t = in_channel
  let create (p : param) : t = open_in p
  let read (ch : t) : string =
    try
      input_line ch
    with
    | End_of_file -> raise (End)
  let close (ch : t) = close_in ch
end

module Console : INPUT_SOURCE with type param = unit
                            and type t = unit =
struct
  exception End
  type param = unit
  type t = unit
  let create (p : param) : t = ()
  let read (ch : t) : string =
    try
      input_line stdin
    with
    | End_of_file -> raise (End)
  let close (ch : t) = close_in stdin
end


module Verbatim : INPUT_ANALYZER =
struct
  let transform (s : string) : fline =
    List.fold_left (fun acc c -> List.cons (c, []) acc) [] @@  to_list s
end

module Markdown : INPUT_ANALYZER =
struct
  let transform (s: string) : fline =
    let s_list = to_list s in
    let rec loop s_list marks acc =
      match s_list with
      | [] -> acc
      | x::xs ->
        begin
          match x with
          | '*' -> loop xs (change_mark Bold marks) acc
          | '/' -> loop xs (change_mark Italic marks) acc
          | '_' -> loop xs (change_mark Underlined marks) acc
          | c -> loop xs marks ((c, marks)::acc)
        end
    in
    loop s_list [] []
end

module type OUTPUT_DEST =
sig
  type param
  type t
  val create : param -> t
  val write : t -> string -> unit
  val close : t -> unit
end

module type OUTPUT_RENDERER =
sig
  val render : fline -> string
end

module Output (Od : OUTPUT_DEST) (Or : OUTPUT_RENDERER)
  :OUTPUT with type param = Od.param
           and type t = Od.t=
struct
  type param = Od.param
  type t = Od.t
  let create (p : param) : t = Od.create p
  let line (ch : t) (fl : fline) : unit = Od.write ch (Or.render fl)
  let close (ch : t) : unit = Od.close ch
end

module File : OUTPUT_DEST =
struct
  type param = string
  type t = out_channel
  let create (p : param) : t = open_out p
  let write (ch : t) (s : string) : unit =
    output_string ch s
  let close (ch : t) : unit = close_out ch
end

module Console : OUTPUT_DEST =
struct
  type param = unit
  type t = unit
  let create (p : param) : t = ()
  let write (ch : t) (s : string) : unit =
    output_string stdout s
  let close (ch : t) : unit = close_out stdout
end


module Html : OUTPUT_RENDERER =
struct
  let render fl =
  let rec aux fl acc=
    match fl with
    | [] ->"\n"
    | (c, marks)::xs ->
      begin
      let s1 = List.fold_left
        (fun acc x ->
        begin
          match x with
          | Bold -> acc ^ "<b>"
          | Underlined -> acc ^  "<u>"
          | Italic -> acc ^  "<i>"
        end) "" marks in
      let s2 = s1^(Char.escaped c) in
      let s3 = s2 ^ (List.fold_left
        (fun acc x ->
        begin
          match x with
          | Bold -> acc ^ "<b>"
          | Underlined -> acc ^ "<u>"
          | Italic -> acc ^ "<i>"
        end) "" (List.rev marks)) in
        aux xs acc^s3
      end
    in
    aux fl ""
end

module Verbatim : OUTPUT_RENDERER =
struct
  let render fl =
    String.concat "" (List.map (fun (c, _) -> Char.escaped c) fl)
end

type ftree =
  | Sequence of ftree list
  | Format of fmark list * ftree
  | Text of string
