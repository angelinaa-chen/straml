open Csv

(**Loads a puzzle grid from the given [filename] csv*)
let load_grid filename : char array array =
  let rows = Csv.load filename in
  (* Convert each row (a list of strings) to an array of characters *)
  Array.of_list
    (List.map
       (fun row ->
         Array.of_list (List.map (fun cell -> cell.[0]) row)
         (* convert each string to its first character *))
       rows)

(** loads a list of target words from a csv [filename]*)
let load_target_words filename =
  try
    let rows = Csv.load filename in
    (* Since we're only interested in the first row, we extract it *)
    match rows with
    | [] -> [] (* If the file is empty, return an empty list *)
    | h :: t ->
        (*there should only be one row *)
        (* Convert the row (list of strings) to a trimmed list of strings *)
        List.map String.trim h
  with _ -> failwith ("an error occured while loading " ^ filename)

(** loads an (int * int) list list of positions from a csv [filename], order
    matters and corresponds to the order of target words*)

let load_positions filename =
  (* Parse a single position string of format (x, y) *)
  let parse_position pos =
    try
      let pos = String.trim pos in
      if
        String.length pos < 5
        || pos.[0] <> '('
        || pos.[String.length pos - 1] <> ')'
      then failwith ("Invalid position format in: " ^ pos);
      let pos = String.sub pos 1 (String.length pos - 2) in
      let nums = String.split_on_char ',' pos in
      match nums with
      | [ r; c ] ->
          let int_r = int_of_string (String.trim r) in
          let int_c = int_of_string (String.trim c) in
          (int_r, int_c)
      | _ -> failwith ("Invalid position format in: " ^ pos)
    with _ -> failwith ("Invalid position format in: " ^ pos)
  in

  (* Parse a row of positions *)
  let parse_row row = List.map parse_position row in

  (* Read CSV file using csv library *)
  try
    let rows = Csv.load filename in

    let () = print_endline "parse" in
    List.map parse_row rows
  with _ -> failwith ("an error occured while loading " ^ filename)

(** constructs a (string * (int * int) list list) data type combining target
    words and their positions. This is the required input type for word
    positions in the functions that use it*)
let construct_word_positions target_words positions_list =
  List.map2
    (fun words positions -> (words, positions))
    target_words positions_list
