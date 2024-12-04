open GMain
open GPango
open Printf
open Cs3110_fin.Logic

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Initialize counters *)
let hint_counter = ref 0
let match_counter = ref 0
let max_hints = 7

(* Create a new window *)
let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 ()

(* Spangram: Orchards (?) *)
let initial_grid : grid =
  [|
    [| 'S'; 'N'; 'P'; 'M'; 'D'; 'S' |];
    [| 'I'; 'K'; 'U'; 'I'; 'R'; 'E' |];
    [| 'E'; 'A'; 'M'; 'P'; 'H'; 'Y' |];
    [| 'Z'; 'R'; 'H'; 'A'; 'D'; 'A' |];
    [| 'O'; 'U'; 'C'; 'G'; 'R'; 'S' |];
    [| 'T'; 'C'; 'N'; 'H'; 'U'; 'P' |];
    [| 'R'; 'S'; 'I'; 'O'; 'P'; 'L' |];
    [| 'E'; 'D'; 'D'; 'A'; 'E'; 'S' |];
  |]

(* Spangram: Medical career *)
let to_your_health : grid =
  [|
    [| 'S'; 'N'; 'U'; 'M'; 'A'; 'R' |];
    [| 'E'; 'R'; 'E'; 'D'; 'H'; 'M' |];
    [| 'D'; 'E'; 'I'; 'P'; 'A'; 'C' |];
    [| 'N'; 'C'; 'A'; 'L'; 'T'; 'I' |];
    [| 'T'; 'I'; 'S'; 'T'; 'C'; 'S' |];
    [| 'E'; 'O'; 'E'; 'R'; 'A'; 'R' |];
    [| 'G'; 'N'; 'E'; 'C'; 'T'; 'O' |];
    [| 'R'; 'U'; 'S'; 'R'; 'O'; 'D' |];
  |]

(* Spangram: Well-suited *)
let nice_fit : grid =
  [|
    [| 'P'; 'M'; 'L'; 'E'; 'R'; 'E' |];
    [| 'U'; 'T'; 'H'; 'Y'; 'I'; 'U' |];
    [| 'J'; 'I'; 'R'; 'D'; 'A'; 'S' |];
    [| 'B'; 'L'; 'L'; 'S'; 'U'; 'D' |];
    [| 'W'; 'E'; 'B'; 'I'; 'T'; 'E' |];
    [| 'S'; 'U'; 'S'; 'G'; 'N'; 'O' |];
    [| 'I'; 'E'; 'C'; 'S'; 'L'; 'R' |];
    [| 'N'; 'I'; 'V'; 'I'; 'S'; 'T' |];
  |]

(* Spangram: Social Media *)
let extremely_online : grid =
  [|
    [| 'T'; 'W'; 'S'; 'S'; 'H'; 'O' |];
    [| 'E'; 'O'; 'R'; 'A'; 'F'; 'L' |];
    [| 'E'; 'C'; 'E'; 'W'; 'O'; 'L' |];
    [| 'T'; 'I'; 'C'; 'M'; 'T'; 'N' |];
    [| 'A'; 'E'; 'O'; 'A'; 'M'; 'E' |];
    [| 'R'; 'L'; 'K'; 'L'; 'I'; 'P' |];
    [| 'E'; 'P'; 'M'; 'I'; 'D'; 'O' |];
    [| 'T'; 'R'; 'O'; 'E'; 'S'; 'T' |];
  |]

let target_words =
  [ "hayrides"; "pumpkins"; "maze"; "cider"; "doughnuts"; "apples"; "orchards" ]

let extremely_online_target =
  [
    "tweet";
    "report";
    "share";
    "follow";
    "comment";
    "post";
    "like";
    "report";
    "socialmedia";
  ]

let nice_fit_target =
  [ "jump"; "birthday"; "leisure"; "business"; "civil"; "strong"; "wellsuited" ]

let to_your_health_target =
  [ "nurse"; "dentist"; "medicalcareer"; "surgeon"; "pharmacist"; "doctor" ]

let word_positions =
  [
    ( "hayrides",
      [ (2, 4); (3, 5); (2, 5); (1, 4); (1, 3); (0, 4); (1, 5); (0, 5) ] );
    ( "pumpkins",
      [ (2, 3); (1, 2); (0, 3); (0, 2); (1, 1); (1, 0); (0, 1); (0, 0) ] );
    ("maze", [ (2, 2); (2, 1); (3, 0); (2, 0) ]);
    ("cider", [ (5, 1); (6, 2); (7, 1); (7, 0); (6, 0) ]);
    ( "doughnuts",
      [ (7, 2); (6, 3); (5, 4); (4, 3); (5, 3); (5, 2); (4, 1); (5, 0); (6, 1) ]
    );
    ("apples", [ (7, 3); (6, 4); (5, 5); (6, 5); (7, 4); (7, 5) ]);
    ( "orchards",
      [ (4, 0); (3, 1); (4, 2); (3, 2); (3, 3); (4, 4); (3, 4); (4, 5) ] );
  ]

(** TODO: FIX*)
let to_your_health_position =
  [
    ( "hayrides",
      [ (3, 4); (1, 5); (2, 5); (1, 4); (1, 3); (0, 4); (1, 5); (0, 5) ] );
    ( "pumpkins",
      [ (2, 3); (1, 2); (0, 3); (0, 2); (1, 1); (1, 0); (0, 1); (0, 0) ] );
    ("maze", [ (2, 2); (2, 1); (3, 0); (2, 0) ]);
    ("cider", [ (5, 1); (6, 2); (7, 1); (7, 0); (6, 0) ]);
    ( "doughnuts",
      [ (7, 2); (6, 3); (5, 4); (4, 3); (5, 3); (5, 2); (4, 1); (5, 0); (6, 1) ]
    );
    ("apples", [ (7, 3); (6, 4); (5, 5); (6, 5); (7, 4); (7, 5) ]);
    ( "orchards",
      [ (4, 0); (3, 1); (4, 2); (3, 2); (3, 3); (4, 4); (3, 4); (4, 5) ] );
  ]

type game_state = Cs3110_fin.Logic.game_state

(* Handle theme selection and starts the game in terminal *)
let select_theme () =
  print_endline "Choose a theme to play:";
  print_endline "1. Fall Fun";
  print_endline "2. Well-Suited";
  print_endline "3. To Your Health";
  print_endline "4. Extremely Online";
  let theme_choice = read_int () in
  match theme_choice with
  | 1 -> initial_grid, target_words, word_positions
  | 2 -> nice_fit, nice_fit_target, word_positions
  | 3 -> to_your_health, to_your_health_target, to_your_health_position
  | 4 -> extremely_online, extremely_online_target, word_positions
  | _ -> initial_grid, target_words, word_positions

let print_letter letter highlight =
  if highlight then Printf.printf "\027[1;32m%c\027[0m " letter
  else Printf.printf "%c " letter

let is_highlighted (r, c) found_words =
  List.exists
    (fun (word, positions) ->
      List.mem word found_words && List.mem (r, c) positions)
    word_positions

let print_grid (grid : letter array array) found_words =
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c letter ->
          print_letter letter (is_highlighted (r, c) found_words))
        row;
      print_newline ())
    grid

let handle_guess state guess =
  let lower_guess = String.lowercase_ascii guess in
  if
    List.mem lower_guess target_words
    && not (List.mem lower_guess state.found_words)
  then { state with found_words = lower_guess :: state.found_words }
  else state

let rec game_loop state match_counter hint_counter max_hints accepted_words =
  Printf.printf "Guess a word: ";
  let guess = read_line () in
  let new_state =
    Cs3110_fin.Logic.process_input state guess target_words match_counter
      hint_counter max_hints accepted_words
  in
  Cs3110_fin.Logic.print_grid new_state.grid new_state.found_words
    word_positions;
  if List.length new_state.found_words = List.length target_words then (
    Printf.printf "YAY congrats! You found all the words (:\n";
    exit 0)
  else game_loop new_state match_counter hint_counter max_hints accepted_words

let initialize_game grid words = { grid; found_words = [] }

let print_theme_info grid theme word_positions =
  print_endline ("Theme: " ^ theme);
  Cs3110_fin.Logic.print_grid grid [] word_positions  
  
let () =
  let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 () in

  ignore (window#connect#destroy ~callback:Main.quit);

  let vbox = GPack.vbox ~border_width:20 ~packing:window#add () in
  let title_label = GMisc.label ~text:"OCaml Strands" ~packing:vbox#pack () in
  title_label#misc#modify_font (GPango.font_description_from_string "Serif 20");

  let start_button = GButton.button ~label:"Play" ~packing:vbox#pack () in
  
  ignore
  (start_button#connect#clicked ~callback:(fun () ->
    let grid, target_words, word_positions = select_theme () in
    let theme =
      match grid with
      | g when g == initial_grid -> "Fall Fun"
      | g when g == nice_fit -> "Well-Suited"
      | g when g == to_your_health -> "To Your Health"
      | g when g == extremely_online -> "Extremely Online"
      | _ -> "Invalid theme :("
    in
      print_theme_info grid theme word_positions;
      
      let accepted_words =
        Cs3110_fin.Logic.load_words "data/filtered_accepted_words.csv" in
      
      let initial_state = initialize_game grid [] in
   
      game_loop initial_state match_counter hint_counter max_hints accepted_words));
      window#show ();
      Main.main ()
  