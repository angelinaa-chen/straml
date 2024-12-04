open GMain
open GPango
open Printf
open Cs3110_fin.Logic
open Cs3110_fin.Grid_data

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Initialize counters *)
let hint_counter = ref 0
let match_counter = ref 0
let max_hints = 7

(* Create a new window *)
let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 ()

type game_state = Cs3110_fin.Logic.game_state
let initialize_game grid words = { grid; found_words = [] }

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

(* Handle all the theme selection *)
let select_theme () =
  print_endline "Choose a theme to play:";
  print_endline "1. Fall Fun";
  print_endline "2. Well-Suited";
  print_endline "3. To Your Health";
  print_endline "4. Extremely Online";
  let theme_choice = read_int () in
  match theme_choice with
  | 1 -> GridData.initial_grid, GridData.target_words, GridData.word_positions
  | 2 -> GridData.nice_fit, GridData.nice_fit_target, GridData.word_positions
  | 3 -> GridData.to_your_health, GridData.to_your_health_target, GridData.word_positions
  | 4 -> GridData.extremely_online, GridData.extremely_online_target, GridData.word_positions
  | _ -> GridData.initial_grid, GridData.target_words, GridData.word_positions

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
  