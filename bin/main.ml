open GMain
open GPango
open Printf

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Initialize counters *)
let hint_counter = ref 0
let match_counter = ref 0
let max_hints = 7

(* Load words from a CSV file into a set *)
let load_words filename =
  let words = Hashtbl.create 100 in
  let channel = open_in filename in
  try
    while true do
      let word = input_line channel in
      Hashtbl.add words word true
    done;
    words
  with End_of_file ->
    close_in channel;
    words

(* Define types *)
type letter = char
type grid = letter array array

type game_state = {
  grid : grid;
  found_words : string list;
}

(* Create a new window *)
let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 ()

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

let target_words =
  [ "hayrides"; "pumpkins"; "maze"; "cider"; "doughnuts"; "apples"; "orchards" ]

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

(* Load accepted and target words from files *)
let accepted_words = load_words "data/filtered_accepted_words.csv"

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

(* Check word input, update counters, and display appropriate messages *)
let process_input state word =
  let word = String.lowercase_ascii word in
  (* Convert user input to lowercase *)
  if List.mem word target_words then (
    incr match_counter;
    printf "Match found! Total matches: %d\n" !match_counter;
    { state with found_words = word :: state.found_words })
  else if Hashtbl.mem accepted_words word then (
    incr hint_counter;
    printf "Hint incremented. Total hints: %d\n" !hint_counter;
    printf "Current hint count: %d/%d\n" !hint_counter max_hints;
    if !hint_counter >= max_hints then printf "Hint available!\n";
    state)
  else (
    printf "Invalid word: %s\n" word;
    state)

let rec game_loop state =
  Printf.printf "Guess a word: ";
  let guess = read_line () in

  (* Call process_input to handle the guess and update counters *)
  let new_state = process_input state guess in

  print_grid new_state.grid new_state.found_words;

  if List.length new_state.found_words = List.length target_words then (
    print_grid new_state.grid new_state.found_words;
    Printf.printf "YAY congrats! You found all the words (:\n")
  else game_loop new_state

(* Execute *)
let () =
  (* Set up exit function when the window is closed *)
  ignore (window#connect#destroy ~callback:Main.quit);
  (* Create vertical element box with 20 px of padding *)
  let vbox = GPack.vbox ~border_width:20 ~packing:window#add () in
  (* Create game title and subtitle with font 20 *)
  let title_label = GMisc.label ~text:"OCaml Strands" ~packing:vbox#pack () in
  title_label#misc#modify_font (GPango.font_description_from_string "Serif 20");
  ignore
    (GMisc.label ~text:"By: Falak, Amy, Angie, and Matthew" ~packing:vbox#pack
       ());

  (* Create a start button to execute program *)
  let start_button = GButton.button ~label:"Play" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (start_button#connect#clicked ~callback:(fun () ->
         print_endline "Theme: Fall Fun";
         let initial_state = { grid = initial_grid; found_words = [] } in
         print_grid initial_state.grid initial_state.found_words;
         game_loop initial_state));

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  Main.main ()
