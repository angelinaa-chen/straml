open Printf

type letter = char
type grid = letter array array

type game_state = {
  grid : grid;
  found_words : string list;
}

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

(* Load accepted and target words from files *)
let accepted_words = load_words "./data/filtered_accepted_words.csv"
let make_state grid words = { grid; found_words = words }

(*stylizes a given letter*)
let print_letter letter highlight =
  if highlight then Printf.printf "\027[1;32m%c\027[0m " letter
  else Printf.printf "%c " letter

let is_highlighted (r, c) found_words word_positions =
  List.exists
    (fun (word, positions) ->
      List.mem word found_words && List.mem (r, c) positions)
    word_positions

let print_grid (grid : letter array array) found_words word_positions =
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c letter ->
          print_letter letter (is_highlighted (r, c) found_words word_positions))
        row;
      print_newline ())
    grid

(** [handle_guess] updates the state if a target word is found *)
let handle_guess state guess target_words =
  let lower_guess = String.lowercase_ascii guess in
  if
    List.mem lower_guess target_words
    && not (List.mem lower_guess state.found_words)
  then { state with found_words = lower_guess :: state.found_words }
  else state

(** Check word input, update counters, and display appropriate messages *)
let process_input state word target_words match_counter hint_counter max_hints =
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
