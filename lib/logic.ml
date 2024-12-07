open Printf

type letter = char
type grid = letter array array

type game_state = {
  grid : grid;
  found_words : string BatSet.t;
  guessed_words : string BatSet.t;
  theme : string;
}

let initialize_game grid theme =
  { grid; found_words = BatSet.empty; guessed_words = BatSet.empty; theme }

(* Load words from a file into a BatSet *)
let load_words path =
  try
    BatFile.lines_of path
    |> BatEnum.fold
         (fun acc word ->
           BatSet.add (String.trim (String.lowercase_ascii word)) acc)
         BatSet.empty
  with Sys_error msg -> failwith ("Failed to load words from file: " ^ msg)

let alr_guessed guess guessed_words =
  if BatSet.mem guess guessed_words then guessed_words
  else BatSet.add guess guessed_words
(* Add the word to the set if it hasn't been guessed yet *)

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
  let found_list = BatSet.to_list found_words in
  Array.iteri
    (fun r row ->
      Array.iteri
        (fun c letter ->
          print_letter letter (is_highlighted (r, c) found_list word_positions))
        row;
      print_newline ())
    grid

(** [handle_guess] updates the state if a target word is found *)
let handle_guess state guess target_words =
  let lowercase_guess = String.lowercase_ascii guess in
  if
    List.mem lowercase_guess target_words
    && not (BatSet.mem lowercase_guess state.found_words)
  then { state with found_words = BatSet.add lowercase_guess state.found_words }
  else state

(** Check word input, update counters, and display appropriate messages *)

let process_input state word target_words match_counter hint_counter max_hints
    accepted_words =
  let guessed_words_updated = BatSet.add word state.guessed_words in
  if BatSet.diff guessed_words_updated state.guessed_words = BatSet.empty then (
    Printf.printf "You've already guessed that word!\n";
    state)
  else if List.mem word target_words then (
    incr match_counter;
    Printf.printf "Match found! Total matches: %d\n" !match_counter;
    {
      state with
      found_words = BatSet.add word state.found_words;
      guessed_words = guessed_words_updated;
    })
  else if BatSet.mem word accepted_words then (
    (* Check with BatSet *)
    incr hint_counter;
    Printf.printf "Hint incremented. Total hints: %d\n" !hint_counter;
    Printf.printf "Current hint count: %d/%d\n" !hint_counter max_hints;
    if !hint_counter >= max_hints then Printf.printf "Hint available!\n";
    { state with guessed_words = guessed_words_updated })
  else (
    Printf.printf "Invalid word: %s\n" word;
    { state with guessed_words = guessed_words_updated })
