open Printf

type letter = char
type grid = letter array array

type game_state = {
  grid : grid;
  found_words : string BatSet.t;
  guessed_words : string BatSet.t;
  theme : string;
}
(** [found_words] is the BatSet of target words that have been found.
    [guessed_words] is the BatSet of all words that have been guessed. [theme]
    is the puzzle that the user chose to play*)

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
let print_letter_yellow letter highlight =
  if highlight then Printf.printf "\027[1;33m%c\027[0m " letter
  else Printf.printf "%c " letter

let print_letter_green letter highlight =
  if highlight then Printf.printf "\027[1;32m%c\027[0m " letter
  else Printf.printf "%c " letter

let print_letter_blue letter highlight =
  if highlight then Printf.printf "\027[1;34m%c\027[0m " letter
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
          if List.mem (r, c) (snd (List.hd word_positions)) then
            print_letter_yellow letter
              (is_highlighted (r, c) found_list word_positions)
          else
            print_letter_blue letter
              (is_highlighted (r, c) found_list word_positions))
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

let word_to_highlight state theme_target_words =
  List.find_opt
    (fun word -> not (BatSet.mem word state.found_words))
    theme_target_words

let hint_highlighter hint_word word_positions grid =
  match List.assoc_opt hint_word word_positions with
  | Some positions ->
      Array.iteri
        (fun r row ->
          Array.iteri
            (fun c letter ->
              let highlight = List.mem (r, c) positions in
              print_letter_green letter highlight)
            row;
          print_newline ())
        grid
  | None -> Printf.printf "Hint word not found in word positions.\n"

let hint_revealer state word_positions target_words accepted_words =
  match word_to_highlight state target_words with
  | None -> Printf.printf "Hints not available. All words have been found!\n"
  | Some hint_word ->
      let valid_guesses_count =
        BatSet.filter
          (fun word -> BatSet.mem word accepted_words)
          state.guessed_words
        |> BatSet.to_list |> List.length
      in
      if valid_guesses_count < 3 then
        Printf.printf
          "Sorry, hint not unlocked yet. Words left to unlock hint: %d\n"
          (3 - valid_guesses_count)
      else (
        Printf.printf "Hint word: %s\n" hint_word;
        hint_highlighter hint_word word_positions state.grid)

let is_word_in_grid grid word =
  let word = String.uppercase_ascii word in
  let rows = Array.length grid in
  let cols = Array.length grid.(0) in
  let word_len = String.length word in

  (* Check if the given position is within grid boundaries *)
  let in_bounds r c = r >= 0 && r < rows && c >= 0 && c < cols in

  (* Moving up, down, left, right, and diagonals *)
  let directions =
    [ (-1, 0); (1, 0); (0, -1); (0, 1); (-1, -1); (-1, 1); (1, -1); (1, 1) ]
  in

  let rec dfs r c index visited =
    if index = word_len then true
    else if
      (not (in_bounds r c)) || visited.(r).(c) || grid.(r).(c) <> word.[index]
    then false
    else (
      visited.(r).(c) <- true;
      let result =
        List.exists
          (fun (dr, dc) -> dfs (r + dr) (c + dc) (index + 1) visited)
          directions
      in
      visited.(r).(c) <- false;
      result)
  in

  let visited = Array.make_matrix rows cols false in
  let rec search r c =
    if r = rows then false
    else if c = cols then search (r + 1) 0
    else if grid.(r).(c) = word.[0] && dfs r c 0 visited then true
    else search r (c + 1)
  in
  search 0 0

let is_spangram word spangram = if word = spangram then true else false

let process_input state word target_words match_counter hint_counter max_hints
    accepted_words word_positions =
  match String.lowercase_ascii word with
  | "hint" ->
      Printf.printf "Processing hint request...\n";
      hint_revealer state word_positions target_words accepted_words;
      state (* Return the same state after processing a hint *)
  | _ ->
      let guessed_words_updated = BatSet.add word state.guessed_words in
      if BatSet.mem word state.guessed_words then (
        Printf.printf "You've already guessed that word!\n";
        state)
      else if List.mem word target_words then (
        incr match_counter;
        if List.hd target_words = word then (
          Printf.printf "You found the Spangram! Total matches: %d\n"
            !match_counter;
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          })
        else (
          Printf.printf "Match found! Total matches: %d\n" !match_counter;
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          }))
      else if is_word_in_grid state.grid word && BatSet.mem word accepted_words
      then (
        incr hint_counter;
        Printf.printf
          "Word count towards hint incremented. Total words guessed: %d\n"
          !hint_counter;
        { state with guessed_words = guessed_words_updated })
      else (
        Printf.printf "Invalid word: %s\n" word;
        { state with guessed_words = guessed_words_updated })
