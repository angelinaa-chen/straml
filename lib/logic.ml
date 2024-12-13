open Printf
open GMain
open GPango

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

(*stylizes a given letter*)
(* GUI Function *)

(**[get_letter] returns a letter as plaint text or highlighted if [highlight] is
   true according to the [highlight mode] selection where 1 - green, 2 - blue,
   and 3 - yellow*)
let get_letter letter highlight highlight_mode =
  if highlight = true then
    if highlight_mode = 1 then
      Printf.sprintf "<span foreground=\"green\">%c </span>" letter
    else if highlight_mode = 2 then
      Printf.sprintf "<span foreground=\"blue\">%c </span>" letter
    else if highlight_mode = 3 then
      Printf.sprintf "<span foreground=\"yellow\">%c </span>" letter
    else Printf.sprintf "%c " letter
  else Printf.sprintf "%c " letter

(* Terminal color functions *)
let print_letter_yellow letter highlight =
  if highlight then Printf.printf "\027[1;33m%c\027[0m " letter
  else Printf.printf "%c " letter

let print_letter_green letter highlight =
  if highlight then Printf.printf "\027[1;32m%c\027[0m " letter
  else Printf.printf "%c " letter

let print_letter_blue letter highlight =
  if highlight then Printf.printf "\027[1;34m%c\027[0m " letter
  else Printf.printf "%c " letter

(** [is_highlighted] iterates over each position in a given grid, returns
    whether or not that position needs to be highlighted based on if it's apart
    of a found word*)
let is_highlighted (r, c) found_words word_positions =
  List.exists
    (fun (word, positions) ->
      List.mem word found_words && List.mem (r, c) positions)
    word_positions

(**[find_index] is the index of each unique [instance] of [x] in [arr]*)
let find_index arr x instance =
  let len = Array.length arr in
  let rec helper_find i count =
    if i >= len then None (* Reached the end of the array *)
    else if arr.(i) = x then
      if count = instance then Some i else helper_find (i + 1) (count + 1)
    else helper_find (i + 1) count
  in
  helper_find 0 0

let show_grid (grid : letter array array) found_words word_positions
    (grid_box : GPack.box) highlight_mode =
  (* Convert the set of found words into a list *)
  let found_list = BatSet.to_list found_words in

  (* Create the grid string by iterating over each row and column *)
  let grid_string =
    Array.fold_left
      (fun (acc : string) (row : letter array) ->
        (* Find the index of the row *)
        let r =
          match find_index grid row 0 with
          | None -> failwith "Row not found in grid"
          | Some index -> index
        in
        (* Initialize the array to keep track of letter instances in the row *)
        let instance_counts = Array.make 26 0 in
        (* Construct the row's string with highlighted letters *)
        let new_row =
          Array.fold_left
            (fun (acc2 : string) (l : letter) ->
              (* Map 'A' - 'Z' to ASCI values 65 - 90 *)
              let char_code = Char.code l - 65 in
              (* Track occurrences of the same letter in the row *)
              let instance = instance_counts.(char_code) in
              instance_counts.(char_code) <- instance + 1;
              (* Find the index of the letter within the row *)
              let c =
                match find_index row l instance with
                | None -> failwith "Letter not found in row"
                | Some index -> index
              in
              (*Highlight character yellow if it belongs to a spangram*)
              if List.mem (r, c) (snd (List.hd word_positions)) then
                acc2
                ^ get_letter l
                    (is_highlighted (r, c) found_list word_positions)
                    3
              else
                (* Append the letter with the appropriate highlight *)
                acc2
                ^ get_letter l
                    (is_highlighted (r, c) found_list word_positions)
                    highlight_mode)
            "" row
        in
        (* Add the new row to the accumulated grid string *)
        acc ^ new_row ^ "\n")
      "" grid
  in

  (* Get the list of all children in the grid box container *)
  let children = grid_box#children in
  (* Remove all existing widgets from the grid box *)
  List.iter (fun widget -> grid_box#remove widget#coerce) children;

  (* Create a new label for the grid with the markup string *)
  let new_grid_label = GMisc.label ~markup:grid_string () in

  (* Set the font for the label to a monospace font for uniform letter width *)
  new_grid_label#misc#modify_font
    (GPango.font_description_from_string "Monospace 30") [@coverage off];

  (* Add the new label to the grid box container *)
  grid_box#add new_grid_label#coerce [@coverage off]

(* Add the new label to the container grid_box#add !grid_label#coerce *)

(** [handle_guess] updates the state if a target word is found *)
let handle_guess state guess target_words =
  let lowercase_guess = String.lowercase_ascii guess in
  if
    List.mem lowercase_guess target_words
    && not (BatSet.mem lowercase_guess state.found_words)
  then { state with found_words = BatSet.add lowercase_guess state.found_words }
  else state

(** finds the next word in [theme_target_words] that has not already been found,
    to be displayed as a hint*)
let word_to_highlight state theme_target_words =
  List.find_opt
    (fun word -> not (BatSet.mem word state.found_words))
    theme_target_words

let hint_highlighter hint_word word_positions grid found_words grid_box
    highlight_mode =
  match List.assoc_opt hint_word word_positions with
  | Some positions ->
      let temp_hint_found_words = BatSet.add hint_word found_words in
      (show_grid grid temp_hint_found_words word_positions grid_box
         highlight_mode [@coverage off])
  | None ->
      Printf.printf "WARNING: Hint word not found in word positions.\n"
      [@coverage off]

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

let hint_revealer state word_positions target_words accepted_words grid_box
    highlight_mode message_label =
  match message_label with
  | Some message_text -> begin
      match word_to_highlight state target_words with
      | None ->
          message_text#set_text
            "Hints not available. All words have been found!\n"
      | Some hint_word ->
          let valid_guesses_count =
            BatSet.filter
              (fun word ->
                BatSet.mem word accepted_words
                && is_word_in_grid state.grid word)
              state.guessed_words
            |> BatSet.to_list |> List.length
          in
          if valid_guesses_count < 3 then
            message_text#set_text
              (Printf.sprintf
                 "Sorry, hint not unlocked yet. Words left to unlock hint: %d\n"
                 (3 - valid_guesses_count))
          else (
            message_text#set_text "";
            hint_highlighter hint_word word_positions state.grid
              state.found_words grid_box highlight_mode)
    end
  | None -> begin
      match word_to_highlight state target_words with
      | None -> ()
      | Some hint_word ->
          let valid_guesses_count =
            BatSet.filter
              (fun word ->
                BatSet.mem word accepted_words
                && is_word_in_grid state.grid word)
              state.guessed_words
            |> BatSet.to_list |> List.length
          in
          if valid_guesses_count < 3 then ()
          else
            hint_highlighter hint_word word_positions state.grid
              state.found_words grid_box highlight_mode
    end

let process_input state word target_words match_counter guess_counter max_hints
    accepted_words word_positions message_label =
  match message_label with
  | Some message_text -> begin
      let guessed_words_updated = BatSet.add word state.guessed_words in
      if BatSet.mem word state.guessed_words then (
        message_text#set_text "You've already guessed that word";
        state)
      else if List.mem word target_words then (
        incr match_counter;
        if List.hd target_words = word then (
          message_text#set_text "You found the Spangram!";
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          })
        else (
          message_text#set_text "Match found!";
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          }))
      else if is_word_in_grid state.grid word && BatSet.mem word accepted_words
      then (
        incr guess_counter;
        { state with guessed_words = guessed_words_updated })
      else (
        message_text#set_text "Invalid word";
        { state with guessed_words = guessed_words_updated })
    end
  | None -> begin
      let guessed_words_updated = BatSet.add word state.guessed_words in
      if List.mem word target_words then (
        incr match_counter;
        if List.hd target_words = word then
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          }
        else
          {
            state with
            found_words = BatSet.add word state.found_words;
            guessed_words = guessed_words_updated;
          })
      else if is_word_in_grid state.grid word && BatSet.mem word accepted_words
      then (
        incr guess_counter;
        { state with guessed_words = guessed_words_updated })
      else { state with guessed_words = guessed_words_updated }
    end
