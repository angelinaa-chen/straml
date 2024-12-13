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
    is the puzzle that the user chose to play. *)

val initialize_game : grid -> string -> game_state
(** [initialize_game grid theme] initializes a new game state with the given
    grid and theme. *)

val load_words : string -> string BatSet.t
(** [load_words path] loads words from the file at [path] into a BatSet. *)

val alr_guessed : string -> string BatSet.t -> string BatSet.t
(** [alr_guessed guess guessed_words] adds [guess] to [guessed_words] if it
    hasn't been guessed yet. *)

val print_letter_yellow : letter -> bool -> unit
(** [print_letter_yellow letter highlight] prints [letter] in yellow if
    [highlight] is true. *)

val print_letter_green : letter -> bool -> unit
(** [print_letter_green letter highlight] prints [letter] in green if
    [highlight] is true. *)

val print_letter_blue : letter -> bool -> unit
(** [print_letter_blue letter highlight] prints [letter] in blue if [highlight]
    is true. *)

val is_highlighted :
  int * int -> string list -> (string * (int * int) list) list -> bool
(** [is_highlighted (r, c) found_words word_positions] checks if the position
    [(r, c)] is highlighted based on [found_words] and [word_positions]. *)

val print_grid :
  grid -> string BatSet.t -> (string * (int * int) list) list -> unit
(** [print_grid grid found_words word_positions] prints the [grid] with
    appropriate highlights for [found_words] and [word_positions]. *)

val handle_guess : game_state -> string -> string list -> game_state
(** [handle_guess state guess target_words] updates the [state] if [guess] is a
    target word. *)

val word_to_highlight : game_state -> string list -> string option
(** [word_to_highlight state theme_target_words] finds the next word to
    highlight based on [state] and [theme_target_words]. *)

val hint_highlighter :
  string -> (string * (int * int) list) list -> grid -> unit
(** [hint_highlighter hint_word word_positions grid] highlights the positions of
    [hint_word] in [grid]. *)

val hint_revealer :
  game_state ->
  (string * (int * int) list) list ->
  string list ->
  string BatSet.t ->
  unit
(** [hint_revealer state word_positions target_words accepted_words] reveals a
    hint if conditions are met. *)

val is_word_in_grid : grid -> string -> bool
(** [is_word_in_grid grid word] checks if [word] exists in [grid]. *)

val is_spangram : string -> string -> bool
(** [is_spangram word spangram] checks if [word] is the spangram. *)

val process_input :
  game_state ->
  string ->
  string list ->
  int ref ->
  int ref ->
  int ->
  string BatSet.t ->
  (string * (int * int) list) list ->
  game_state
(** [process_input state word target_words match_counter hint_counter max_hints
    accepted_words word_positions]
    processes the input word and updates the game state accordingly. *)
