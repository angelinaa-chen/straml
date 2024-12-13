type letter = char
(** type of the*)

type grid = letter array array
(** type of the grid puzzle *)

type game_state = {
  grid : grid;
  found_words : string BatSet.t;
  guessed_words : string BatSet.t;
  theme : string;
}
(** type of a game_state, tracks [found_words]: the BatSet of target words that
    have been found, [guessed_words]: the BatSet of all words that have been
    guessed, [theme]: is the puzzle that the user chose to play*)

val initialize_game : grid -> string -> game_state
(** initializes an empty game_state with a given [grid] and [theme], and sets
    [found_words] and [guessed_words] as empty BatSets*)

val alr_guessed : 'a -> 'a BatSet.t -> 'a BatSet.t
(** Checks if a value exists within a list, adds it to the list if not*)

val is_highlighted : 'a * 'b -> 'c list -> ('c * ('a * 'b) list) list -> bool
(*iterates over each position in a given grid, returns whether or not that
  position needs to be highlighted based on if it's apart of a found word*)

val show_grid :
  grid ->
  'a BatSet.t ->
  ('a * (int * int) list) list ->
  GPack.box ->
  int ->
  unit
(**[show_grid] displays [grid] in [grid_box], higlighting each grid position in
   [word_positions] that spell a word in [found_words], it highlights each
   character according to the [highlight_mode] selection*)

val load_words : string -> string BatSet.t
(** Load words from a file into a BatSet, used for the accepted_words.csv to
    determine what's a valid word that counts towards hints *)

val word_to_highlight : game_state -> string list -> string option
(** finds the next word in [theme_target_words] that has not already been found,
    to be displayed as a hint*)

(* if there's a hint available, calls hint_highlighter to highlights it in the
   grid *)
val hint_revealer :
  game_state ->
  (string * (int * int) list) list ->
  string list ->
  string BatSet.t ->
  GPack.box ->
  int ->
  GMisc.label option ->
  unit

val process_input :
  game_state ->
  string ->
  string list ->
  int ref ->
  int ref ->
  'a ->
  string BatSet.t ->
  'b ->
  GMisc.label option ->
  game_state
(** handles one turn of the game given the current [state] and a guess [word].
    performs appropriate processing and returns an updated game_state*)

val handle_guess : game_state -> string -> string list -> game_state
(** updates the state if a target word is found *)

val is_word_in_grid : letter array array -> string -> bool
(** returns whether or not the letters in this word are adjacent in the grid
    (can be reached in one line)*)

val get_letter : letter -> bool -> int -> string
(**[get_letter] returns the given letter formatted with the correct color*)

val print_letter_yellow : letter -> bool -> unit
(** prints a given letter in yellow if it satisfies the given predicate*)

val print_letter_blue : letter -> bool -> unit
(** prints a given letter in blue if it satisfies the given predicate*)

val print_letter_green : letter -> bool -> unit
(** prints a given letter in green if it satisfies the given predicate*)

val find_index : 'a array -> 'a -> int -> int option
(**[find_index] is the index of each unique [instance] of [x] in [arr]*)

val is_spangram : 'a -> 'a -> bool
(**returns whether or not the a word is the spangram*)
