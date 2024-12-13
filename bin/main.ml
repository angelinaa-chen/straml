open GMain
open GPango
open Gtk
open Printf
open Cs3110_fin.Logic
open Cs3110_fin.Terminal_logic
open Unix
open Gdk
open Cs3110_fin.Filter_csv

let hint_counter = ref 0
let match_counter = ref 0
let max_hints = 7
let guessed_words = BatSet.empty

(* CLI Functions *)
let terminal_stats_summary state match_counter hint_counter start_time =
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "\n--- Game Summary ---\n";
  Printf.printf "Words Found: %d\n" (BatSet.cardinal state.found_words);
  Printf.printf "Total Guesses: %d\n" (BatSet.cardinal state.guessed_words);
  Printf.printf "Hints Used: %d\n" !hint_counter;
  Printf.printf "Time Spent: %.2f seconds\n" elapsed_time;
  Printf.printf "Thanks for playing! :) \n"

let rec terminal_game_loop state match_counter hint_counter max_hints
    accepted_words target_words word_positions start_time =
  Printf.printf "\n--- Current Grid ---\n";
  print_grid state.grid state.found_words word_positions;
  Printf.printf "Guess a word (or type 'q' to quit, 'hint' for a hint): ";
  let guess = String.trim (read_line ()) in
  match String.lowercase_ascii guess with
  | "q" ->
      terminal_stats_summary state match_counter hint_counter start_time;
      exit 0
  | "hint" ->
      Printf.printf "Processing hint request...\n";
      hint_revealer state word_positions target_words accepted_words;
      terminal_game_loop state match_counter hint_counter max_hints
        accepted_words target_words word_positions start_time
  | _ ->
      let new_state =
        process_input state guess target_words match_counter hint_counter
          max_hints accepted_words word_positions
      in
      if BatSet.cardinal new_state.found_words = List.length target_words then (
        Printf.printf "\nCongrats! You found all the words! :)\n";
        terminal_stats_summary new_state match_counter hint_counter start_time;
        exit 0)
      else
        terminal_game_loop new_state match_counter hint_counter max_hints
          accepted_words target_words word_positions start_time

let select_theme () =
  print_endline
    "Choose a theme to play by typing the associated number with it below. To \
     exit out of the game at any time, simply type 'q'.";
  print_endline "1. Fall Fun";
  print_endline "2. Well-Suited";
  print_endline "3. To Your Health";
  print_endline "4. Extremely Online";
  print_endline "5. Beatlemania!";
  try
    let theme_choice = read_int () in
    match theme_choice with
    | 1 -> ("data/fall_fun", "Fall Fun")
    | 2 -> ("data/nice_fit", "Well-Suited")
    | 3 -> ("data/health", "To Your Health")
    | 4 -> ("data/online", "Extremely Online")
    | 5 -> ("data/beatle", "Beatlemania!")
    | _ ->
        Printf.printf "Invalid choice. Defaulting to Fall Fun.\n";
        ("data/fall_fun", "Fall Fun")
  with Failure _ ->
    Printf.printf "Invalid input. Defaulting to Fall Fun.\n";
    ("data/fall_fun", "Fall Fun")

(*load accepted words*)
let () = Cs3110_fin.Filter_csv.process_csv

(**Loads a puzzle grid from the given [filename] csv*)
let load_grid filename : char array array =
  let lines = ref [] in
  let ic = open_in filename in
  try
    while true do
      let line = input_line ic in
      let chars =
        Array.of_list
          (List.map (fun s -> s.[0]) (String.split_on_char ',' line))
      in
      lines := chars :: !lines
    done;
    [||] (* unreachable, required to satisfy type *)
  with End_of_file ->
    close_in ic;
    (* reverse (for order) and convert to array *)
    Array.of_list (List.rev !lines)

(** loads a list of target words from a csv [filename]*)
let load_target_words filename =
  let ic = open_in filename in
  try
    let line = input_line ic in
    let words = String.split_on_char ',' line in
    let trimmed_words = List.map String.trim words in
    close_in ic;
    trimmed_words
  with
  | End_of_file ->
      close_in ic;
      []
  | e ->
      close_in ic;
      raise e

(** loads an (int * int) list list of positions from a csv [filename], order
    matters and corresponds to the order of target words*)
let load_positions filename : (int * int) list list =
  (* parse a single position string of format (x, y) *)
  let parse_position pos =
    try
      let pos = String.trim pos in
      if
        String.length pos < 5
        || pos.[0] <> '('
        || pos.[String.length pos - 1] <> ')'
      then raise (Failure ("Invalid position format in: " ^ pos));
      let pos = String.sub pos 1 (String.length pos - 2) in
      let nums = String.split_on_char ',' pos in
      match nums with
      | [ r; c ] ->
          let int_r : int = int_of_string (String.trim r) in
          let int_c = int_of_string (String.trim c) in
          (int_r, int_c)
      | _ -> raise (Failure ("Invalid position format in: " ^ pos))
    with _ -> raise (Failure ("Invalid position format in: " ^ pos))
  in

  (* parse a line of positions *)
  let parse_line line =
    let positions = String.split_on_char ';' line in
    List.map parse_position positions
  in

  (* open file, read all lines *)
  let ic = open_in filename in
  let lines = ref [] in
  try
    while true do
      let line = input_line ic in
      lines := line :: !lines
    done;
    []
  with End_of_file ->
    close_in ic;
    List.rev_map parse_line !lines

(** constructs a (string * (int * int) list list) data type combining target
    words and their positions. This is the required input type for word
    positions in the functions that use it*)
let construct_word_positions target_words positions_list =
  List.map2
    (fun words positions -> (words, positions))
    target_words positions_list

(* print functions for debugging*)
(* let print_pair_list lst = List.iter (fun (a, b) -> Printf.printf "%s: [" a;
   List.iter (fun (x, y) -> Printf.printf "(%d, %d) " x y) b; Printf.printf
   "]\n") lst

   let print_char_grid (grid : char array array) : unit = Array.iter (fun row ->
   Array.iter (fun c -> Printf.printf "%c " c) row; print_newline ()) grid

   let print_list list = List.iter (fun elt -> print_endline elt) list *)

(*fall fun*)
let fall_fun_grid = load_grid "data/fall_fun/grid.csv"
let fall_fun_target = load_target_words "data/fall_fun/target.csv"

let fall_fun_positions =
  construct_word_positions fall_fun_target
    (load_positions "data/fall_fun/positions.csv")

(*nice fit*)
let nice_fit_grid = load_grid "data/nice_fit/grid.csv"
let nice_fit_target = load_target_words "data/nice_fit/target.csv"

let nice_fit_positions =
  construct_word_positions nice_fit_target
    (load_positions "data/nice_fit/positions.csv")

(*to your health*)
let health_grid = load_grid "data/health/grid.csv"
let health_target = load_target_words "data/health/target.csv"

let health_positions =
  construct_word_positions health_target
    (load_positions "data/health/positions.csv")

(*online*)
let online_grid = load_grid "data/online/grid.csv"
let online_target = load_target_words "data/online/target.csv"

let online_positions =
  construct_word_positions online_target
    (load_positions "data/online/positions.csv")

(*beatle*)
let beatle_grid = load_grid "data/beatle/grid.csv"
let beatle_target = load_target_words "data/beatle/target.csv"

let beatle_positions =
  construct_word_positions beatle_target
    (load_positions "data/beatle/positions.csv")

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Initialize counters *)
let guess_counter = ref 0
let match_counter = ref 0
let hints_used = ref 0
let max_hints = 7
let guessed_words = BatSet.empty

type game_state = Cs3110_fin.Logic.game_state

let print_theme_info grid theme word_positions =
  print_endline ("Theme: " ^ theme);
  Cs3110_fin.Logic.show_grid grid BatSet.empty word_positions

(** [window_count] is how many Gmain windows are actively running*)
let window_count = ref 0

(**[destroy_window] properly exits Gmain if and only if there are no windows
   running*)
let destroy_window () =
  (* Check if any windows are active *)
  window_count := !window_count - 1;
  if !window_count < 1 then GMain.quit () else ()

(**[make_instruction_window] temporarily disables [instruction_button] and
   displays game instructions in a separate window, re-enabling
   [instruction_button] after the instructions are closed*)
let make_instruction_window instruction_button =
  instruction_button#set_sensitive false;
  let instruction_window =
    GWindow.window ~title:"Instructions" ~border_width:20 ~width:400 ~height:400
      ()
  in
  window_count := !window_count + 1;

  (* Connect destroy signal *)
  ignore (instruction_window#connect#destroy ~callback:destroy_window);

  (* Centered vertical box *)
  let vbox = GPack.vbox ~spacing:10 ~packing:instruction_window#add () in
  vbox#set_halign `CENTER;
  vbox#set_valign `CENTER;

  (* Title label *)
  let title_label = GMisc.label ~text:"How to Play" ~packing:vbox#pack () in
  ignore
    (title_label#misc#connect#realize ~callback:(fun () ->
         title_label#misc#modify_font
           (GPango.font_description_from_string "Serif 30")));

  ignore (GMisc.label ~text:"" ~height:20 ~packing:vbox#pack ());

  (* Instructions label *)
  let instructions_text =
    "Welcome to Straml: The OCaml-based adaptation of the New York Times game \
     Strands! Similar to Wordle, Strands consists of grid of letters, a theme \
     and solution words.\n\
    \ The objective of the game is to find all of the hidden answers within \
     the grid to win the game!\n\n\
     To solve words, letters can be joined together horizontally, vertically \
     or diagonally by one space to its immediate side.\n\
    \ You can combine as many unselected letters together as needed, but only \
     dictionary words of more than 3 letters will be accepted as answers.\n\n\
     Once an accepted non-theme word has been found, the match counter will \
     increase and the hint count will increase by a third. Each time 3 \
     non-theme words are found, the player will earn a hint.\n\
    \ Players can enter as many non-theme words as needed but they must use \
     the hint button before they can begin earning additional hints. \n\
    \ Using a hint highlights the letters of the theme word, but players must \
     still figure out what the word spells.\n\n\
     Keep organizing the words until all are correctly grouped. The \
     connections can sometimes be subtle, so think creatively and explore \
     different possibilities.\n\n\
     Good luck and have fun!!\n\n\
    \ Source: https://wordsrated.com/strands-nyt-game-rules-and-how-to-play/\n"
  in
  ignore
    (let instructions_label =
       GMisc.label ~text:instructions_text ~packing:vbox#pack ()
     in
     instructions_label#misc#modify_font
       (GPango.font_description_from_string "Serif 17"));

  (* Button *)
  let button_hbox = GPack.hbox ~spacing:10 ~packing:vbox#pack () in
  button_hbox#set_halign `CENTER;
  button_hbox#set_valign `CENTER;
  let quit_button = GButton.button ~label:"OK" ~packing:button_hbox#pack () in
  ignore
    (quit_button#connect#clicked ~callback:(fun () ->
         instruction_button#set_sensitive true;
         instruction_window#destroy ()));

  (* Show window *)
  instruction_window#show ()

(**[make_choose_window] creates a window with buttons in which the user can
   select a theme to play, recursively is called if user wants to start game
   again*)

let rec make_choose_window () =
  (*[stats_summary] displays a window with the user's end game stats after
    finding all the words*)
  let stats_summary state match_counter guess_counter start_time =
    (* Create new window *)
    let stats_window =
      GWindow.window ~title:"End Game Stats" ~border_width:20 ~width:400
        ~height:400 ()
    in
    window_count := !window_count + 1;

    (* Connect destroy signal *)
    ignore (stats_window#connect#destroy ~callback:destroy_window);

    (* Vertical box *)
    let vbox = GPack.vbox ~border_width:20 ~packing:stats_window#add () in

    (* Title label *)
    let title_label = GMisc.label ~text:"Game Summary" ~packing:vbox#pack () in
    ignore
      (title_label#misc#connect#realize ~callback:(fun () ->
           title_label#misc#modify_font
             (GPango.font_description_from_string "Serif 15")));

    (* Summary label *)
    let end_time = Unix.gettimeofday () in
    let elapsed_time = end_time -. start_time in
    let summary =
      Printf.sprintf "Words Found: %d\n" !match_counter
      ^ Printf.sprintf "Total Guesses: %d\n" !guess_counter
      ^ Printf.sprintf "Hints Used: %d\n" !hints_used
      ^ Printf.sprintf "Time Spent: %.2f seconds\n\n" elapsed_time
      ^ Printf.sprintf "Play again?\n"
    in
    ignore (GMisc.label ~text:summary ~packing:vbox#pack ());

    (* Buttons *)
    let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in
    let restart_button =
      GButton.button ~label:"Play Again" ~packing:hbox#pack ()
    in
    ignore
      (restart_button#connect#clicked ~callback:(fun () ->
           make_choose_window ();
           stats_window#destroy ()));

    let quit_button = GButton.button ~label:"Quit" ~packing:hbox#pack () in
    ignore
      (quit_button#connect#clicked ~callback:(fun () -> stats_window#destroy ()));

    (* Show window *)
    stats_window#show ()
  in

  let make_game_window parent grid target_words word_positions theme =
    (* Create new window *)
    let game_Window = GWindow.window ~title:theme ~border_width:20 () in
    window_count := !window_count + 1;

    (* Set up exit function when the window is closed *)
    ignore (game_Window#connect#destroy ~callback:destroy_window);

    let vbox = GPack.vbox ~spacing:10 ~packing:game_Window#add () in
    vbox#set_halign `CENTER;
    vbox#set_valign `CENTER;

    (* Create a small question mark button *)
    let help_button = GButton.button ~label:"?" ~packing:vbox#pack () in
    help_button#set_halign `END;
    help_button#set_valign `START;

    ignore
      (help_button#connect#clicked ~callback:(fun () ->
           make_instruction_window help_button));

    let title_label =
      GMisc.label ~use_underline:true ~text:theme ~packing:vbox#pack ()
    in
    title_label#misc#modify_font
      (GPango.font_description_from_string "Serif Bold 40");
    ignore (GMisc.label ~text:"" ~height:10 ~packing:vbox#pack ());

    (* Create a box for the game grid *)
    let grid_box = GPack.vbox ~packing:vbox#pack () in
    let monospace_font = GPango.font_description_from_string "Monospace 150" in

    let spacing = 0 in

    for i = 0 to Array.length grid - 1 do
      let hbox = GPack.hbox ~spacing ~packing:grid_box#pack () in
      hbox#set_halign `CENTER;

      for j = 0 to Array.length grid.(i) - 1 do
        let cell_frame =
          GBin.frame ~width:70 ~height:70 ~shadow_type:`ETCHED_IN
            ~packing:hbox#pack ()
        in

        let cell_label =
          GMisc.label
            ~text:(String.make 1 grid.(i).(j))
            ~packing:cell_frame#add ()
        in

        cell_label#misc#modify_font monospace_font;

        cell_label#set_xalign 0.5;
        cell_label#set_yalign 0.5
      done
    done;

    (* Add more vertical space between the grid and the text entry *)
    let vertical_space_after_grid = 20 in
    (* Space after grid *)
    let _spacer =
      GPack.vbox ~spacing:vertical_space_after_grid ~packing:vbox#pack ()
    in

    (* Initialize info box to display game information *)
    let info_box = GPack.vbox ~packing:vbox#pack () in
    let game_info_label =
      GMisc.label
        ~text:"Hints used: 0 Guesses to hint: 3 Words guessed: 0 Words found: 0"
        ~packing:info_box#pack ()
    in

    (* Text entry for answers *)
    let text_entry = GEdit.entry ~packing:vbox#pack () in

    (* Add space between the answer box (text entry) and buttons *)
    let hbox = GPack.hbox ~spacing:30 ~packing:vbox#pack () in
    hbox#set_halign `CENTER;

    let submit_button = GButton.button ~label:"Submit" ~packing:hbox#pack () in

    let hint_button = GButton.button ~label:"Hint" ~packing:hbox#pack () in
    hint_button#set_sensitive false;

    (* Initialize game state *)
    let accepted_words =
      Cs3110_fin.Logic.load_words "data/filtered_accepted_words.csv"
    in
    let state = ref (Cs3110_fin.Logic.initialize_game grid theme) in
    let start_time = Unix.gettimeofday () in

    let hint_function () =
      Printf.printf "Processing hint request...\n";
      Cs3110_fin.Logic.hint_revealer !state word_positions target_words
        accepted_words grid_box 2;
      hints_used := !hints_used + 1;
      hint_button#set_sensitive false;
      game_info_label#set_text
        (Printf.sprintf
           "Hints used: %s Guesses to hint: %s Words guessed: %s Words found: \
            %s"
           (string_of_int !hints_used)
           (string_of_int (3 - (!guess_counter mod 3)))
           (string_of_int !guess_counter)
           (string_of_int !match_counter))
    in

    (* Connect submit_button to process input behavior *)
    ignore
      (submit_button#connect#clicked ~callback:(fun () ->
           let guess = text_entry#text in
           match String.lowercase_ascii guess with
           | "q" ->
               stats_summary !state match_counter guess_counter start_time;
               game_Window#destroy ()
           | "hint" ->
               hint_function ();
               game_Window#show ()
           | _ ->
               let new_state =
                 Cs3110_fin.Logic.process_input !state guess target_words
                   match_counter guess_counter max_hints accepted_words
                   word_positions
               in
               Cs3110_fin.Logic.show_grid new_state.grid new_state.found_words
                 word_positions grid_box 1;

               if !guess_counter - !hints_used >= 3 then
                 hint_button#set_sensitive true;

               (* Update game_info label*)
               game_info_label#set_text
                 (Printf.sprintf
                    "Hints used: %s Guesses to hint: %s Words guessed: %s \
                     Words found: %s"
                    (string_of_int !hints_used)
                    (string_of_int (3 - (!guess_counter mod 3)))
                    (string_of_int !guess_counter)
                    (string_of_int !match_counter));

               game_Window#show ();

               if
                 BatSet.cardinal new_state.found_words
                 = List.length target_words
               then (
                 Printf.printf "Congrats! You found all the words. :)\n";
                 stats_summary new_state match_counter guess_counter start_time;
                 game_Window#destroy ())
               else state := new_state));

    (* Connect hint_button to directly ask for hint *)
    ignore (hint_button#connect#clicked ~callback:(fun () -> hint_function ()));

    (* Show initial grid *)
    Cs3110_fin.Logic.show_grid grid BatSet.empty word_positions grid_box 1;

    game_Window#show ()
  in

  (*BEGIN CHOOSE WINDOW
    FUNCTIONALITY--------------------------------------------------------------------*)

  (* Reset hint and match counters for a new game*)
  guess_counter := 0;
  match_counter := 0;
  let choose_window =
    GWindow.window ~title:"Choose Theme" ~border_width:20 ~width:400 ~height:400
      ()
  in
  window_count := !window_count + 1;

  ignore (choose_window#connect#destroy ~callback:destroy_window);

  let vbox = GPack.vbox ~spacing:20 ~packing:choose_window#add () in
  vbox#set_halign `CENTER;
  vbox#set_valign `CENTER;

  let title_label = GMisc.label ~text:"Choose Theme" ~packing:vbox#pack () in
  title_label#misc#modify_font
    (GPango.font_description_from_string "Serif Bold 20");

  let button1 =
    GButton.button ~label:"Fall Fun" ~packing:vbox#pack ~show:true ()
  in
  button1#set_width_request 300;
  ignore
    (button1#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window fall_fun_grid fall_fun_target
              fall_fun_positions "Fall Fun");
         choose_window#destroy ()));

  let button2 =
    GButton.button ~label:"Nice Fit" ~packing:vbox#pack ~show:true ()
  in
  button2#set_width_request 300;
  ignore
    (button2#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window nice_fit_grid nice_fit_target
              nice_fit_positions "Nice Fit");
         choose_window#destroy ()));

  let button3 =
    GButton.button ~label:"To Your Health" ~packing:vbox#pack ~show:true ()
  in
  button3#set_width_request 300;
  ignore
    (button3#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window health_grid health_target
              health_positions "To Your Health");
         choose_window#destroy ()));

  let button4 =
    GButton.button ~label:"Extremely Online" ~packing:vbox#pack ~show:true ()
  in
  button4#set_width_request 300;
  ignore
    (button4#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window online_grid online_target
              online_positions "Extremely Online");
         choose_window#destroy ()));

  let button5 =
    GButton.button ~label:"Beatlemania!" ~packing:vbox#pack ~show:true ()
  in
  button5#set_width_request 300;
  ignore
    (button5#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window beatle_grid beatle_target
              beatle_positions "Beatlemania!");
         choose_window#destroy ()));

  choose_window#show ()

let () =
  let start_window =
    GWindow.window ~title:"Straml" ~border_width:10 ~width:400 ~height:400 ()
  in
  (* Set the window to fullscreen *)
  start_window#fullscreen ();

  (* Set up exit function when the window is closed *)
  ignore (start_window#connect#destroy ~callback:destroy_window);
  window_count := !window_count + 1;

  (* Create vertical element box with 20 px of padding *)
  let vbox = GPack.vbox ~spacing:10 ~packing:start_window#add () in
  vbox#set_halign `CENTER;
  vbox#set_valign `CENTER;

  let title_label = GMisc.label ~text:"Straml" ~packing:vbox#pack () in
  title_label#misc#modify_font
    (GPango.font_description_from_string "Serif Bold 30");

  let description_label =
    GMisc.label ~text:"An OCaml-based adaptation of Strands" ~packing:vbox#pack
      ()
  in
  description_label#misc#modify_font
    (GPango.font_description_from_string "Serif 20");
  let phrase_label =
    GMisc.label ~text:"Find hidden words and uncover the theme!"
      ~packing:vbox#pack ()
  in
  phrase_label#misc#modify_font (GPango.font_description_from_string "Serif 15");
  ignore (GMisc.label ~text:"" ~packing:vbox#pack ());

  let name_label =
    GMisc.label
      ~text:"By: Falak Raheja, Amy Wang, Angelina Chen, and Matthew Jia"
      ~packing:vbox#pack ()
  in
  name_label#misc#modify_font (GPango.font_description_from_string "Serif 15");
  ignore (GMisc.label ~text:"" ~packing:vbox#pack ());

  let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in
  hbox#set_halign `CENTER;
  (* center horizontally in vbox *)
  hbox#set_valign `CENTER;

  (* Create a fixed container inside the horizontal box *)
  let fixed_container = GPack.fixed ~packing:hbox#add () in

  (* Create and pack quit button *)
  let quit_button = GButton.button ~label:"Quit" () in
  (* Set up a callback for the button click event *)
  ignore
    (quit_button#connect#clicked ~callback:(fun () -> start_window#destroy ()));
  fixed_container#put ~x:40 ~y:0 quit_button#coerce;

  (* Create and pack start button *)
  let start_button = GButton.button ~label:"Play" () in
  (* Set up a callback for the button click event *)
  ignore
    (start_button#connect#clicked ~callback:(fun () ->
         make_choose_window ();
         start_window#destroy ()));
  fixed_container#put ~x:120 ~y:0 start_button#coerce;

  (* Create and pack instruction button *)
  let instruction_button = GButton.button ~label:"Instructions" () in
  (* Set up a callback for the button click event *)
  ignore
    (instruction_button#connect#clicked ~callback:(fun () ->
         make_instruction_window instruction_button));
  fixed_container#put ~x:200 ~y:0 instruction_button#coerce;

  let terminal_button =
    GButton.button ~label:"Play in Terminal" ~packing:hbox#add ()
  in
  ignore
    (terminal_button#connect#clicked ~callback:(fun () ->
         let data_path, theme = select_theme () in
         let grid = load_grid (data_path ^ "/grid.csv") in
         let target_words = load_target_words (data_path ^ "/target.csv") in
         let word_positions =
           construct_word_positions target_words
             (load_positions (data_path ^ "/positions.csv"))
         in
         let state = initialize_game grid theme in
         let start_time = Unix.gettimeofday () in
         terminal_game_loop state match_counter hint_counter max_hints
           (load_words "data/filtered_accepted_words.csv")
           target_words word_positions start_time));

  (* Show all widgets *)
  start_window#show ();

  (* Start the GTK main loop *)
  Main.main ()
