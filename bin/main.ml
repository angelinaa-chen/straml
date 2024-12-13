open GMain
open GPango
open Gtk
open Printf
open Cs3110_fin.Logic
open Unix
open Gdk

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
let hint_counter = ref 0
let match_counter = ref 0
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

  (* Vertical box *)
  let vbox = GPack.vbox ~border_width:20 ~packing:instruction_window#add () in

  (* Title label *)
  let title_label = GMisc.label ~text:"How to Play" ~packing:vbox#pack () in
  ignore
    (title_label#misc#connect#realize ~callback:(fun () ->
         title_label#misc#modify_font
           (GPango.font_description_from_string "Serif 15")));

  (* Summary label *)
  let instructions_text =
    "Welcome to **Straml**, the word association game! Your goal is to group a \
     list of given words into sets based on their common themes or \
     connections.\n\n\
     Unlike the touchscreen version, you’ll use a text input box to create \
     your groups. When you’re ready to submit a group, type the words into the \
     input box separated by commas (e.g., `apple, orange, banana` for fruits) \
     and press the **Submit** button.\n\n\
     If your group is correct, it will be accepted, and the grouped words will \
     be removed from the list. If the group is incorrect, you’ll be prompted \
     to try again.\n\n\
     Keep organizing the words until all are correctly grouped. Remember, the \
     connections can sometimes be subtle, so think creatively and explore \
     different possibilities.\n\n\
     Good luck and have fun!\n"
  in
  ignore (GMisc.label ~text:instructions_text ~packing:vbox#pack ());

  (* Buttons *)
  let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in

  let quit_button = GButton.button ~label:"Ok" ~packing:hbox#pack () in
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
  (*BEGIN INTERNAL FUNCTION
    DEFINITIONS-----------------------------------------------------*)

  (*[stats_summary] displays a window with the user's end game stats after
    finding all the words*)
  let stats_summary state match_counter hint_counter start_time =
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
      Printf.sprintf "Words Found: %d\n" (BatSet.cardinal state.found_words)
      ^ Printf.sprintf "Total Guesses: %d\n"
          (BatSet.cardinal state.guessed_words)
      ^ Printf.sprintf "Hints Used: %d\n" !hint_counter
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
    let game_Window =
      GWindow.window ~title:theme ~border_width:20 ~width:400 ~height:400 ()
    in
    window_count := !window_count + 1;

    (* Set up exit function when the window is closed *)
    ignore (game_Window#connect#destroy ~callback:destroy_window);

    (* Use a fixed container to position the help button *)
    let fixed_container = GPack.fixed ~packing:game_Window#add () in

    (* Create a small question mark button *)
    let help_button =
      GButton.button ~label:"?" ~packing:(fixed_container#put ~x:180 ~y:10) ()
    in
    (* Add a callback to the button *)
    ignore
      (help_button#connect#clicked ~callback:(fun () ->
           make_instruction_window help_button));

    (* Create vertical element box with 10 px of padding *)
    let vbox = GPack.vbox ~spacing:10 () in
    fixed_container#put ~x:40 ~y:50 vbox#coerce;

    (* Create game title with font 15*)
    let title_label =
      GMisc.label ~use_underline:true ~text:theme ~packing:vbox#pack ()
    in
    title_label#misc#modify_font
      (GPango.font_description_from_string "Serif 15");

    let grid_box = GPack.vbox ~border_width:0 ~packing:vbox#pack () in
    (* Add a label to the new window *)
    ignore (GMisc.label ~markup:"" ~packing:grid_box#pack ());

    (* Create a text entry box *)
    let text_entry = GEdit.entry ~packing:vbox#add () in

    let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in

    (* Create a button *)
    let submit_button = GButton.button ~label:"Submit" ~packing:hbox#add () in
    (* Create a button *)
    let hint_button = GButton.button ~label:"Hint" ~packing:hbox#add () in

    let accepted_words =
      Cs3110_fin.Logic.load_words "data/filtered_accepted_words.csv"
    in
    let state = ref (Cs3110_fin.Logic.initialize_game grid theme) in
    let start_time = Unix.gettimeofday () in

    (* Connect submit_button to process input behavior*)
    ignore
      (submit_button#connect#clicked ~callback:(fun () ->
           let guess = text_entry#text in
           match String.lowercase_ascii guess with
           | "q" ->
               stats_summary !state match_counter hint_counter start_time;
               game_Window#destroy ()
           | "hint" ->
               Printf.printf "Processing hint request...\n";
               Cs3110_fin.Logic.hint_revealer !state word_positions target_words
                 accepted_words grid_box 2;
               game_Window#show ()
           | _ ->
               let new_state =
                 Cs3110_fin.Logic.process_input !state guess target_words
                   match_counter hint_counter max_hints accepted_words
                   word_positions
               in
               Cs3110_fin.Logic.show_grid new_state.grid new_state.found_words
                 word_positions grid_box 1;
               game_Window#show ();
               if
                 BatSet.cardinal new_state.found_words
                 = List.length target_words
               then (
                 Printf.printf "Congrats! You found all the words. :)\n";
                 stats_summary new_state match_counter hint_counter start_time;
                 game_Window#destroy ())
               else state := new_state));

    (* Connect hint_button to directly ask for hint*)
    ignore
      (hint_button#connect#clicked ~callback:(fun () ->
           Printf.printf "Processing hint request...\n";
           Cs3110_fin.Logic.hint_revealer !state word_positions target_words
             accepted_words grid_box 2;
           game_Window#show ()));

    (* Show initial grid*)
    Cs3110_fin.Logic.show_grid grid BatSet.empty word_positions grid_box 1;

    game_Window#show ()
  in

  (*BEGIN CHOOSE WINDOW
    FUNCTIONALITY--------------------------------------------------------------------*)

  (* Reset hint and match counters for a new game*)
  hint_counter := 0;
  match_counter := 0;

  (* Create new window *)
  let choose_window =
    GWindow.window ~title:"Choose Theme" ~border_width:20 ~width:400 ~height:400
      ()
  in
  window_count := !window_count + 1;

  (* Set up exit function when the window is closed *)
  ignore (choose_window#connect#destroy ~callback:destroy_window);

  (* Create vertical element box with 10 px of padding *)
  let vbox = GPack.vbox ~spacing:10 ~packing:choose_window#add () in
  (* Add a label to the new window *)
  ignore (GMisc.label ~text:"Choose a Theme:" ~packing:vbox#pack ());

  let button1 = GButton.button ~label:"Fall Fun" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button1#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window fall_fun_grid fall_fun_target
              fall_fun_positions "Fall Fun");
         (* (make_game_window choose_window GridData.initial_grid
            GridData.target_words GridData.word_positions "Fall Fun"); *)
         choose_window#destroy ()));

  let button2 = GButton.button ~label:"Nice Fit" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button2#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window nice_fit_grid nice_fit_target
              nice_fit_positions "Nice Fit");
         (* (make_game_window choose_window GridData.nice_fit
            GridData.nice_fit_target GridData.word_positions "Well-Suited"); *)
         choose_window#destroy ()));

  let button3 = GButton.button ~label:"To Your Health" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button3#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window health_grid health_target
              health_positions "To Your Health");
         (* (make_game_window choose_window GridData.to_your_health
            GridData.to_your_health_target GridData.to_your_health_position "To
            Your Health"); *)
         choose_window#destroy ()));

  let button4 =
    GButton.button ~label:"Extremely Online" ~packing:vbox#pack ()
  in
  (* Set up a callback for the button click event *)
  ignore
    (button4#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window online_grid online_target
              online_positions "Extremely Online");
         (* (make_game_window choose_window GridData.extremely_online
            GridData.extremely_online_target GridData.extremely_online_positions
            "Extremely Online"); *)
         choose_window#destroy ()));

  let button5 = GButton.button ~label:"Beatlemania!" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button5#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_window choose_window beatle_grid beatle_target
              beatle_positions "Beatlemania!");
         (* (make_game_window choose_window GridData.beatlemania
            GridData.beatlemania_target GridData.extremely_online_positions
            "Beatlemania!"); *)
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
  let vbox = GPack.vbox ~spacing:50 ~packing:start_window#add () in
  vbox#set_halign `CENTER;
  vbox#set_valign `CENTER;

  let title_label = GMisc.label ~text:"Straml" ~packing:vbox#pack () in
  title_label#misc#modify_font
    (GPango.font_description_from_string "Serif Bold 30");

  let phrase_label =
    GMisc.label ~text:"Find hidden words and uncover the day’s theme."
      ~packing:vbox#pack ()
  in
  phrase_label#misc#modify_font (GPango.font_description_from_string "Serif 20");
  ignore
    (GMisc.label ~text:"By: Falak, Amy, Angie, and Matthew" ~packing:vbox#pack
       ());

  let hbox = GPack.hbox ~spacing:20 ~packing:vbox#pack () in
  hbox#set_halign `CENTER;
  (* center horizontally in vbox *)
  hbox#set_valign `CENTER;

  (* center vertically in vbox *)

  (* Create a fixed container inside the horizontal box *)
  let fixed_container = GPack.fixed ~packing:hbox#add () in

  (* Create and pack start button *)
  let start_button = GButton.button ~label:"Play" () in
  (* Set up a callback for the button click event *)
  ignore
    (start_button#connect#clicked ~callback:(fun () ->
         make_choose_window ();
         start_window#destroy ()));
  fixed_container#put ~x:40 ~y:0 start_button#coerce;

  (* Create and pack instruction button *)
  let instruction_button = GButton.button ~label:"Instructions" () in
  (* Set up a callback for the button click event *)
  ignore
    (instruction_button#connect#clicked ~callback:(fun () ->
         make_instruction_window instruction_button));
  fixed_container#put ~x:120 ~y:0 instruction_button#coerce;

  (* Show all widgets *)
  start_window#show ();

  (* Start the GTK main loop *)
  Main.main ()
