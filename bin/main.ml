open GMain
open GPango
open Gtk
open Printf
open Cs3110_fin.Logic
open Cs3110_fin.Grid_data
open Unix

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Initialize counters *)
let hint_counter = ref 0
let match_counter = ref 0
let max_hints = 7
let guessed_words = BatSet.empty

type game_state = Cs3110_fin.Logic.game_state

(* Function to print the stats summary *)
let stats_summary state match_counter hint_counter start_time =
  let end_time = Unix.gettimeofday () in
  let elapsed_time = end_time -. start_time in
  Printf.printf "\n--- Game Summary ---\n";
  Printf.printf "Words Found: %d\n" (BatSet.cardinal state.found_words);
  Printf.printf "Total Guesses: %d\n" (BatSet.cardinal state.guessed_words);
  Printf.printf "Hints Used: %d\n" !hint_counter;
  Printf.printf "Time Spent: %.2f seconds\n" elapsed_time;
  Printf.printf "Thanks for playing! :) \n"

let print_theme_info grid theme word_positions =
  print_endline ("Theme: " ^ theme);
  Cs3110_fin.Logic.show_grid grid BatSet.empty word_positions

(* How many GUI windows are actively running*)
let window_count = ref 1

let destroy_window () =
  (* Check if any windows are active *)
  window_count := !window_count - 1;
  if !window_count < 1 then GMain.quit () else ()

let make_game_Window parent grid target_words word_positions theme =
  (* Create new window *)
  let game_Window = GWindow.window ~title:theme ~border_width:20 () in
  window_count := !window_count + 1;

  (* Set up exit function when the window is closed *)
  ignore (game_Window#connect#destroy ~callback:destroy_window);

  (* Create vertical element box with 20 px of padding *)
  let vbox = GPack.vbox ~border_width:20 ~packing:game_Window#add () in

  (* Create game title with font 15*)
  let title_label = GMisc.label ~text:theme ~packing:vbox#pack () in
  title_label#misc#modify_font (GPango.font_description_from_string "Serif 15");

  let grid_box = GPack.vbox ~border_width:0 ~packing:vbox#pack () in
  (* Add a label to the new window *)
  ignore (GMisc.label ~markup:"" ~packing:grid_box#pack ());

  (* Create a text entry box *)
  let text_entry = GEdit.entry ~packing:vbox#add () in

  (* Create a button *)
  let button = GButton.button ~label:"Submit" ~packing:vbox#add () in

  Cs3110_fin.Logic.show_grid grid BatSet.empty word_positions grid_box;

  let accepted_words =
    Cs3110_fin.Logic.load_words "data/filtered_accepted_words.csv"
  in
  let state = ref (Cs3110_fin.Logic.initialize_game grid theme) in
  let start_time = Unix.gettimeofday () in
  (* Connect the button click event to update the label's text *)
  ignore
    (button#connect#clicked ~callback:(fun () ->
         let guess = text_entry#text in
         if guess = "q" then (
           stats_summary !state match_counter hint_counter start_time;
           exit 0);
         let new_state =
           Cs3110_fin.Logic.process_input !state guess target_words
             match_counter hint_counter max_hints accepted_words word_positions
         in
         Cs3110_fin.Logic.show_grid new_state.grid new_state.found_words
           word_positions grid_box;
         game_Window#show ();
         if BatSet.cardinal new_state.found_words = List.length target_words
         then (
           Printf.printf "Congrats! You found all the words. :)\n";
           stats_summary new_state match_counter hint_counter start_time;
           exit 0)
         else state := new_state));

  game_Window#show ()

let make_choose_Window () =
  (* Create new window *)
  let choose_Window =
    GWindow.window ~title:"Choose Theme" ~border_width:20 ()
  in
  window_count := !window_count + 1;

  (* Set up exit function when the window is closed *)
  ignore (choose_Window#connect#destroy ~callback:destroy_window);

  (* Create vertical element box with 20 px of padding *)
  let vbox = GPack.vbox ~border_width:20 ~packing:choose_Window#add () in
  (* Add a label to the new window *)
  ignore (GMisc.label ~text:"Choose a Theme:" ~packing:vbox#pack ());

  let button1 = GButton.button ~label:"Fall Fun" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button1#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_Window choose_Window GridData.initial_grid
              GridData.target_words GridData.word_positions "Fall Fun");
         choose_Window#destroy ()));

  let button2 = GButton.button ~label:"Well-Suited" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button2#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_Window choose_Window GridData.nice_fit
              GridData.nice_fit_target GridData.word_positions "Well-Suited");
         choose_Window#destroy ()));

  let button3 = GButton.button ~label:"To Your Health" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button3#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_Window choose_Window GridData.to_your_health
              GridData.to_your_health_target GridData.to_your_health_position
              "To Your Health");
         choose_Window#destroy ()));

  let button4 =
    GButton.button ~label:"Extremely Online" ~packing:vbox#pack ()
  in
  (* Set up a callback for the button click event *)
  ignore
    (button4#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_Window choose_Window GridData.extremely_online
              GridData.extremely_online_target
              GridData.extremely_online_positions "Extremely Online");
         choose_Window#destroy ()));

  let button5 = GButton.button ~label:"Beatlemania!" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (button5#connect#clicked ~callback:(fun () ->
         ignore
           (make_game_Window choose_Window GridData.beatlemania
              GridData.beatlemania_target GridData.extremely_online_positions
              "Beatlemania!");
         choose_Window#destroy ()));
  choose_Window#show ()

(* Execute GUI*)
let () =
  let start_Window = GWindow.window ~title:"Straml" ~border_width:10 () in
  (* Set up exit function when the window is closed *)
  ignore (start_Window#connect#destroy ~callback:destroy_window);

  (* Create vertical element box with 20 px of padding *)
  let vbox = GPack.vbox ~border_width:20 ~packing:start_Window#add () in
  (* Create game title and subtitle with font 20 *)
  let title_label = GMisc.label ~text:"Straml" ~packing:vbox#pack () in
  title_label#misc#modify_font (GPango.font_description_from_string "Serif 20");
  ignore
    (GMisc.label ~text:"By: Falak, Amy, Angie, and Matthew" ~packing:vbox#pack
       ());

  (* Create a start button to execute program *)
  let start_button = GButton.button ~label:"Play" ~packing:vbox#pack () in
  (* Set up a callback for the button click event *)
  ignore
    (start_button#connect#clicked ~callback:(fun () ->
         make_choose_Window ();
         start_Window#destroy ()));

  (* Show all widgets *)
  start_Window#show ();

  (* Start the GTK main loop *)
  Main.main ()
