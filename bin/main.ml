open GMain
open GPango

(* Initialize GTK *)
let () = ignore (GMain.init ())

(* Create a new window *)
let window = GWindow.window ~title:"OCaml Strands" ~border_width:10 ()

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
         print_endline "Substitute function here"));

  (* Show all widgets *)
  window#show ();

  (* Start the GTK main loop *)
  Main.main ()
