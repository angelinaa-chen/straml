module GridData = struct
  type grid = char array array

  (* Spangram: Orchards (?) *)
  let initial_grid : grid =
    [|
      [| 'S'; 'N'; 'P'; 'M'; 'D'; 'S' |];
      [| 'I'; 'K'; 'U'; 'I'; 'R'; 'E' |];
      [| 'E'; 'A'; 'M'; 'P'; 'H'; 'Y' |];
      [| 'Z'; 'R'; 'H'; 'A'; 'D'; 'A' |];
      [| 'O'; 'U'; 'C'; 'G'; 'R'; 'S' |];
      [| 'T'; 'C'; 'N'; 'H'; 'U'; 'P' |];
      [| 'R'; 'S'; 'I'; 'O'; 'P'; 'L' |];
      [| 'E'; 'D'; 'D'; 'A'; 'E'; 'S' |];
    |]

  (* Spangram: Medical career *)
  let to_your_health : grid =
    [|
      [| 'S'; 'N'; 'U'; 'M'; 'A'; 'R' |];
      [| 'E'; 'R'; 'E'; 'D'; 'H'; 'M' |];
      [| 'D'; 'E'; 'I'; 'P'; 'A'; 'C' |];
      [| 'N'; 'C'; 'A'; 'L'; 'T'; 'I' |];
      [| 'T'; 'I'; 'S'; 'T'; 'C'; 'S' |];
      [| 'E'; 'O'; 'E'; 'R'; 'A'; 'R' |];
      [| 'G'; 'N'; 'E'; 'C'; 'T'; 'O' |];
      [| 'R'; 'U'; 'S'; 'R'; 'O'; 'D' |];
    |]

  (* Spangram: Well-suited *)
  let nice_fit : grid =
    [|
      [| 'P'; 'M'; 'L'; 'E'; 'R'; 'E' |];
      [| 'U'; 'T'; 'H'; 'Y'; 'I'; 'U' |];
      [| 'J'; 'I'; 'R'; 'D'; 'A'; 'S' |];
      [| 'B'; 'L'; 'L'; 'S'; 'U'; 'D' |];
      [| 'W'; 'E'; 'B'; 'I'; 'T'; 'E' |];
      [| 'S'; 'U'; 'S'; 'G'; 'N'; 'O' |];
      [| 'I'; 'E'; 'C'; 'S'; 'L'; 'R' |];
      [| 'N'; 'I'; 'V'; 'I'; 'S'; 'T' |];
    |]

  (* Spangram: Social Media *)
  let extremely_online : grid =
    [|
      [| 'T'; 'W'; 'S'; 'S'; 'H'; 'O' |];
      [| 'E'; 'O'; 'R'; 'A'; 'F'; 'L' |];
      [| 'E'; 'C'; 'E'; 'W'; 'O'; 'L' |];
      [| 'T'; 'I'; 'C'; 'M'; 'T'; 'N' |];
      [| 'A'; 'E'; 'O'; 'A'; 'M'; 'E' |];
      [| 'R'; 'L'; 'K'; 'L'; 'I'; 'P' |];
      [| 'E'; 'P'; 'M'; 'I'; 'D'; 'O' |];
      [| 'T'; 'R'; 'O'; 'E'; 'S'; 'T' |];
    |]

  let bealtemania : grid =
    [|
      [| 'X'; 'M'; 'A'; 'N'; 'P'; 'L' |];
      [| 'A'; 'T'; 'V'; 'O'; 'E'; 'H' |];
      [| 'R'; 'E'; 'T'; 'I'; 'L'; 'T' |];
      [| 'N'; 'G'; 'D'; 'T'; 'U'; 'I' |];
      [| 'Y'; 'O'; 'R'; 'I'; 'L'; 'O' |];
      [| 'D'; 'A'; 'S'; 'B'; 'E'; 'N' |];
      [| 'R'; 'S'; 'K'; 'C'; 'A'; 'S' |];
      [| 'E'; 'T'; 'E'; 'Y'; 'L'; 'B' |];
    |]

  let target_words =
    [
      "hayrides"; "pumpkins"; "maze"; "cider"; "doughnuts"; "apples"; "orchards";
    ]

  let extremely_online_target =
    [
      "tweet";
      "report";
      "share";
      "follow";
      "comment";
      "post";
      "like";
      "report";
      "socialmedia";
    ]

  let nice_fit_target =
    [
      "jump"; "birthday"; "leisure"; "business"; "civil"; "strong"; "wellsuited";
    ]

  let to_your_health_target =
    [ "nurse"; "dentist"; "medicalcareer"; "surgeon"; "pharmacist"; "doctor" ]

  let bealtemania_target =
    [ "revolution"; "blackbird"; "songtitles"; "yesterday"; "help"; "taxman" ]

  let word_positions =
    [
      ( "hayrides",
        [ (2, 4); (3, 5); (2, 5); (1, 4); (1, 3); (0, 4); (1, 5); (0, 5) ] );
      ( "pumpkins",
        [ (2, 3); (1, 2); (0, 3); (0, 2); (1, 1); (1, 0); (0, 1); (0, 0) ] );
      ("maze", [ (2, 2); (2, 1); (3, 0); (2, 0) ]);
      ("cider", [ (5, 1); (6, 2); (7, 1); (7, 0); (6, 0) ]);
      ( "doughnuts",
        [
          (7, 2); (6, 3); (5, 4); (4, 3); (5, 3); (5, 2); (4, 1); (5, 0); (6, 1);
        ] );
      ("apples", [ (7, 3); (6, 4); (5, 5); (6, 5); (7, 4); (7, 5) ]);
      ( "orchards",
        [ (4, 0); (3, 1); (4, 2); (3, 2); (3, 3); (4, 4); (3, 4); (4, 5) ] );
    ]

  let to_your_health_position =
    [
      ("nurse", [ (0, 1); (0, 2); (1, 1); (0, 0); (1, 0) ]);
      ("dentist", [ (2, 0); (2, 1); (3, 0); (4, 0); (4, 1); (4, 2); (4, 3) ]);
      ( "medicalcareer",
        [
          (3, 0);
          (2, 1);
          (3, 1);
          (2, 2);
          (3, 1);
          (3, 2);
          (3, 3);
          (4, 4);
          (5, 4);
          (5, 3);
          (5, 2);
          (6, 2);
          (7, 3);
        ] );
      ("surgeon", [ (7, 2); (7, 1); (7, 0); (6, 0); (5, 0); (5, 1); (6, 1) ]);
      ( "pharmacist",
        [
          (2, 3);
          (1, 4);
          (0, 4);
          (0, 5);
          (1, 5);
          (2, 4);
          (2, 5);
          (3, 5);
          (4, 5);
          (3, 4);
        ] );
      ("doctor", [ (7, 5); (7, 4); (6, 3); (6, 4); (6, 5); (5, 5) ]);
    ]

  let extremely_online_positions =
    [
      ("tweet", [ (0, 0); (0, 1); (1, 0); (2, 0); (3, 0) ]);
      ("report", [ (5, 0); (6, 0); (6, 1); (7, 2); (7, 1); (7, 0) ]);
      ("share", [ (3, 0); (4, 0); (3, 1); (2, 1); (2, 2) ]);
      ("follow", [ (1, 4); (0, 5); (1, 5); (2, 5); (2, 4); (2, 3) ]);
      ("comment", [ (3, 2); (4, 2); (3, 3); (4, 4); (4, 5); (3, 5); (3, 4) ]);
      ("post", [ (5, 5); (6, 5); (7, 4); (7, 5) ]);
      ("like", [ (5, 3); (6, 3); (5, 2); (4, 1) ]);
      ("report", [ (5, 0); (6, 0); (6, 1); (7, 2); (7, 1); (7, 0) ]);
      ( "socialmedia",
        [
          (2, 0);
          (1, 1);
          (2, 1);
          (3, 1);
          (4, 0);
          (5, 1);
          (6, 2);
          (7, 3);
          (6, 4);
          (5, 4);
          (4, 3);
        ] );
    ]

  let nice_fit_position =
    [
      ("jump", [ (0, 2); (0, 1); (1, 1); (0, 0) ]);
      ( "birthday",
        [ (0, 3); (1, 2); (2, 2); (1, 1); (2, 1); (3, 2); (4, 2); (3, 1) ] );
      ("leisure", [ (0, 2); (0, 3); (1, 4); (2, 5); (1, 5); (0, 4); (0, 5) ]);
      ( "wellsuited",
        [
          (4, 0);
          (4, 1);
          (3, 1);
          (3, 2);
          (3, 3);
          (3, 4);
          (4, 3);
          (4, 3);
          (4, 5);
          (3, 5);
        ] );
      ( "business",
        [ (4, 2); (5, 1); (5, 0); (6, 0); (7, 0); (6, 1); (5, 2); (6, 3) ] );
      ("strong", [ (7, 4); (7, 5); (6, 5); (5, 5); (5, 4); (5, 3) ]);
      ("civil", [ (6, 2); (7, 1); (7, 2); (7, 3); (6, 4) ]);
    ]
end
