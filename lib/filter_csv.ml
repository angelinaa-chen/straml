open Csv
open Re

let input_file = "accepted_words.csv"
let output_file = "filtered_accepted_words.csv"

let is_valid word =
  let word = String.trim word in
  (* Checks if the word length is greater than 4 & starts with an alphabetical
     letter *)
  String.length word > 4 && Re.execp (Re.Perl.compile_pat "^[a-zA-Z]") word

(* Iterate over rows in the CSV *)
let process_row row = List.filter is_valid row

let () =
  let csv_in = Csv.load input_file in

  let filtered_csv =
    List.filter_map
      (fun row ->
        let filtered_row = process_row row in
        (* Filters out any empty rows *)
        if filtered_row = [] then None else Some filtered_row)
      csv_in
  in

  Csv.save output_file filtered_csv;

  Printf.printf "Filtered data saved to %s.\n" output_file
