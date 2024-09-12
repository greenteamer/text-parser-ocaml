module Bat = Batteries
open Yojson.Basic
open Yojson.Basic.Util

type json = Yojson.Basic.t

let input_file = "target-logs"
let coollist_file = "coollist.txt"
let output_file = "output.json"
let excluded_list_file = [ "test.json" ]
let try_card_number = Utils.result_from_try Utils.get_cardNumber

(* Функция для записи JSON объекта в файл *)
let write_json_to_file filename json =
  let out_chan = open_out filename in
  json |> pretty_to_string |> output_string out_chan;
  close_out out_chan
;;

let make_list str = Str.split (Str.regexp "DATA: ") str

let get_data_from_line line =
  let str_list = make_list line in
  let rec aux lst =
    match lst with
    | [] -> failwith "Unexpected line"
    | [ x ] -> x
    | _ :: xs -> aux xs
  in
  aux str_list
;;

let try_props_data yojson =
  try yojson |> member "props" with
  | Type_error (msg, _) -> failwith ("Error getting props: " ^ msg)
;;

(* Remove fields from json *)
let remove_fields fields_to_remove (json : json) =
  match json with
  | `Assoc assoc_list ->
    let filtered_list =
      List.filter (fun (key, _value) -> not (List.mem key fields_to_remove)) assoc_list
    in
    `Assoc filtered_list
  | _ -> failwith "Expected a JSON object"
;;

let print_json_type (json : json) =
  let () =
    match json with
    | `Assoc _ -> print_endline "It's an Assoc"
    | `List _ -> print_endline "It's a List"
    | `String str -> print_endline str
    | `Int _ -> print_endline "It's an Int"
    | `Float _ -> print_endline "It's a Float"
    | `Bool _ -> print_endline "It's a Bool"
    | `Null -> print_endline "It's Null"
  in
  json
;;

(* filter *)
let filter (json : json) =
  match json with
  | `Assoc _ ->
    (try
       let cardNumber = Utils.get_cardNumber json in
       let cardMonth = Utils.get_cardMonth json in
       let cardYear = Utils.get_cardYear json in
       match cardNumber, cardMonth, cardYear with
       | _, m, y ->
         (match int_of_string m, int_of_string y with
          | _, year when year > 2024 -> true
          | _, year when year > 24 && year < 1000 -> true
          | month, 24 when month > 8 -> true
          | month, 2024 when month > 8 -> true
          | _ -> false)
     with
     | _ -> false)
  | _ -> false
;;

let coollist_filter (bins : string list) (json : json) =
  let contains str entry =
    try Str.search_forward (Str.regexp entry) str 0 >= 0 with
    | _ -> false
  in
  let res =
    json |> try_card_number |> Result.map (fun cn -> List.exists (contains cn) bins)
  in
  match res with
  | Ok exist -> exist
  | Error _ -> false
;;

let process_list_file filename =
  let filelines = Bat.File.lines_of filename in
  filelines |> Bat.Enum.map get_data_from_line |> Bat.List.of_enum
;;

let process_json_file filename coollist =
  let filelines = Bat.File.lines_of filename in
  filelines
  |> Bat.Enum.map get_data_from_line
  |> Bat.Enum.map from_string
  |> Bat.Enum.map try_props_data
  |> Bat.Enum.filter (fun json -> filter json)
  |> Bat.Enum.filter (fun json -> coollist_filter coollist json)
  |> Bat.Enum.map
       (remove_fields
          [ "campaignId"; "affId"; "product1_id"; "product1_qty"; "salesUrl" ])
  |> Bat.List.of_enum
  |> (fun x ->
       let ln = Bat.List.length x in
       print_endline (Int.to_string ln);
       x)
  |> fun list -> `List list
;;

let () =
  let program input coollist =
    coollist
    |> process_list_file
    |> process_json_file input
    |> write_json_to_file output_file
  in
  ignore (program input_file coollist_file);
  print_endline "done"
;;
