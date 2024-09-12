open Yojson.Basic.Util

type reason = JsonParseError of string
type ('a, 'b) result_from_try = ('a -> 'b) -> 'a -> ('b, reason) result

let result_from_try : ('a, 'b) result_from_try =
  fun f x ->
  try Ok (f x) with
  | Failure msg -> Error (JsonParseError msg)
  | e -> Error (JsonParseError ("Unexpected exception: " ^ Printexc.to_string e))
;;

let get_email json = json |> member "emailAddress" |> to_string
let get_cardNumber json = json |> member "cardNumber" |> to_string
let get_cardMonth json = json |> member "cardMonth" |> to_string
let get_cardYear json = json |> member "cardYear" |> to_string
