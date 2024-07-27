type building = {
  name: string;
  rooms: string list;
}
[@@deriving yojson]

type buildings = building list
[@@deriving yojson]

let building_file = "reviews.json"
let buildings = Yojson.Safe.from_file building_file |> buildings_of_yojson |> function
  | Ok r -> r
  | Error e -> failwith e

type room = {
  building: string;
  name: string;
}
[@@deriving yojson, show]

let rooms = List.map (fun (b: building) -> List.map (fun r -> {building = b.name; name = r}) b.rooms) buildings |> List.flatten

module IntSet = Set.Make(Int)

let extract_numbers s =
  let rec extract_numbers' acc = function
    | [] -> acc
    | c :: cs when c >= '0' && c <= '9' -> (match acc with
      | x :: xs -> extract_numbers' ((x ^ (String.make 1 c)) :: xs) cs
      | [] -> extract_numbers' [String.make 1 c] cs
    )
    | _ :: cs -> extract_numbers' (""::acc) cs in
  let numbers = extract_numbers' [] (List.init (String.length s) (String.get s)) in
  let numbers = List.filter (fun s -> s <> "") numbers in
  let numbers = List.map int_of_string numbers in
  IntSet.of_list numbers

let contains s1 s2 =
  let re = Str.regexp_string s2 in
  try ignore (Str.search_forward re s1 0); true
  with Not_found -> false

let is_match (r1: Parse.room) (r2: room) =
  (*let words1 = r1.address |> String.split_on_char ' ' in
  let words2 = r1.name |> String.split_on_char ' ' in
  let words = List.append words1 words2 in
  let score = List.fold_left (fun acc w -> if contains r2.name w then acc + 1 else acc) 0 words in
  let score = score + List.fold_left (fun acc w -> if contains r2.name w then acc + 1 else acc) 0 words in*)
  let numbers1 = extract_numbers r1.name in
  let numbers2 = extract_numbers r2.name in
  if not (IntSet.equal numbers1 numbers2) then false else
  if contains r1.building "Cosin" then false else
  if contains r1.building "Fitzwilliam" <> contains r2.building "Fitzwilliam" then false else
  if contains r1.building "William" <> contains r2.building "William" then false else
  if contains r1.building "Whittle" <> contains r2.building "Whittle" then false else
  if contains r1.building "Little" <> contains r2.building "Little" then false else
  if contains r1.building "Hostel" <> contains r2.building "Hostel" then false else
  if contains r1.building "Trumpington" <> contains r2.building "Trumpington" then false else
  if contains r1.building "Fen" <> contains r2.building "Fen" then false else
  if contains r1.name "Noah" <> contains r2.building "Noah" then false else
  if (contains r1.building "Old" || contains r1.building "Gisborne") <> contains r2.building "Staircase" then false else
  if contains r1.building "Peter" then false else
  if contains r1.building "Parkside" <> contains r2.building "Parkside" then false else
  if contains r1.building "Court B" <> contains r2.building "B Staircase" then false else
  if contains r1.building "Court C" <> contains r2.building "C Staircase" then false else
  if contains r1.building "Court D" <> contains r2.building "D Staircase" then false else
  if contains r1.building "Court G" <> contains r2.building "G Staircase" then false else
  if contains r1.building "Court H" <> contains r2.building "H Staircase" then false else
  if contains r1.building "Court I" <> contains r2.building "I Staircase" then false else
  if contains r1.building "Court K" <> contains r2.building "K Staircase" then false else
  if contains r1.building "Court L" <> contains r2.building "L Staircase" then false else
  if contains r1.building "Court M" <> contains r2.building "M Staircase" then false else
  true


type room_with_review = Parse.room * room option
[@@deriving show]

let matches = List.map (fun r -> (r, List.find_opt (fun r2 -> is_match r r2) rooms))
