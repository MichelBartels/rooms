type room = {
  id: int;
  name: string;
  rentband: string;
  rent: int;
  comments: string;
  properties: string list;
  images: string list;
  equipment: string list;
  building: string;
  address: string;
  site: string;
}
[@@deriving show]

type resp_site = {
  id: int [@key "OptimeIndex"];
  description: string [@key "Description"];
}
[@@deriving yojson {strict = false}]

type resp_building = {
  id: int [@key "OptimeIndex"];
  description: string [@key "Description"];
  address: string [@key "Address"];
  lat: string [@key "Latitude"];
  long: string [@key "Longitude"];
  site: resp_site [@key "Site"]
}
[@@deriving yojson {strict = false}]

type resp_room_type = {
  id: int [@key "OptimeIndex"];
  description: string [@key "Description"]
}
[@@deriving yojson {strict = false}]

type resp_charge = {
  id: int [@key "OptimeIndex"];
  description: string [@key "Description"];
  price: float [@key "Price_FutureExternal"];
}
[@@deriving yojson {strict = false}]

type resp_property = {
  id: int [@key "OptimeIndex"];
  description: string [@key "Description"];
  notes: string option [@key "Notes"];
}
[@@deriving yojson {strict = false}]

type resp_room = {
  id: int [@key "OptimeIndex"];
  building: resp_building [@key "Building"];
  description: string [@key "Description"];
  equipment: string list [@key "EquipmentDescriptions"];
  images: string list [@key "Images"];
  room_type: resp_room_type [@key "RoomType"];
  charge: resp_charge option [@key "StudentAccommodationChargeItem"];
  comments: string [@key "Comments"];
  properties: resp_property list [@key "RoomEquipment"];
}
[@@deriving yojson {strict = false}]

type resp = {
  rooms: resp_room list [@key "Rooms"];
}
[@@deriving yojson {exn = true; strict = false}]

type resp_room_endpoint = {
  room: resp_room [@key "Room"]
}
[@@deriving yojson {exn = true; strict = false}]

let flatten room =
  {
        id = room.id;
        name = room.description;
        rentband = Option.map (fun (charge: resp_charge) -> charge.description) room.charge |> Option.value ~default:"";
        rent = Option.map (fun charge -> Int.of_float charge.price) room.charge |> Option.value ~default:0;
        comments = room.comments;
        properties = List.map (fun (p: resp_property) -> p.description) room.properties;
        images = room.images;
        equipment = room.equipment;
        building = room.building.description;
        address = room.building.address;
        site = room.building.site.description;
      }

let parse str =
    let json = Yojson.Safe.from_string str in
    let resp = resp_of_yojson_exn json in
    List.map flatten resp.rooms

let parse_individual_room str =
  try
  let json = Yojson.Safe.from_string str in
  let resp = resp_room_endpoint_of_yojson json in
  if Result.is_error resp then (print_endline "Could not parse: "; print_endline str)
    else ();
  Option.map (fun resp -> flatten resp.room) (Result.to_option resp)
  with Yojson.Json_error str -> print_endline str; None
