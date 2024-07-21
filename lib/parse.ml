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
  charge: resp_charge [@key "StudentAccommodationChargeItem"];
  comments: string [@key "Comments"];
  properties: resp_property list [@key "RoomEquipment"];
}
[@@deriving yojson {strict = false}]

type resp = {
  rooms: resp_room list [@key "Rooms"];
}
[@@deriving yojson {exn = true; strict = false}]

let flatten resp =
  List.map resp.rooms ~f:(fun room ->
      {
        id = room.id;
        name = room.description;
        rentband = room.charge.description;
        rent = Int.of_float room.charge.price;
        comments = room.comments;
        properties = List.map room.properties ~f:(fun p -> p.description);
        images = room.images;
        equipment = room.equipment;
        building = room.building.description;
        address = room.building.address;
        site = room.building.site.description;
      })

let parse str =
    let json = Yojson.Safe.from_string str in
    let resp = resp_of_yojson_exn json in
    resp.rooms |> List.length |> Int.to_string |> Out_channel.print_string;
    resp
