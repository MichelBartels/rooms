open Core
open Caqti_request.Infix
open Caqti_type.Std

module type Table = sig
  type t
  val create: string
  val t: t Caqti_type.t
  val add: string
end

module RoomBase = struct
  type t = {
    id: int;
    building_id: int;
    description: string;
    charge_id: int;
    comments: string;
    room_type: string;
  }
  let create = 
    {|
      CREATE TABLE room (
        id INT PRIMARY KEY,
        building_id INT,
        description TEXT,
        charge_id INT,
        comments TEXT,
        room_type INT
    )
  |}
  let t =
    let encode {id; building_id; description; charge_id; comments; room_type} = Ok (id, building_id, description, (charge_id, comments, room_type)) in
    let decode (id, building_id, description, (charge_id, comments, room_type)) = Ok {id; building_id; description; charge_id; comments; room_type} in
    let rep = tup4 int int string (tup3 int string string) in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO room (id, building_id, description, charge_id, comments, room_type) VALUES (?, ?, ?, ?, ?, ?)"
end

module PropertyBase = struct
  type t = {
    id: int;
    description: string;
    notes: string option;
  }
  let create =
    {|
      CREATE TABLE property (
        id INT PRIMARY KEY,
        description TEXT,
        notes TEXT
      )
    |}
  let t =
    let encode {id; description; notes} = Ok (id, description, notes) in
    let decode (id, description, notes) = Ok {id; description; notes} in
    let rep = tup3 int string (option string) in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO property (id, description, notes) VALUES (?, ?, ?)"
end

module ChargeBase = struct
  type t = {
    id: int;
    description: string;
    price: int;
  }
  let create =
    {|
      CREATE TABLE charge (
        id INT PRIMARY KEY,
        description TEXT,
        price INT
      )
    |}
  let t =
    let encode {id; description; price} = Ok (id, description, price) in
    let decode (id, description, price) = Ok {id; description; price} in
    let rep = tup3 int string int in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO charge (id, description, price) VALUES (?, ?, ?)"
end

module BuildingBase = struct
  type t = {
    id: int;
    description: string;
    address: string;
    latitude: float;
    longitude: float;
    site: string;
  }
  let create =
    {|
      CREATE TABLE building (
        id INT PRIMARY KEY,
        description TEXT,
        address TEXT,
        latitude DOUBLE,
        longitude DOUBLE,
        site TEXT
      )
    |}
  let t =
    let encode {id; description; address; latitude; longitude; site} = Ok (id, description, address, (latitude, longitude, site)) in
    let decode (id, description, address, (latitude, longitude, site)) = Ok {id; description; address; latitude; longitude; site} in
    let rep = tup4 int string string (tup3 float float string) in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO building (id, description, address, latitude, longitude, site) VALUES (?, ?, ?, ?, ?, ?)"
end

module EquipmentBase = struct
  type t = {
    room_id: int;
    description: string;
  }
  let create =
    {|
      CREATE TABLE equipment (
        room_id INT,
        description TEXT
      )
    |}
  let t =
    let encode {room_id; description} = Ok (room_id, description) in
    let decode (room_id, description) = Ok {room_id; description} in
    let rep = tup2 int string in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO equipment (room_id, description) VALUES (?, ?)"
end

module ImageBase = struct
  type t = {
    room_id: int;
    image: string;
  }
  let create =
    {|
      CREATE TABLE image (
        room_id INT,
        image TEXT
      )
    |}
  let t =
    let encode {room_id; image} = Ok (room_id, image) in
    let decode (room_id, image) = Ok {room_id; image} in
    let rep = tup2 int string in
    custom ~encode ~decode rep
  let add =
    "INSERT INTO image (room_id, image) VALUES (?, ?)"
end

let unwrap = function
  | Ok v -> v
  | Error err -> failwith (Caqti_error.show err)

let unwrap_lwt a = a |> Lwt_main.run |> unwrap

let db_uri =
  Uri.of_string "sqlite3:rooms.db"

let pool =
  Caqti_lwt.connect_pool db_uri |> unwrap

let apply v query =
  let inner query' (module C: Caqti_lwt.CONNECTION) =
    C.exec query' v in
  (query |> inner |> Caqti_lwt.Pool.use) pool |> unwrap_lwt

module Table (T: Table) = struct
  type t = T.t
  let create = 
    unit ->. unit @@ T.create
  let t = T.t
  let add row = t ->. unit @@ T.add |> apply row
end

module Room = Table(RoomBase)
module Property = Table(PropertyBase)
module Charge = Table(ChargeBase)
module Building = Table(BuildingBase)
module Equipment = Table(EquipmentBase)
module Image = Table(ImageBase)

let create =
  List.map [Room.create; Property.create; Charge.create; Building.create; Equipment.create; Image.create] ~f:(apply ()) |> ignore
