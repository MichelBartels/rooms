(* Upload rooms to Notion *)
open Core

type parent = {
  type_: string [@key "type"];
  database_id : string
}
[@@deriving yojson]

type content = {
  content: string;
}
[@@deriving yojson]

type text_object = {
  type_: string [@key "type"];
  text: content [@key "text"];
}
[@@deriving yojson]

let text_object_of_string s = { type_ = "text"; text = { content = s } }

type title_object = {
  type_: string [@key "type"];
  title: text_object list [@key "title"];
}
[@@deriving yojson]

let title_object_of_string s = { type_ = "title"; title = [text_object_of_string s] }

type rich_text_object = {
  type_: string [@key "type"];
  rich_text: text_object list [@key "rich_text"];
}
[@@deriving yojson]

let rich_text_object_of_string s = { type_ = "rich_text"; rich_text = [text_object_of_string s] }

type number_object = {
  type_: string [@key "type"];
  number: int [@key "number"];
}
[@@deriving yojson]

let number_object_of_int i = { type_ = "number"; number = i }

type url = {
  url: string option;
}
[@@deriving yojson]

type external_image_object = {
  type_: string [@key "type"];
  external_: url [@key "external"];
}
[@@deriving yojson]

type image_object = {
  type_: string [@key "type"];
  image: external_image_object [@key "image"];
}
[@@deriving yojson]

let image_object_of_url url = { type_ = "image"; image = { type_ = "external"; external_ = { url } } }

type multi_select_option = {
  name: string;
}
[@@deriving yojson]

type multi_select_properties = {
  options: multi_select_option list [@key "options"];
}
[@@deriving yojson]

type multi_select_object = {
  type_: string [@key "type"];
  multi_select: multi_select_option list [@key "multi_select"];
}
[@@deriving yojson]

let multi_select_object_of_string_list l = { type_ = "multi_select"; multi_select = List.map l ~f:(fun s -> { name = s }) }

type file_object = {
  type_: string [@key "type"];
  name: string [@key "name"];
  external_: url [@key "external"];
}
[@@deriving yojson]

type files_object = {
  type_: string [@key "type"];
  files: file_object list [@key "files"];
}
[@@deriving yojson]

let files_object_of_urls urls = { type_ = "files"; files = List.map urls ~f:(fun url -> { type_ = "external"; name = url; external_ = { url = Some url } }) }

type properties = {
  name: title_object [@key "Name"];
  rent_band: rich_text_object [@key "Rent Band"];
  rent: number_object [@key "Rent per term"];
  comments: rich_text_object [@key "Comments"];
  properties: multi_select_object [@key "Properties"];
  building: rich_text_object [@key "Building"];
  address: rich_text_object [@key "Address"];
  site: rich_text_object [@key "Site"];
  images: files_object [@key "Images"];
  booker: url [@key "Booker"];
  reviews: url [@key "Reviews"];
}
[@@deriving yojson]

(*type image_object_wrapped = {
  object_: string [@key "object"] [@default "block"];
  type_: string [@key "type"] [@default "image"];
  image: image_object [@key "body"];
}
[@@deriving yojson]*)

type page = {
  parent: parent;
  properties: properties;
  children: image_object list;
}
[@@deriving yojson]

let notion_rooms_db_id = "f821d368ef3540f28803c1e946c0fb67"


let page_of_room ((room, review): Reviews.room_with_review) = Parse.(
  let parent = { type_ = "database_id"; database_id = notion_rooms_db_id } in
  let properties = {
    name = title_object_of_string room.name;
    rent_band = rich_text_object_of_string room.rentband;
    rent = number_object_of_int room.rent;
    comments = rich_text_object_of_string room.comments;
    properties = multi_select_object_of_string_list room.properties;
    building = rich_text_object_of_string room.building;
    address = rich_text_object_of_string room.address;
    site = rich_text_object_of_string room.site;
    images = files_object_of_urls room.images;
    booker = { url = Some ("https://pet.optime.cloud/#accommodation/details/" ^ (string_of_int room.id)) };
    reviews = { url = Option.map ~f:(fun review -> "https://adj35.user.srcf.net/room-reviews/rooms/" ^ review.name ^ ".html") review};
  } in
  { parent; properties; children = List.map room.images ~f:(fun image -> {  type_ = "image"; image = { type_ = "external"; external_ = { url = Some image } } }) }
)

let notion_api_key = "secret_VnyhYTqmRxKTKDoVP5zKWWL5ss2lDbeViWJIVRRsjAQ"

let upload_room_lwt room =
  let page = page_of_room room in
  let json = page_to_yojson page in
  let json_str = Yojson.Safe.to_string json in
  let res_lwt = Cohttp_lwt_unix.Client.post
    ~headers:(Cohttp.Header.of_list [
        "Authorization", "Bearer " ^ notion_api_key;
        "Content-Type", "application/json";
        "Notion-Version", "2022-06-28";
      ])
    ~body:(Cohttp_lwt.Body.of_string json_str) (Uri.of_string "https://api.notion.com/v1/pages") in
  let open Lwt in
  res_lwt >>= fun (resp, body) ->
  body |> Cohttp_lwt.Body.to_string >|= fun body_str ->
  if Cohttp.Code.is_success (Cohttp.Response.status resp |> Cohttp.Code.code_of_status) then
    ()
  else
    failwith @@ "Failed to upload room: " ^ body_str ^ "\n"

let upload_rooms rooms =
  Lwt_list.iter_s upload_room_lwt rooms |> Lwt_main.run
