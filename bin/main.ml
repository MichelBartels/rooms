open Load_rooms.Parse
open Load_rooms.Upload
open Load_rooms.Reviews

let api_uri =
  Uri.of_string "https://pet.optime.cloud/api/booker/search/accommodation"

let auth_key = "" (* you can get the key by inspecting the headers when you're on Booker *)


let header =
  let open Cohttp.Header in
  add (init ()) "Cookie" @@ "osc_auth=" ^ auth_key

let params = [("StudentID", ["3055"])]

let get =
  let inner_async =
    let open Lwt in
    Cohttp_lwt_unix.Client.post_form ~headers:header ~params:params api_uri >>= fun (_ , body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun str ->
    str in
  Lwt_main.run inner_async

let rooms = get |> parse |> matches |> List.map (fun (room, review) -> (room.Load_rooms.Parse.name, (room, review)))
let () = List.filter_map (fun room -> let room_ = List.assoc_opt room rooms in if Option.is_none room_ then print_endline room else (); room_) Load_rooms.Filter.rooms |> upload_rooms
