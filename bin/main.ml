open Load_rooms.Parse
(*open Load_rooms.Upload*)
open Load_rooms.Reviews

let api_uri =
  Uri.of_string "https://pet.optime.cloud/api/booker/search/accommodation"

let auth_key = "<INSERT KEY here>" (* you can get the key by inspecting the headers when you're on Booker *)


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

let () = get |> parse |> matches |> List.length |> print_int
