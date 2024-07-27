open Load_rooms.Parse
(*open Load_rooms.Upload*)
open Load_rooms.Reviews

let api_uri =
  Uri.of_string "https://pet.optime.cloud/api/booker/search/accommodation"

let auth_key = "CfDJ8NFl9MkPRlhNqN-IcGfomkgaMu65O7hn-Lc8Dd53-EnYb7Crr5pAha0jx5F4mymKdpT1D1jJxSb_-ZmdIh6LeZ6QzX9dVzhSOQiipzMLT6vTWP15eoxqHZSsmnttGOnSKLpj1TmoRjXscmHZKzSrGDGHFtmgFjtaPdp0whYIVB3iGFfAI9kaFjKW8HhSUkSa4xRo_xBXfS6gLvmvOe7oyad9xgBkgOSe_iRa--74uyROnijQijoXFhk3-n1tYyE1yD9sigHkiisdKwltpb6Kul7frnaqAtnbIvJrwAZ1SbjfkMA5LNt_ixDgnH1UkOc4CVeJAZYlft4D5Wy1c1J86YPDhZ0LkeKFcZVgnCOapDnvCtHdTN_zXRUrrL-okiQnyWNKNDbeRFq5rILBx_gzLFlTxPn_UL0e0hqNW28DE1VxTegythZttbAeqg8xJbccaQQht_VJfoqabeypKULdzNV-VtwDJjd4qb8zByqw1ZMvvYarZrmZQBGhAl2qzt_a8Qn8vCbtn4TWtrGrFQoaMRXReO_qkAC_mE-0KA-pK1xWHYhLWAv_hR4I8YQY2GjuxSfnsmgRwdbCnKso64CFJx5nYkNsIwg1btaL9SVZWBM6GBfGtF-HZ_YXP57y2ZZroBdcCx9CJKgUFeXd44BPxkPgru5yqA8uAqcBeq9HqnNUZcvM4oajH5OFtoqrzAtb0aqUzIL07XzE0IAIrtyENOnLImmcjwkgYyJEpW03sz-NEBeDQC4YIMXE8wg69wk8z2LPbx5EEAYkkbn6squfeXrSlR1N2kI0GGz-uVl8f3QuFa_UxbHJC7HHs7DaJQqEW08fIkisz0UlDxKwT5RJ00le-1hSD8D3J92l4fhlILk3QH_jGy0qVk_H2B723GQwn9Qj7jvv6TdG1C6YZIg0oSkskfcnMNDWDTgH5TtLQrdJ_JrbuxfXah2fOmOD4SvG5IHobYDqLTBshnhr-jFOKak"


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
