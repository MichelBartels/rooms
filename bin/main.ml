open Load_rooms.Parse
open Load_rooms.Upload
open Load_rooms.Reviews

let api_uri =
  "https://pet.optime.cloud/api/booker/search/getRoomDetails/"

let auth_key = "CfDJ8NFl9MkPRlhNqN-IcGfomkjU8wOzX1lwY7CbELHfzukcgoy3HfOOeCPoc2UTX9hL-mD_yyNUSHM8Wv4uAAzzKnp-jz5hsWES-EkN7QSl5h9gzRpHCw3Fo9RhDM42RH0PP_s0YP1O3C69ldG4OHR6j6yYiEVDPOSoTy5dUPbTRHaT3sstTJbp93JUrQsaz5r9MeRTsKvQr8BmNHzT3EVSZoMmueM48NasBzCCSWBqyEAq1z-uCb-Zg5UtCaQd97pc3pdNpcUFG0k0ZiY0_1vp6FODZQRcxEBPeW3FD7RFPwpm8dNXo49qMV2IzVykavu2ccXK9fK1lPjnXw92d_hy3IjdnNDyzmRYJq6HjOGy37-BwzxiW0wWNZp5vIIOBmgP0JR_qM3beEDcV6yoEB0OXQTfX6X_cDy1UvKqGZSy-vwprfA5aJID1ivfNbFEJrXTuBiHZV6zcXDm-nrRarWnLxgb1qHN-bVUka9lX761m7WbZTZVAVNvxSBg4ifSru-PP1dWzfH0n3Rt2c0GBb3rJRnEVxnVv5vSwRqaya3uQU9B4B8SPmEz99Cm1OiK8EKqV4mgK_Z35oHjPfcKNgiQHJ1d4RdehjASsYqFLpgDCibHxB6r3B0iWesOWLdAPGJD0Ylx9m4nmuvCA-SM-5O3ohSq4Z07KfzqjYns6wG7FsbHSJNAY8Kb2ojJe3VU86JYkH1HHEcGxaHggRaHrc08lIyLNJmSqwm4iX_2UjRRH2zsqrk7nUxK5eMV0dpkrikWOerMdR28v8_sfOvJXOhrllvJguhLqjn6gMBsUW81UVUG3mW6L-XLFyBf7ebspyc2aHY02wWyVgnxJLuNcuo8LEIBdfTwEmpfpCnRep88mCJRFi5b9mIZNGK3SsYFIRi2O_Fmz90RstM6Zhxw_pGoshQulHZz29q593EkLEQy64lkbvLFoGmIzh2fMzGFUtyYxO8o177NEidzLMpfoZMlqnY" (* you can get the key by inspecting the headers when you're on Booker *)


let header =
  let open Cohttp.Header in
  add (init ()) "Cookie" @@ "osc_auth=" ^ auth_key

(* let params = [("StudentID", ["3055"])] *)

let get i =
  let inner_async =
    let open Lwt in
    Cohttp_lwt_unix.Client.get ~headers:header (Uri.of_string (api_uri ^ (string_of_int i))) >>= fun (_ , body) ->
    body |> Cohttp_lwt.Body.to_string >|= fun str ->
    str in
  Lwt_main.run inner_async

let indices =
  let rec inner i aux =
    if i == 1415 then aux else inner (i + 1) (i::aux) in
  inner 765 []


let rooms = indices |> List.map (fun i -> print_int i; print_endline ""; get i) |> List.filter_map parse_individual_room
            |> matches |> List.map (fun (room, review) -> (room.Load_rooms.Parse.name, (room, review)))
let () = List.filter_map (fun room -> let room_ = List.assoc_opt room rooms in if Option.is_none room_ then print_endline room else (); room_) Load_rooms.Filter.rooms |> upload_rooms
