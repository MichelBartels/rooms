open Load_rooms.Parse
open Load_rooms.Db
open Core

let api_uri =
  Uri.of_string "https://pet.optime.cloud/api/booker/search/accommodation"

let auth_key = "CfDJ8NFl9MkPRlhNqN-IcGfomkiC-7r_6gSVz-4ctp4jlRWY8hNSXJ3bt-T0lwtXRoE4nZkbPVTIBzWsT0fNzTauKV_lBB1HHvD6KQN47KIyx0_TuM3wghpjufplCLEW1-ki0AVoJ53Np7_AzrqGPfsIjXJOEtTRXFoUxDf1y52n1T6_iPx-FhRTVwgEPq06HSNCNH51fZ04avOnVGBq96jxLkL1kx0vcsszFDUVVwpNu4IMyn6KIcuq7GxfX1q8fifqg5u2TVHaaUt4HmrzYJChOAOR3kUJlmreTdxKYND5TvJl-WHb69Uk6GoqMe4PAr8fZu8Nx5XNPdToZen6ERVtSqOFTudqav-L3iKwnjBeN1ijU4mlKWR6mGB0xzqbqOe3RsP65GVkS0vT1W8znHMhWgVYw6R7amC4L10rkHVJkJA5BEBh_fx8vCwnW3lKj4OjgA4_nfuQhkQyiIK0zwQZTK4FqUIeLfHtbhSedO9fvi6Zjfb4fEM6BsR1QJcfNJH1ErSHNTK_yWQAMcPssV58b-V_z2QNct4peKBJaBP-ULpJ7WEXU36Nid4hFKemSxFj8neYEekcpw-iz3vgI61Bqs4I6IfaFVe5aoeK5fAPQ3vL67aVBpYHhd3XzHU37V2LOKxeBi4qWC9_QaGhUM8QU6JM2VoksH8hkv7jYHCTAchyo3_RqGPLVIgOZ2M_6YC9MpIGgDiIA1RaZn5se_r45fBrE3CPcbsm6bK78be0ZT5f4DBpBuV15_nkAjFp0M1Ps99TYwp3cb8D_C056ySmufIKZhg9TnhABuBnprp6p-veygT7H06UZ_3KG9C_XTz41w-Gxyk5tWKXdZWJRkGIXtNPeHfsyT8Q1rd6id9TCRm_T9btjM5j-RaM9EFo6CYj0xZ9_kXQjseRfRtcq_hZkASBJsKgRs_oLb33kOklBkV45rMGJ0l3yTneZHndTgqBo_MsDSc-cyv5EXD4MA2G5js"


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

let insert resp =
  let inserted = Int.Hash_set.create () in
  let rec insert_properties = function
    | {id; description; notes}::properties ->
        if Hash_set.exists inserted ~f:(fun id' -> id' = id) then () 
        else (Hash_set.add inserted id;
        let open Property in
        add {id; description; notes});
        insert_properties properties
    | [] -> () in
  let insert_charge {id; description; price} =
    if Hash_set.exists inserted ~f:(fun id' -> id' = id) then ()
    else (Hash_set.add inserted id;
    let open Charge in
    add {id; description; price=(int_of_float price)}) in
  let insert_building {id; description; address; lat; long; site} =
    if Hash_set.exists inserted ~f:(fun id' -> id' = id) then ()
    else (Hash_set.add inserted id;
    let open Building in
    add {id; description; address; latitude=(float_of_string lat); longitude=(float_of_string long); site=site.description}) in
  let rec insert_equipments room_id = function
    | equipment::equipments ->
        let open Equipment in
        add {room_id; description=equipment};
        insert_equipments room_id equipments
    | [] -> () in
  let rec insert_images room_id = function
    | image::images ->
        let open Image in
        add {room_id; image};
        insert_images room_id images
    | [] -> () in
  let rec insert_rooms = function
    | {id; building; description; room_type; charge; comments; properties; equipment; images}::rooms ->
        let open Room in
        add {id; building_id=building.id; description; charge_id=charge.id; comments; room_type=room_type.description};
        insert_properties properties;
        insert_charge charge;
        insert_building building;
        insert_equipments id equipment;
        insert_images id images;
        insert_rooms rooms
    | [] -> () in
  insert_rooms resp.rooms

let () =
  create;
  get |> parse |> insert
