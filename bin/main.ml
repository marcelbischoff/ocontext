open Jingoo

type keyword_record = { keyword : string; _similarity : float; _index : int }

let max_search_terms = 10
let ( >>= ) = Option.bind

module StringMap = Map.Make (String)

let load_keyword_records file =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let keywords = json |> member "keyword" |> to_list |> filter_string in
  let similarities = json |> member "similarity" |> to_list |> filter_float in
  let map_to_kw a b = { keyword = fst b; _similarity = snd b; _index = a } in
  List.combine keywords similarities
  |> List.to_seq |> Seq.mapi map_to_kw |> Array.of_seq

let keyword_records_to_keyword_map keyword_records =
  let imap a = (a.keyword, a) in
  keyword_records |> Array.to_seq |> Seq.map imap |> StringMap.of_seq

let keyword_records = load_keyword_records "data.json"
let _keyword_map = keyword_records_to_keyword_map keyword_records

let search search_term =
  let sw = String.lowercase_ascii search_term in
  let extract_kw record = record.keyword in
  let filter_kw kw = String.starts_with kw ~prefix:sw in
  keyword_records |> Array.to_list |> List.to_seq |> Seq.map extract_kw
  |> Seq.filter filter_kw |> Seq.take max_search_terms |> List.of_seq

let code = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890_-"
let length = String.length code

module CharMap = Map.Make (Char)

let char_to_id_map =
  String.to_seq code |> Seq.mapi (fun a b -> (b, a)) |> CharMap.of_seq

let char_to_idx (c : char) = CharMap.find_opt c char_to_id_map

let decode (s : string) =
  let a = char_to_idx (String.get s 0) in
  let b = char_to_idx (String.get s 1) in
  Option.to_result ~none:(Error "not decodable")
    ( a >>= fun i ->
      b >>= fun j -> Some ((length * i) + j) )

let decode_all s =
  let rec aux lst is =
    let ilen = String.length is in
    match is with
    | "" -> lst
    | _ ->
        let chunk = String.sub is 0 2 in
        let remainder = String.sub is 2 (ilen - 2) in
        let decoded = decode chunk :: lst in
        aux decoded remainder
  in
  List.rev (aux [] s)

let () =
  Dream.run @@ Dream.logger (*@@ Dream.memory_sessions*)
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Jg_template.from_file "templates/main.html" |> Dream.html);
         Dream.post "/search" (fun request ->
             match%lwt Dream.form ~csrf:false request with
             | `Ok [ ("search", search_term) ] ->
                 let active_item search =
                   Jg_template.from_file "templates/active.html"
                     ~models:[ ("search", Jg_types.Tstr search) ]
                 in
                 search search_term |> List.map active_item |> String.concat ""
                 |> Dream.html
             | _ -> Dream.empty `Bad_Request);
         Dream.post "/" (fun request ->
             match%lwt Dream.form ~csrf:false request with
             | `Ok [ ("search", s) ] ->
                 Jg_template.from_string "{{ s }}"
                   ~models:[ ("s", Jg_types.Tstr s) ]
                 |> Dream.html
             | _ -> Dream.empty `Bad_Request);
         Dream.get "/state/:state" (fun request ->
             let state = Dream.param request "state" in
             let decoded = decode_all state in
             let state_rep =
               List.map Result.to_list decoded
               |> List.flatten |> List.map string_of_int |> String.concat ","
             in
             Dream.html ("state = " ^ state ^ "<br/>decoded = " ^ state_rep));
       ]
