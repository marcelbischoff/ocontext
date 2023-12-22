open Jingoo

type keyword_record = {
  keyword : string;
  similarity : float;
  index : int;
  rank : int;
}

let max_search_terms = 10
let ( >>= ) = Option.bind

module StringMap = Map.Make (String)

let load_keyword_records file =
  let json = Yojson.Basic.from_file file in
  let open Yojson.Basic.Util in
  let keywords = json |> member "keyword" |> to_list |> filter_string in
  let similarities = json |> member "similarity" |> to_list |> filter_float in
  let ranks = json |> member "rank" |> to_list |> filter_int in
  let () = assert (List.length keywords = List.length similarities) in
  let () = assert (List.length keywords = List.length ranks) in
  let map_to_kw a b =
    {
      keyword = fst b;
      similarity = fst @@ snd b;
      index = a;
      rank = snd @@ snd b;
    }
  in
  List.combine keywords (List.combine similarities ranks)
  |> List.to_seq |> Seq.mapi map_to_kw |> Array.of_seq

let keyword_records_to_keyword_map keyword_records =
  let imap a = (a.keyword, a) in
  keyword_records |> Array.to_seq |> Seq.map imap |> StringMap.of_seq

let keyword_records = load_keyword_records "data.json"
let keyword_map = keyword_records_to_keyword_map keyword_records

let search search_term =
  let sw = String.lowercase_ascii search_term in
  let extract_kw record = record.keyword in
  let filter_kw kw = String.starts_with kw ~prefix:sw in
  keyword_records |> Array.to_list |> List.to_seq |> Seq.map extract_kw
  |> Seq.filter filter_kw |> Seq.take max_search_terms |> List.of_seq

let code = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789_-"
let length = String.length code

module CharMap = Map.Make (Char)

let char_to_id_map =
  String.to_seq code |> Seq.mapi (fun a b -> (b, a)) |> CharMap.of_seq

let char_to_idx (c : char) = CharMap.find_opt c char_to_id_map
let idx_to_char (i : int) = String.get code i

let encode (i : int) : string option =
  match 0 <= i && i < length * length with
  | false -> None
  | true ->
      let s1 = idx_to_char (i / length) |> Char.escaped in
      let s2 = idx_to_char (i mod length) |> Char.escaped in
      Some (s1 ^ s2)

let encode_all (state_rep : int list) : string =
  state_rep |> List.filter_map encode |> String.concat ""

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

let keyword_to_idx kw =
  let get_index k = Some k.index in
  keyword_map |> StringMap.find_opt kw >>= get_index

let get_current_keywords lst =
  let imap i = Array.get keyword_records i in
  let cmp kw1 kw2 = compare kw2.similarity kw1.similarity in
  lst |> List.map imap |> List.sort_uniq cmp

let decode_state state =
  let decoded = decode_all state in
  let state_rep = List.map Result.to_list decoded |> List.flatten in
  get_current_keywords state_rep

let format keyword_record =
  keyword_record.keyword ^ ": "
  ^ string_of_int keyword_record.rank
  ^ " ("
  ^ Float.to_string keyword_record.similarity
  ^ ")"

let parse_keywords state current search = 
                 let current_keywords =
                   current |> List.map format |> String.concat "<br>"
                 in
                 Jg_template.from_file "templates/keywords.html"
                   ~models:
                     [
                       ("current_keywords", Jg_types.Tstr current_keywords);
                       ("search", Jg_types.Tstr search);
                     ]
  |> Dream.html ~headers:[ ("HX-Push-Url", "/" ^ state) ]

let post state request =
  match%lwt Dream.form ~csrf:false request with
  | `Ok [ ("search", s) ] -> (
      (*
                 Jg_template.from_string "{{ s }}"
                   ~models:[ ("s", Jg_types.Tstr ( s ^ "state=" ^ state)) ]
                 |> Dream.html *)
      let idx =
        match s with
        | "/hint" ->
            let decoded = decode_all state in
            let state_rep = List.map Result.to_list decoded |> List.flatten in
            let current = get_current_keywords state_rep in
            let rank =
              match current with
              | best :: _ when best.rank = 1 -> 1
              | best :: _ -> best.rank / 2
              | _ -> Array.length keyword_records / 2
            in
            keyword_records |> Array.find_opt (fun x -> x.rank = rank)
            >>= fun x -> Some x.index
        | _ -> keyword_to_idx s
      in
      let decoded = decode_all state in
      let state_rep = List.map Result.to_list decoded |> List.flatten in
      match idx with
      | Some i ->
          let new_state = i :: state_rep |> encode_all in
          Dream.redirect request ("/keywords/" ^ new_state)
      | None ->
             let current = decode_state state in
             match current with
             | best :: _ when best.similarity = 1.0 ->
                 Jg_template.from_file "templates/win.html" |> Dream.html
             | _ -> parse_keywords state current "Word not found!"
        )
  | _ -> Dream.empty `Bad_Request

let search state request =
  match%lwt Dream.form ~csrf:false request with
  | `Ok [ ("search", search_term) ] ->
      let active_item search =
        Jg_template.from_file "templates/active.html"
          ~models:
            [ ("search", Jg_types.Tstr search); ("state", Jg_types.Tstr state) ]
      in
      search search_term |> List.map active_item |> String.concat ""
      |> Dream.html
  | _ -> Dream.empty `Bad_Request



let () =
  Dream.run ~interface:"0.0.0.0"
  @@ Dream.logger (*@@ Dream.memory_sessions*)
  @@ Dream.router
       [
         Dream.get "/" (fun _ ->
             Jg_template.from_file "templates/main.html" |> Dream.html);
         Dream.get "/:state" (fun request ->
             let state = Dream.param request "state" in
             Jg_template.from_file "templates/main.html"
               ~models:[ ("state", Jg_types.Tstr state) ]
             |> Dream.html);
         Dream.get "/keywords/:state" (fun request ->
             let state = Dream.param request "state" in
             let current = decode_state state in
             match current with
             | best :: _ when best.similarity = 1.0 ->
                 Jg_template.from_file "templates/win.html" |> Dream.html
             | _ -> parse_keywords state current "");
         Dream.post "/search/" (fun request -> search "" request);
         Dream.post "/search/:state" (fun request ->
             let state = Dream.param request "state" in
             search state request);
         Dream.post "/" (fun request -> post "" request);
         Dream.post "/:state" (fun request ->
             let state = Dream.param request "state" in
             post state request);
         Dream.get "/state/:state" (fun request ->
             let state = Dream.param request "state" in
             let decoded = decode_all state in
             let state_rep =
               List.map Result.to_list decoded
               |> List.flatten |> List.map string_of_int |> String.concat ","
             in
             Dream.html ("state = " ^ state ^ "<br/>decoded = " ^ state_rep));
         Dream.get "dist/**" (Dream.static "dist/.");
       ]
