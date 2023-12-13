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

let search search_term =
  let sw = String.lowercase_ascii search_term in
  let extract_kw record = record.keyword in
  let filter_kw kw = String.starts_with kw ~prefix:sw in
  keyword_records |> Array.to_list |> List.to_seq |> Seq.map extract_kw
  |> Seq.filter filter_kw |> Seq.take max_search_terms |> List.of_seq

let keyword_map = keyword_records_to_keyword_map keyword_records

let current_keywords lst =
  let imap i = Array.get keyword_records i in
  let cmp kw1 kw2 = compare kw2.similarity kw1.similarity in
  lst |> List.map imap |> List.sort cmp

let keyword_to_idx kw =
  let get_index k = Some k.index in
  keyword_map |> StringMap.find_opt kw >>= get_index

let next_prompt state =
  let () = print_string "guess the word: " in
  let kw = read_line () in
  let idx = keyword_to_idx kw in
  match idx with
  | Some i -> i :: state
  | None ->
      let suggestions = search kw |> String.concat "\n" in
      let () = print_endline "word does not exist, try" in
      let () = print_endline suggestions in
      state

let format keyword_record =
  keyword_record.keyword ^ ": "
  ^ string_of_int keyword_record.rank
  ^ " ("
  ^ Float.to_string keyword_record.similarity
  ^ ")"

let rec play state =
  let new_state = state |> next_prompt in
  let current = current_keywords new_state in
  let () = current |> List.map format |> String.concat "\n" |> print_endline in
  match current with
  | [] -> play []
  | best :: _ -> (
      match best.similarity with
      | 1.0 -> print_endline "you won!"
      | _ -> play new_state)
;;

play []
