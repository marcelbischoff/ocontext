let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))

type record = { word : string; similarity : float; idx : int; rank : int }
type wordv = { word : string; vector : float array }
type intermediate = { adotb : float; adota : float; bdotb : float }

let get_chars (s : string) : char list =
  let len = String.length s in
  List.init len Fun.id |> List.map (String.get s)

let cosine_sim (x : float array) (y : float array) : float =
  let mul (a : float) (b : float) : intermediate =
    { adotb = a *. b; adota = a *. a; bdotb = b *. b }
  in
  let fold (a : intermediate) (b : intermediate) : intermediate =
    {
      adotb = a.adotb +. b.adotb;
      adota = a.adota +. b.adota;
      bdotb = a.bdotb +. b.bdotb;
    }
  in
  let sum v = v.adotb /. Float.sqrt (v.adota *. v.bdotb) in
  Array.map2 mul x y
  |> Array.fold_left fold { adotb = 0.0; adota = 0.0; bdotb = 0.0 }
  |> sum

let is_alpha s =
  let is_char_alpha (c : char) : bool =
    match c with 'a' .. 'z' -> true | _ -> false
  in
  get_chars s |> List.for_all is_char_alpha

let parse_line (line : string) : wordv option =
  let cols = line |> split_str " " in
  match cols with
  | word :: vec_str ->
      let vector = vec_str |> List.map float_of_string |> Array.of_list in
      if is_alpha word && String.length word > 3 && String.length word < 8 then Some { word; vector }
      else None
  | _ -> failwith "parse error"
;;

let filename = "data/wiki-news-300d-1M.vec" in
let lines = read_file filename |> split_str "\n" in
let word = "camel" in
match lines with
| _ :: data ->
    let word_vectors : wordv list =
      data |> List.to_seq |> Seq.filter_map parse_line
      |> Seq.take ((64 * 64 * 64) - 1)
      |> List.of_seq
    in
    let word_vector =
      word_vectors
      |> List.map (fun (x : wordv) -> (x.word, x.vector))
      |> List.assoc word
    in
    let records : record list =
      word_vectors
      |> List.mapi (fun i (x : wordv) ->
             {
               word = x.word;
               similarity = cosine_sim word_vector x.vector;
               idx = i;
               rank = 0;
             })
      |> List.sort (fun a b -> Float.compare b.similarity a.similarity)
      |> List.mapi (fun i x -> { x with rank = i })
      |> List.sort (fun a b -> compare a.idx b.idx)
    in
    let keyword = records |> List.map (fun (x : record) -> x.word) in
    let similarity = records |> List.map (fun x -> x.similarity) in
    let rank = records |> List.map (fun x -> x.rank) in
    let () = print_endline "{\"keyword\": [" in
    let () =
      keyword
      |> List.map (fun w -> "\"" ^ w ^ "\"")
      |> String.concat "," |> print_endline
    in
    let () = print_endline "], \"similarity\": [" in
    let () =
      similarity
      |> List.map (function 1.0 -> "1.0" | x -> string_of_float x)
      |> String.concat "," |> print_endline
    in
    let () = print_endline "], \"rank\": [" in
    let () =
      rank |> List.map string_of_int |> String.concat "," |> print_endline
    in
    print_endline "]}"
| _ -> failwith "data invalid"
