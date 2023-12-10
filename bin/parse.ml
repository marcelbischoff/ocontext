let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))

type wordv = {word: string; _vector: float list}

let get_chars (s: string) : char list  =
    let len = String.length s in 
    List.init len Fun.id |> List.map (String.get s)

let is_alpha s = 
    let is_char_alpha (c: char) : bool = 
        match c with 
        | 'a' .. 'z' -> true
        | _ -> false
    in
    get_chars  s|> List.for_all is_char_alpha 

let parse_line (line: string) : wordv option =
    let cols = line |> split_str " " in 
    match cols with
    | word :: vec_str -> let vector = vec_str |> List.map float_of_string in
    if is_alpha word && String.length word > 1 then 
        Some {word; _vector=vector}
    else None
    | _ -> failwith "parse error"
;;

let filename = "data/wiki-news-300d-1M.vec" in
let lines = read_file filename |> split_str "\n" in
match lines with 
| header :: data -> (
    let () = print_endline header in
    data |> List.filter_map parse_line |> List.to_seq |> Seq.take (64*64*64 -1)
    |> Seq.map (fun x -> x.word) |> List.of_seq
    |> String.concat "\n" |> print_endline
)
| _ -> failwith "data invalid"



