let read_file file = In_channel.with_open_text file In_channel.input_all
let split_str sep = Str.(split (regexp sep))
;;

let filename = "data/wiki-news-300d-1M.vec" in
let lines = read_file filename |> split_str "\n" in
List.nth lines 0 |> print_endline



