let ( $ ) f x = f x
let ( |> ) x f = f x
let ( >> ) f g x = f (g x)

module String =
struct
  include String

  let implode sep l =
    let buflen = List.map length l |> List.fold_left (+) 0 in
    let buf = Buffer.create buflen in
    let rec loop = function
      | [] -> Buffer.contents buf
      | [x] -> Buffer.add_string buf x; Buffer.contents buf
      | x::xs -> 
	Buffer.add_string buf x; Buffer.add_string buf sep; loop xs in
    loop l

  let endswith s1 s2 =
    let s2_len = length s2 in
    length s1 >= s2_len && (sub s1 (length s1 - s2_len) s2_len) = s2
end
