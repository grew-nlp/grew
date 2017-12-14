open Ocamlbuild_plugin

let read file =
  let in_ch = open_in file in
  try
    let v = input_line in_ch in
    close_in in_ch;
    v
  with End_of_file -> failwith ("Error loading file: "^file)

let () =
  dispatch begin function
  | After_rules ->
    let version = "\""^(read "VERSION")^"\"" in
    let pp_src = S[A"-pp"; A("cppo -D 'VERSION "^version^"'")] in
    flag ["ocaml"; "ocamldep"] & pp_src;
    flag ["ocaml"; "compile"] & pp_src;
  | _ -> ()
  end
