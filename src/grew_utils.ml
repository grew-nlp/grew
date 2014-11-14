(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Log

open Grew_args

module StringMap = Map.Make (String)

module Array_ = struct
  (* dichotomic search in a sorted array *)
  let dicho_find elt array =
    let rec loop low high =
      (if low > high then raise Not_found);
      match (low+high)/2 with
      | middle when array.(middle) = elt -> middle
      | middle when array.(middle) < elt -> loop (middle+1) high
      | middle -> loop low (middle - 1) in
    loop 0 ((Array.length array) - 1)
end

module Counter = struct
  let back = sprintf "\r%s\r" (String.make 100 ' ')

  let print value total text =
    if not !Grew_args.quiet
    then printf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !Grew_args.quiet then printf "%s100.00%%\n%!" back
end

module Grew_utils = struct
  let svg_file_from_dot dot =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    let svg_file_name = Str.global_replace (Str.regexp ".dot") ".svg" temp_file_name in
    ignore (Sys.command(sprintf "dot -Tsvg -o %s %s " svg_file_name temp_file_name));
    svg_file_name

  let save_pdf_dot dot output_file =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    ignore (Sys.command(sprintf "dot -Tpdf -o %s %s " output_file temp_file_name))

  let save_svg_dot dot output_file =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    ignore (Sys.command(sprintf "dot -Tsvg -o %s %s " output_file temp_file_name))

IFDEF DEP2PICT THEN
  let svg_file_from_dep dep =
    let temp_file_name = Filename.temp_file "grew_" ".svg" in
    ignore (Dep2pict.Dep2pict.fromDepStringToSvgFile dep temp_file_name);
    temp_file_name
ELSE
  let svg_file_from_dep dep =
    Log.critical "[svg_file_from_dep] is not available without dep2pict"
END

  let string_of_loc = function
    | None -> ""
    | Some (file,line) -> sprintf "\nfile : %s\nline : %d" file line

  let read_file file =
    let in_ch = open_in file in
    (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
    (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

    let res = ref [] in
    try
      while true
      do res := (input_line in_ch) :: !res
      done; assert false
    with End_of_file -> close_in in_ch; List.rev !res

  let list_index elt l =
    let rec loop i = function
      | [] -> None
      | e::t when e=elt -> Some i
      | _::t -> loop (i+1) t
    in loop 0 l
end

