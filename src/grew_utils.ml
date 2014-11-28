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

(* ================================================================================ *)
module StringMap = Map.Make (String)

(* ================================================================================ *)
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

(* ================================================================================ *)
module Counter = struct
  let back = sprintf "\r%s\r" (String.make 100 ' ')

  let print value total text =
    if not !Grew_args.quiet
    then printf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !Grew_args.quiet then printf "%s100.00%%\n%!" back
end (* module Counter *)

(* ================================================================================ *)
module File = struct
  let read file =
    let in_ch = open_in file in
    (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
    (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

    let res = ref [] in
    try
      while true
      do res := (input_line in_ch) :: !res
      done; assert false
    with End_of_file -> close_in in_ch; List.rev !res
end (* module File *)

(* ================================================================================ *)
module List_ = struct
  (** [index elt l] returns Some i where i is the index of the first occurence of [elt] in [l].
      None is returned if [l] does not contain [elt]. *)
  let index elt l =
    let rec loop i = function
      | [] -> None
      | e::t when e=elt -> Some i
      | _::t -> loop (i+1) t
    in loop 0 l

  let iteri fct =
    let rec loop i = function
      | [] -> ()
      | h::t -> (fct i h); (loop (i+1) t) in
    loop 0

  let mapi fct =
    let rec loop i = function
      | [] -> []
      | h::t -> let head = fct i h in head :: (loop (i+1) t)
    in loop 0

end (* module List_ *)

(* ================================================================================ *)
module Html = struct
  let build_header prev_opt next_opt =
    match prev_opt, next_opt with
      | None, None -> ""
      | Some p, None -> sprintf "<a href=\"%s.html\">Previous</a>" p;
      | None, Some n -> sprintf "<a href=\"%s.html\">Next</a>" n;
      | Some p, Some n -> sprintf "<a href=\"%s.html\">Previous</a> -- <a href=\"%s.html\">Next</a>" p n


  let write_error ?(header="") ~html ?init basename msg =
    let stat_file = sprintf "%s.stat" basename in

    let out_ch = open_out stat_file in
    fprintf out_ch "ERROR\n";
    fprintf out_ch "%s" msg;
    close_out out_ch;

    if html
    then Libgrew.error_html
      ~no_init: !Grew_args.no_init
      ?main_feat: !Grew_args.main_feat
      ~dot: !Grew_args.dot
      ~header
      msg
      ?init
      basename
end (* module Html *)


(* ================================================================================ *)
module Pdf = struct
  let dot_to_file dot output_file =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    ignore (Sys.command(sprintf "dot -Tpdf -o %s %s " output_file temp_file_name))
end (* module Pdf *)

(* ================================================================================ *)
module Svg = struct
  let dot_to_tmp dot =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    let svg_file_name = Str.global_replace (Str.regexp ".dot") ".svg" temp_file_name in
    ignore (Sys.command(sprintf "dot -Tsvg -o %s %s " svg_file_name temp_file_name));
    svg_file_name

  let dot_to_file dot output_file =
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".dot" in
    fprintf out_ch "%s" dot;
    close_out out_ch;
    ignore (Sys.command(sprintf "dot -Tsvg -o %s %s " output_file temp_file_name))

IFDEF DEP2PICT THEN
  let dep_to_tmp dep =
    let temp_file_name = Filename.temp_file "grew_" ".svg" in
    ignore (Dep2pict.Dep2pict.fromDepStringToSvgFile dep temp_file_name);
    temp_file_name
ELSE
  let dep_to_tmp dep =
    Log.critical "[Svg.dep_to_tmp] is not available without dep2pict"
END
end (* module Svg *)

(* ================================================================================ *)
module Corpus = struct
  (** [load_conll file] load a corpus. It retuns an array of couples: (id: string, graph:Instance.t).
      The identifier is the one described by the sentid feature of the first line of the conll desc;
      If no setinf is found, the identifier is the name of the file with a 5 digits number for the position in the file. *)
  let load_conll file =
    let base = Filename.chop_extension (Filename.basename file) in
    let in_ch = open_in file in
    (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
    (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

    let cpt = ref 0 in
    let name = ref None in
    let res = ref [] in
    let last = ref [] in
    let line_num = ref 0 in

    let save_one () =
      match !name with
        | None -> ()
        | Some n ->
          res := (n, Libgrew.of_conll file (List.rev !last)) :: !res;
          last := [];
          name := None in
    try
      while true do
        incr line_num;
        match (!name, input_line in_ch) with
          | (_, "") -> save_one ()
          | Some oc, line -> last :=  (!line_num, line) :: !last
          | None, line ->
            (* first line of the conll, build the name of the graph (sentid or file_xxxxx) *)
            (match Str.split (Str.regexp "\t") line with
              | [_;_;_;_;_;fs_string;_;_;_;_] ->
                begin
                  let fs = List.map
                    (fun feat_string ->
                      match Str.split (Str.regexp "=") feat_string with
                        | [name;value] -> (name,value)
                        | _ -> failwith (Printf.sprintf "#1 >>%S<<\n%!" feat_string)
                  ) (Str.split (Str.regexp "|") fs_string) in
                  try name := Some (List.assoc "sentid" fs)
                  with Not_found -> name := Some (sprintf "%s_%05d" base !cpt)
                end;
                incr cpt;
                last :=  (!line_num, line) :: !last
              | _ -> Printf.printf "Illegal Conll line >>>%s<<<<\n%!" line; exit 3
            )
      done; assert false

    with End_of_file ->
      save_one ();
      List.rev !res



  (** [load source] loads a corpus; [source] can be:
      - a folder, the corpus is the set of graphs (files matching *.gr or *.conll) in the folder
      - a conll file *)
  let get_graphs source =
    if Sys.is_directory source
    then (* if [source] is a folder *)
      begin
        let files_array = Sys.readdir source in
        Array.fold_right
          (fun file acc ->
            if Filename.check_suffix file ".gr"
            then (Filename.chop_extension file, Libgrew.load_graph (Filename.concat source file)) :: acc
            else if Filename.check_suffix file ".conll"
            then (load_conll (Filename.concat source file)) @ acc
            else acc
          ) files_array []
      end
    else (* if [source] is a file *) load_conll source


end (* module Corpus *)

(* ==================================================================================================== *)
module Int =
  struct
    type t = int
    let compare = Pervasives.compare
  end

module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

(* ==================================================================================================== *)
module Timer = struct
  type t = int

  let cpt = ref 0

  let table = ref Int_map.empty

  let create () =
    incr cpt;
    let current_time = Unix.times () in
    table := Int_map.add !cpt current_time.Unix.tms_utime !table;
    !cpt

  let see timer =
    let current_time = Unix.times () in
    current_time.Unix.tms_utime -. (Int_map.find timer !table)

  let get timer =
    let current_time = Unix.times () in
    let diff = current_time.Unix.tms_utime -. (Int_map.find timer !table) in
    table := Int_map.remove !cpt !table;
    diff
end
