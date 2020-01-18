(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.fr                                          *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Log
open Conll
open Libgrew

open Grew_args

(* ================================================================================ *)
module StringMap = Map.Make (String)

(* ================================================================================ *)
module String_ = struct
  let contains sub str =
    try let _ = Str.search_forward (Str.regexp_string sub) str 0 in true
    with Not_found -> false
end

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
    then eprintf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !Grew_args.quiet then eprintf "%s100.00%%\n%!" back
end (* module Counter *)

(* ================================================================================ *)
module File = struct
  let read_rev file =
    let in_ch = open_in file in
    let line_num = ref 0 in
    let res = ref [] in
    try

      (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
      (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

      while true do
        incr line_num;
        res := (!line_num, input_line in_ch) :: !res
      done; assert false
    with End_of_file -> close_in in_ch; !res

  let read file = List.rev (read_rev file)

  exception Found of int
  let get_suffix file_name =
  let len = String.length file_name in
    try
      for i = len-1 downto 0 do
        if file_name.[i] = '.'
        then raise (Found i)
      done;
      None
    with
    | Found i -> Some (String.sub file_name i (len-i))

  let read_stdin () =
    let cpt = ref 0 in
    let res = ref [] in
    try
      while true do
        incr cpt;
        res := (!cpt, input_line stdin) :: !res
      done;
    assert false
  with End_of_file -> List.rev !res

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

  let rec opt_map fct = function
    | [] -> []
    | h::t ->
  match (fct h) with
  | Some x -> x::(opt_map fct t)
  | None -> opt_map fct t

  let opt_mapi fct =
    let rec loop i = function
      | [] -> []
      | h::t ->
    match fct i h with
    | Some x -> x::(loop (i+1) t)
    | None -> loop (i+1) t
    in loop 0

end (* module List_ *)

(* ================================================================================ *)
module Corpus = struct

  exception Fail of string
  exception File_not_found of string

  let load_conll ?domain file =
    let conll_corpus = Conll_corpus.load file in
    Array.map (fun (sentid, conll) -> (sentid, Graph.of_conll ?domain conll)) conll_corpus

  let brown_form_lines ?domain lines =
    let brown_list =
      List_.opt_map
        (fun (i,line) -> match Str.split (Str.regexp "#") line with
          | [] -> None
          | [line] -> let sentid = sprintf "%05d" i in Some (sentid, Graph.of_brown ?domain ~sentid line)
          | [sentid; line] -> Some (sentid, Graph.of_brown ?domain ~sentid line)
          | _ -> raise (Fail (sprintf "[line %d] Illegal Brown line >>>%s<<<<\n%!" i line))
        ) lines in
      Array.of_list brown_list

  let load_brown ?domain file =
    let lines = File.read file in
    try brown_form_lines ?domain lines
    with Fail msg -> raise (Fail (sprintf "[file %s] %s" file msg))

  (** [load source] loads a corpus; [source] can be:
      - a folder, the corpus is the set of graphs (files matching *.gr or *.conll) in the folder
      - a conll file *)
  let get_graphs ?domain source_list =
    match source_list with
    | [source] ->
    begin
    if not (Sys.file_exists source)
    then raise (File_not_found source);
    if Sys.is_directory source
    then (* if [source] is a folder *)
      begin
        let files_array = Sys.readdir source in
        let graph_list =
        Array.fold_right
          (fun file acc ->
            if Filename.check_suffix file ".gr"
            then (Filename.chop_extension file, Graph.load ?domain (Filename.concat source file)) :: acc
            else if (Filename.check_suffix file ".conll" || Filename.check_suffix file ".conllu")
            then
              let conll = Conll.load (Filename.concat source file) in
              let graph = Graph.of_conll ?domain conll in
              match Conll.get_sentid conll with
              | Some sentid -> (sentid, graph) :: acc
              | None -> (file, graph) :: acc
            else acc
          ) files_array [] in
          Array.of_list graph_list
      end
    else (* if [source] is a file *)

      match File.get_suffix source with
      | Some s when String_.contains "conll" s -> load_conll ?domain source
      | Some s when String_.contains "cupt" s -> load_conll ?domain source
      | Some s when String_.contains "melt" s -> load_brown ?domain source
      | Some s when String_.contains "brown" s -> load_brown ?domain source
      | Some s when String_.contains "gr" s -> [| (source, Graph.load ?domain source) |]

      | _ ->
        Log.fwarning "Unknown suffix for file \"%s\", trying to guess format..." source;
        try load_conll ?domain source
          with _ ->
          try load_brown ?domain source
          with _ -> raise (Fail (sprintf "Cannot load file \"%s\", unknown format" source))

    end
    | [] -> raise (Fail ( "Empty input list\n%!"))
    | _ ->
      let conll_corpus = Conll_corpus.load_list source_list in
      Array.map (fun (sentid, conll) -> (sentid, Graph.of_conll ?domain conll)) conll_corpus

  let from_stdin () =
    let lines = File.read_stdin () in
    try
      let conll_corpus = Conll_corpus.from_lines ~basename: "stdin" lines in
      Array.map (fun (sentid, conll) -> (sentid, Graph.of_conll conll)) conll_corpus
    with _ -> brown_form_lines lines

  let input ?domain () =
    match !Grew_args.input_data with
    | [] -> from_stdin ()
    | input_list -> get_graphs ?domain input_list

end (* module Corpus *)

(* ==================================================================================================== *)
module Int =
  struct
    type t = int
    let compare = Stdlib.compare
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
