(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                          *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Log
open Conllx
open Libgrew

(* ==================================================================================================== *)
module Int_set = Set.Make (Int)
module Int_map = Map.Make (Int)

let quiet = ref false

(* ==================================================================================================== *)
exception Error of Yojson.Basic.t

let error_ ?file ?line ?fct ?data msg =
  let opt_list = [
    Some ("error", `String msg);
    (CCOption.map (fun x -> ("file", `String x)) file);
    (CCOption.map (fun x -> ("line", `Int x)) line);
    (CCOption.map (fun x -> ("function", `String x)) fct);
    (CCOption.map (fun x -> ("data", x)) data);
  ] in
  let json = `Assoc (CCList.filter_map (fun x->x) opt_list) in
  raise (Error json)

let error ?file ?line ?fct ?data = Printf.ksprintf (error_ ?file ?line ?fct ?data)

(* -------------------------------------------------------------------------------- *)

let fail msg = Log.fmessage "%s" msg; exit 2

let handle fct () =
  try fct ()
  with
  | Error json ->                  fail (Yojson.Basic.pretty_to_string json)
  | Conllx_error json ->           fail (Yojson.Basic.pretty_to_string json)
  | Libgrew.Error msg ->           fail msg
  | Sys_error msg ->               fail (sprintf "System error: %s" msg)
  | Yojson.Json_error msg ->       fail (sprintf "Json error: %s" msg)
  | Libgrew.Bug msg ->             fail (sprintf "Libgrew.bug, please report: %s" msg)
  | exc ->                         fail (sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))

(* ---------------------------------------------------------------------------------------------------- *)
let ensure_dir dir =
  try (* catch if dir does not exist *)
    match Unix.stat dir with
    | { Unix.st_kind = Unix.S_DIR } -> None
    | _ ->  Some (sprintf "grew_match option ignored: %s already exists and is not directory" dir)
  with Unix.Unix_error (Unix.ENOENT,_,_) ->
    begin (* dir does not exist -> try to create it *)
      try Unix.mkdir dir 0o755; None
      with exc -> Some (sprintf "grew_match option ignored: cannot create dir %s (%s)" dir (Printexc.to_string exc))
    end

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
    if not !quiet
    then eprintf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !quiet then eprintf "%s100.00%%\n%!" back
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

(* ==================================================================================================== *)
module Stat = struct
  type pat_desc = {
    id: string;
    desc: string;
    code: string list;
  }

  type t = pat_desc list

  let load_json json_file =
    let open Yojson.Basic.Util in

    let json =
      try Yojson.Basic.from_file json_file
      with Yojson.Json_error msg -> error ~fct:"Stat.load_json" ~file:json_file "%s" msg in

      let parse_pattern (id, json) =
        let assoc = json |> to_assoc in
      { id;
        desc = List.assoc "desc" assoc |> to_string;
        code = List.assoc "code" assoc |> to_list |> List.map to_string;
      } in

      (json |> to_assoc |> List.map parse_pattern, json)
end


(* ==================================================================================================== *)
module Validation = struct
  type item = {
    pattern: string list;
    description: string;
    level: string;
  }

  type modul = {
    title: string;
    items: item list
  }

  (* -------------------------------------------------------------------------------- *)
  let load_json json_file =
    let open Yojson.Basic.Util in

    let json =
      try Yojson.Basic.from_file json_file
      with Yojson.Json_error msg -> error ~fct:"Validation.load_json" ~file:json_file "%s" msg in

    let parse_one json =
      let pattern =
        try json |> member "pattern" |> to_string |> (fun x -> [x])
        with Type_error _ ->
        try json
            |> member "pattern"
            |> to_list
            |> (List.map to_string)
        with Type_error (json_error,_) ->
          error
            ~fct:"Validation.load_json"
            ~file: json_file
            "\"pattern\" field is mandatory and must be a string or a list of strings (%s)" json_error in
      let description =
        try json |> member "description" |> to_string
        with Type_error _ -> "No description" in
      let level =
        try json |> member "level" |> to_string
        with Type_error _ -> "No level" in

      { pattern; description; level } in

    let title =
      try json |> member "title" |> to_string
      with Type_error (json_error,_) ->
        error
          ~fct:"Validation.load_json"
          ~file: json_file
          "\"title\" field is mandatory and must be a string or a list of strings (%s)" json_error in
    let items = List.map parse_one (json |> member "items" |> to_list) in

    { title; items }


  (* -------------------------------------------------------------------------------- *)
  let check ?dir modul_list (corpus_desc:Corpus_desc.t) =
    let corpus = Corpus_desc.build_corpus corpus_desc in
    let config = Corpus_desc.get_config corpus_desc in

    let date =
      let tm = Unix.localtime (Unix.time ()) in
      sprintf "%d/%02d/%02d - %02d:%02d"
        (tm.Unix.tm_year + 1900) (tm.Unix.tm_mon + 1) tm.Unix.tm_mday
        tm.Unix.tm_hour tm.Unix.tm_min in

    let modules =
      `List
        (List.map
           (fun modul ->
              let (out_items : Yojson.Basic.t) =
                `List
                  (List.map
                     (fun item ->
                        let grew_pattern =
                          try Pattern.parse ~config (String.concat " " item.pattern)
                          with Libgrew.Error msg ->
                            error
                              ~fct:"Validation.check"
                              ~data:(`String (String.concat " " item.pattern))
                              "cannot parse pattern associated with desc: %s" item.description
                        in
                        let count =
                          Corpus.fold_left (fun acc _ graph ->
                              acc + (List.length (Graph.search_pattern ~config grew_pattern graph))
                            ) 0 corpus in
                        `Assoc [
                          "count", `Int count;
                          "pattern", `List (List.map (fun x -> `String x) item.pattern);
                          "description", `String item.description;
                          "level", `String item.level
                        ]
                     ) modul.items) in
              `Assoc ["title", `String modul.title; "items", out_items]
           ) modul_list) in

    let json = `Assoc [
        "corpus", `String (Corpus_desc.get_id corpus_desc);
        "date", `String date;
        "modules", modules
      ] in

    match dir with
    | None -> printf "%s\n" (Yojson.Basic.pretty_to_string json)
    | Some dir ->
      let out_file = Filename.concat dir ((Corpus_desc.get_id corpus_desc) ^ ".json") in
      CCIO.with_out out_file (fun out_ch -> fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string json))

end