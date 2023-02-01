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
open Conll
open Grewlib

let quiet = ref false

(* ==================================================================================================== *)
module Log = struct
  let warning_ message =
    ANSITerminal.eprintf [ANSITerminal.blue] "WARNING: %s\n" message;
    Printf.eprintf "%!" (* force synchronous printing *)

  let warning message = Printf.ksprintf warning_ message

  let fail_ message =
    ANSITerminal.eprintf [ANSITerminal.red] "FAIL: %s\n" message;
    Printf.eprintf "%!" (* force synchronous printing *);
    exit 1

  let fail message = Printf.ksprintf fail_ message
end

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

let handle fct () =
  try fct ()
  with
  | Error json ->                  Log.fail "%s" (Yojson.Basic.pretty_to_string json)
  | Conll_error json ->           Log.fail "%s" (Yojson.Basic.pretty_to_string json)
  | Grewlib.Error msg ->           Log.fail "%s" msg
  | Sys_error msg ->               Log.fail "%s" (sprintf "System error: %s" msg)
  | Yojson.Json_error msg ->       Log.fail "%s" (sprintf "Json error: %s" msg)
  | Grewlib.Bug msg ->             Log.fail "%s" (sprintf "Grewlib.bug, please report: %s" msg)
  | exc ->                         Log.fail "%s" (sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))

(* ---------------------------------------------------------------------------------------------------- *)
let ensure_dir dir =
  try (* catch if dir does not exist *)
    match Unix.stat dir with
    | { Unix.st_kind = Unix.S_DIR; _ } -> ()
    | _ ->  error "path `%s` already exists and is not directory" dir
  with Unix.Unix_error (Unix.ENOENT,_,_) -> (* dir does not exist -> try to create it *)
    try Unix.mkdir dir 0o755
    with exc -> error "cannot create dir %s (%s)" dir (Printexc.to_string exc)

(* ================================================================================ *)
module Counter = struct
  let back = sprintf "\r%s\r" (String.make 100 ' ')

  let print value total text =
    if not !quiet
    then eprintf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !quiet then eprintf "%s100.00%%\n%!" back
end (* module Counter *)

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

    let parse_request (id, json) =
      let assoc = json |> to_assoc in
      { id;
        desc = List.assoc "desc" assoc |> to_string;
        code = List.assoc "code" assoc |> to_list |> List.map to_string;
      } in

    (json |> to_assoc |> List.map parse_request, json)

  let compute_ratios l =  
    let sum = float (List.fold_left (+) 0 l) in
    List.map (fun i -> `Float ((Float.round (10000. *. float i /. sum)) /. 100.)) l
end (* module Stat *)

(* ==================================================================================================== *)
module Validation = struct
  type item = {
    request: string list;
    description: string;
    level: string;
  }

  type modul = {
    title: string;
    items: item list;
    languages: string list option; (* list of the languages codes restriction, None for all lang *)
  }

  (* -------------------------------------------------------------------------------- *)
  let load_json json_file =
    let open Yojson.Basic.Util in

    let json =
      try Yojson.Basic.from_file json_file
      with Yojson.Json_error msg -> error ~fct:"Validation.load_json" ~file:json_file "%s" msg in

    let parse_one json =
      let request =
        try json |> member "request" |> to_string |> (fun x -> [x])
        with Type_error _ ->
        try json
            |> member "request"
            |> to_list
            |> (List.map to_string)
        with Type_error (json_error,_) ->
          error
            ~fct:"Validation.load_json"
            ~file: json_file
            "\"request\" field is mandatory and must be a string or a list of strings (%s)" json_error in
      let description =
        try json |> member "description" |> to_string
        with Type_error _ -> "No description" in
      let level =
        try json |> member "level" |> to_string
        with Type_error _ -> "No level" in

      { request; description; level } in

    let title =
      try json |> member "title" |> to_string
      with Type_error (json_error,_) ->
        error
          ~fct:"Validation.load_json"
          ~file: json_file
          "\"title\" field is mandatory and must be a string or a list of strings (%s)" json_error in
    let items = List.map parse_one (json |> member "items" |> to_list) in
    let languages = try Some (json |> member "languages" |> to_list |> List.map to_string) with Type_error _ -> None in
    { title; items; languages }

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
        (CCList.filter_map
          (fun modul ->
            match (Corpus_desc.get_lang_opt corpus_desc, modul.languages) with
              | (Some lang, Some lang_list) when not (List.mem lang lang_list) -> None
              | _ ->
                let (out_items : Yojson.Basic.t) =
                  `List
                    (List.map
                      (fun item ->
                        let grew_request =
                          try Request.parse ~config (String.concat " " item.request)
                          with Grewlib.Error _msg -> (* TODO *)
                            error
                              ~fct:"Validation.check"
                              ~data:(`String (String.concat " " item.request))
                              "cannot parse request associated with desc: %s" item.description in
                        let count =
                          Corpus.fold_left (fun acc _ graph ->
                              acc + (List.length (Matching.search_request_in_graph ~config grew_request graph))
                            ) 0 corpus in
                        `Assoc [
                          "count", `Int count;
                          "request", `List (List.map (fun x -> `String x) item.request);
                          "description", `String item.description;
                          "level", `String item.level
                        ]
                      ) modul.items
                    ) in
              Some (`Assoc ["title", `String modul.title; "items", out_items])
          ) modul_list
        ) in

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

end (* module Validation *)
