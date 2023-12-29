(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2023 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Conll
open Grewlib

open Grew_cli_global

(* ==================================================================================================== *)
module Log = struct
  let now () =
    let gm = Unix.localtime (Unix.time ()) in
    Printf.sprintf "%02d/%02d/%02d - %02d:%02d"
      (gm.Unix.tm_year - 100)
      (gm.Unix.tm_mon + 1)
      gm.Unix.tm_mday
      gm.Unix.tm_hour
      gm.Unix.tm_min

  let warning_ message =
    ANSITerminal.eprintf [ANSITerminal.blue] "WARNING: %s\n" message;
    eprintf "%!" (* force synchronous printing *)

  let warning message = Printf.ksprintf warning_ message

  let fail_ message =
    ANSITerminal.eprintf [ANSITerminal.red] "FAIL: %s\n" message;
    eprintf "%!" (* force synchronous printing *);
    exit 1

  let fail message = Printf.ksprintf fail_ message

  let green x = Printf.ksprintf (fun s -> ANSITerminal.eprintf [ANSITerminal.green] "%s" s; eprintf "%!") x
  let blue x = Printf.ksprintf (fun s -> ANSITerminal.eprintf [ANSITerminal.blue] "%s" s; eprintf "%!") x
  let red x = Printf.ksprintf (fun s -> ANSITerminal.eprintf [ANSITerminal.red] "%s" s; eprintf "%!") x
  let magenta x = Printf.ksprintf (fun s -> ANSITerminal.eprintf [ANSITerminal.magenta] "%s" s; eprintf "%!") x
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

let bug_ msg = Log.fail "%s" msg 
let bug msg = Printf.ksprintf bug_ msg
(* -------------------------------------------------------------------------------- *)

let handle fct () =
  try fct ()
  with
  | Error json ->                  Log.fail "%s" (Yojson.Basic.pretty_to_string json)
  | Conll_error json ->            Log.fail "%s" (Yojson.Basic.pretty_to_string json)
  | Grewlib.Error msg ->           Log.fail "%s" msg
  | Sys_error msg ->               Log.fail "%s" (sprintf "System error: %s" msg)
  | Yojson.Json_error msg ->       Log.fail "%s" (sprintf "Json error: %s" msg)
  | Grewlib.Bug msg ->             Log.fail "%s" (sprintf "Grewlib.bug, please report: %s" msg)
  | exc ->                         Log.fail "%s" (sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))

(* ================================================================================ *)
module Counter = struct
  let back = sprintf "\r%s\r" (String.make 100 ' ')

  let print value total text =
    if not !quiet
    then eprintf "%s%.2f%% (%s)%!" back (((float value) /. (float total))*. 100. ) text

  let finish () = if not !quiet then eprintf "%s100.00%%\n%!" back
end (* module Counter *)

(* ==================================================================================================== *)
module Input = struct
  type t =
    | Multi of Corpus_desc.t list
    | Mono of Corpus.t

  let parse () =
    let config = !config in
    match !input_data with
    | [] -> Mono (Corpus.from_stdin ~config ())
    | l ->
      try Multi (CCList.flat_map Corpus_desc.load_json !input_data)
      with Grewlib.Error _ ->
        (* TODO add specific error for compile/ clean *)
        match l with
        | [one] ->
          begin
            try
              match Unix.stat one with
              | { Unix.st_kind = Unix.S_DIR; _ } -> Mono (Corpus.from_dir ~config one)
              | _ -> Mono (Corpus.from_file ~config one)
            with Unix.Unix_error _ -> error ~fct:"[Input.parse]" "File not found `%s`" one
          end
        | files ->
          let sub_corpora =
            List.fold_left
              (fun acc file ->
                 try
                   let subcorpus = Corpus.from_file ~config file in
                   subcorpus :: acc
                 with Unix.Unix_error _ -> error ~fct:"[Input.parse]" "File not found `%s`" file
              ) [] files in
          Mono (Corpus.merge sub_corpora)
end

(* ==================================================================================================== *)
module Corpusbank = struct
  let desc_map = ref None

  (* lazy loading of corpus desc files *)
  let get_desc_map () =
    match !Grew_cli_global.corpusbank with
    | None -> error "No corpusbank defined"
    | Some corpusbank -> 
    match !desc_map with
    | Some data -> data
    | None ->
      try
        let all_files = Sys.readdir corpusbank in
        let data = Array.fold_left
          (fun acc file ->
            if Filename.extension file = ".json"
            then
              begin
                let descs = Corpus_desc.load_json (Filename.concat corpusbank file) in
                List.fold_left
                  (fun acc2 desc ->
                    let id = Corpus_desc.get_id desc in
                    if String_map.mem id acc2
                    then error "Duplicate definition of corpus_id `%s`" id
                    else String_map.add id desc acc2
                  ) acc descs
            end
            else acc
          ) String_map.empty all_files in
        desc_map := Some data;
        data
      with Sys_error _ -> error "corpusbank directory `%s` not found" corpusbank

  let get_corpus_desc_opt corpus_id = String_map.find_opt corpus_id (get_desc_map ())
end

(* ==================================================================================================== *)
module File = struct
  (* get the last modif time of a [file]. Returns [min_float] if the file does not exist *)
  let last_modif file =
    try
      let stat = Unix.stat file in
      stat.Unix.st_mtime
    with Unix.Unix_error _ -> Float.min_float
  
  let concat_names l = List.fold_left Filename.concat "" l
end