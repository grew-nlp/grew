(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Conll
open Grewlib

open Grew_cli_global

let rec json_to_string = function
  | `Assoc list ->
      list |> List.map (fun (k,v) -> sprintf "%s --> %s" k (json_to_string v)) |> String.concat "\n"
  | `String s -> s
  | x -> Yojson.Basic.pretty_to_string x

let from_json json =
  let open Yojson.Basic.Util in
  if !Grew_cli_global.verbose
  then json_to_string json
  else sprintf "%s" (json |> member "message" |> to_string)

let help () = List.iter (fun x -> ANSITerminal.printf [ANSITerminal.blue] "%s\n%!" x) [
    "----------------------------------------------------------------";
    "Usage: grew <subcommand> [<args>]";
    "";
    "Main subcommands:";
    "  transform  Apply a GRS on a corpus";
    "  grep       Search request(s) in corpora";
    "  count      Count request(s) in corpora";
    "";
    "Other subcommands:";
    "  help       Print this message";
    "  version    Print current version number";
    "  libraries  Print versions of Ocaml libraries used";
    "";
    "-----------------------------------------------------------------";
    "See https://grew.fr/usage/cli/ for a comprehensive documentation";
    "----------------------------------------------------------------";
  ]

  let transform_help () = List.iter (fun x -> ANSITerminal.printf [ANSITerminal.blue] "%s\n%!" x) [
    "----------------------------------------------------------------";
    "Description: Apply a Graph Rewriting System to a set of graphs"; 
    "";
    "Usage: grew transform [<args>]";
    "";
    "Optional args:";
    "  -grs <file>            select the GRS to apply (default is an empty GRS)";
    "  -strat <string>        select the strategy from the GRS to apply (default is 'main')";
    "  -i <file>|<directory>  select the input data (default is reading from stdin)";
    "  -config <config>       select the config to used to parse request (default is 'ud')";
    "";
    "-------------------------------------------------------------------------";
    "See https://grew.fr/usage/cli#transform for a comprehensive documentation";
    "-------------------------------------------------------------------------";
  ]

  let grep_help () = List.iter (fun x -> ANSITerminal.printf [ANSITerminal.blue] "%s\n%!" x) [
    "----------------------------------------------------------------";
    "Description: Search for request(s) in corpora";
    "";
    "Usage: grew grep [<args>]";
    "";
    "Required args:";
    "  -request <file>|<code>    describe the request to search for";
    "";
    "Optional args:";
    "  -i <file/directory>       select the input data (default is reading from stdin)";
    "  -key <clutering_item>     make a clustering of search output (the option can appear multiple times)";
    "  -config <config>          select the config to used to parse request (default is 'ud')";
    "";
    "--------------------------------------------------------------------";
    "See https://grew.fr/usage/cli#grep for a comprehensive documentation";
    "--------------------------------------------------------------------";
  ]
  let count_help () = List.iter (fun x -> ANSITerminal.printf [ANSITerminal.blue] "%s\n%!" x) [
    "----------------------------------------------------------------";
    "Description: Count for request(s) in corpora";
    "";
    "Usage: grew count [<args>]";
    "";
    "Required args:";
    "  -request <file>|<code>    describe request to count for (the option can appear multiple times)";
    "";
    "Optional args:";
    "  -i <file/directory>       select the input data (default is reading from stdin)";
    "  -key <clutering_item>     make a clustering of count output (the option can appear multiple times)";
    "  -config <config>          select the config to used to parse request (default is 'ud')";
    "";
    "---------------------------------------------------------------------";
    "See https://grew.fr/usage/cli#count for a comprehensive documentation";
    "---------------------------------------------------------------------";
  ]

(* ==================================================================================================== *)
module Log = struct

  let echo_help = ref false
  let warning_ message =
    ANSITerminal.eprintf [ANSITerminal.blue] "WARNING: %s\n" message;
    eprintf "%!" (* force synchronous printing *)

  let warning message = Printf.ksprintf warning_ message

  let fail_ message =
    ANSITerminal.eprintf [ANSITerminal.red] "%s\n" message;
    eprintf "%!" (* force synchronous printing *);
    begin
      match (!echo_help, !subcommand) with
      | (false, _ ) -> ()
      | (_, Some "grep") -> grep_help ()
      | (_, Some "count") -> count_help ()
      | (_, Some "transform") -> transform_help ()
      | _ -> help ()
    end;
    exit 1

  let fail message = Printf.ksprintf fail_ message
end

(* ==================================================================================================== *)
exception Error of Yojson.Basic.t

let error_ ?file ?line ?fct ?data msg =
  let opt_list = [
    Some ("message", `String msg);
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
  | Error json ->          Log.fail "Error: %s" (from_json json)
  | Conll_error json ->    Log.fail "Conll error: %s" (from_json json)
  | Grewlib.Error msg ->           Log.fail "Grewlib error:\n  %s" msg
  | Sys_error msg ->               Log.fail "System error: %s" msg
  | Yojson.Json_error msg ->       Log.fail "Json error: %s" msg
  | Grewlib.Bug msg ->             Log.fail "Grewlib.bug, please report:\n%s" msg
  | exc ->                            Log.fail "Uncaught exception, please report: %s" (Printexc.to_string exc)

(* ================================================================================ *)
module Counter = struct

  let print value total text =
    if not !quiet
    then eprintf "\r\027[K%.2f%% (%s)%!" (((float value) /. (float total))*. 100. ) text

  let finish () = if not !quiet then eprintf "\r\027[K100.00%%\n%!"
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
            with 
            | Unix.Unix_error _ -> error ~fct:"[Input.parse]" "File not found `%s`" one
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
module File = struct
  (* get the last modif time of a [file]. Returns [min_float] if the file does not exist *)
  let last_modif file =
    try
      let stat = Unix.stat file in
      stat.Unix.st_mtime
    with Unix.Unix_error _ -> Float.min_float
  
  let concat_names l = List.fold_left Filename.concat "" l
end

(* This function tries to turn a string from the command line into a request
   The input [string_request] can be a file or a request code.
   Error reporting is tricky: reporting the file loading error of the parsing error.
*)
let request_load_or_parse ~config string_request =
  if Sys.file_exists string_request
  then 
    try Request.load ~config string_request
    with Grewlib.Error e -> 
      error "Cannot load the request file `%s`:\n - %s" string_request e
  else 
    try Request.parse ~config string_request
    with Grewlib.Error e -> 
      if CCString.ends_with ~suffix:".req" string_request (* we guess that the user try to use a file! *)
      then error "Cannot find the request file `%s`" string_request
      else error "Cannot parse the request string `%s`:\n - %s" string_request e
