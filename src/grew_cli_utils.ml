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
