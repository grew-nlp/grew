(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Conll

open Grewlib

open Grew_cli_global
open Grew_cli_utils

module Grew_args = struct
  let help () = List.iter (fun x -> ANSITerminal.printf [ANSITerminal.blue] "%s\n%!" x) [
      "----------------------------------------------------------------";
      "See https://grew.fr/usage/cli/ for a comprehensive documentation";
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
      "-----------------------------------------------------------------";
    ]


  let push_request string_request =
    requests := !requests @ [string_request]

  let push_request_list string_request_list = 
    requests := !requests @ string_request_list

  let rec loop = function
    | [] -> ()
    | "-grs" :: file :: args -> grs := Some file; loop args
    | "-i" :: files :: args -> input_data := !input_data @ (Str.split (Str.regexp " ") files); loop args
    | "-o" :: file :: args -> output_data := Some file; loop args
    | "-strat" :: s :: args -> strat := s; loop args

    | "-request" :: string :: args -> push_request string; loop args
    | "-requests" :: multi_string :: args -> push_request_list (Str.split (Str.regexp " ") multi_string); loop args

    | "-pattern" :: string :: args ->
        Log.warning "-pattern command line args are deprecated, please use -request instead" ; 
        push_request string; loop args
    | "-patterns" :: multi_string :: args ->
        Log.warning "-patterns command line args are deprecated, please use -requests instead" ; 
        push_request_list (Str.split (Str.regexp " ") multi_string); loop args

    | "-key" :: s :: args -> clustering := !clustering @ [s]; loop args
    | "-whether" :: s :: args ->
        Log.warning "Whether argument is deprecated, see https://grew.fr/usage/cli/#with-clustering";
        clustering := !clustering @ [Printf.sprintf "{%s}" s]; loop args
    | "-html" :: args -> html := true; loop args

    | "-timeout" :: f :: args -> timeout := Some (float_of_string f); Rewrite.set_timeout (Some (float_of_string f)); loop args
    | "-max_rules" :: i :: args -> Rewrite.set_max_rules (int_of_string i); loop args

    | "-quiet" :: args -> quiet := true; loop args
    | "-verbose" :: args -> verbose := true; loop args

    | "-cupt" :: args -> output := Conll (Conll_columns.cupt); loop args
    | "-semcor" :: args -> output := Conll (Conll_columns.frsemcor); loop args
    | "-columns" :: desc :: args -> output := Conll (desc |> CCString.split_on_char ' ' |> Conll_columns.of_list); loop args
    | "-dot" :: args -> output := Dot; loop args
    | "-json" :: args -> output := Json; loop args
    | "-tsv" :: args -> output := Tsv; loop args
    | "-multi_json" :: args -> output := Multi_json; loop args

    | "-text_from_tokens" :: args -> text_from_tokens := true; loop args
    | "-force" :: args -> force := true; loop args

    | "-safe_commands" :: args -> Grewlib.set_safe_commands true; loop args
    | "-track_rules" :: args -> Grewlib.set_track_rules true; loop args
    | "-debug" :: args -> Grewlib.set_debug_mode true; loop args
    | "-dep_dir" :: dir :: args -> dep_dir := Some dir; loop args

    | "-config" :: value :: args -> config := handle (fun () -> Conll_config.build value) (); loop args

    | "-CORPUSBANK" :: value :: args -> setenv "CORPUSBANK" value; loop args
    | "-UDTOOLS" :: value :: args -> setenv "UDTOOLS" value; loop args
    | "-SUDTOOLS" :: value :: args -> setenv "SUDTOOLS" value; loop args
    | "-SUDVALIDATION" :: value :: args -> setenv "SUDVALIDATION" value; loop args

    | "-rff" :: value :: args -> config := Conll_config.remove_from_feats value !config; loop args

    | "-gr" :: args -> Log.warning "The GR file is no longer supported, please use JSON format"; loop args
    | x :: args when String.length x > 0 && x.[0] = '-' -> Log.warning "Invalid option: %s, it is ignored!" x; loop args
    | x :: args -> anonymous_args := x :: !anonymous_args; loop args

  let parse () =
    match Array.to_list Sys.argv with
    | [] -> assert false
    | [_] -> help (); exit 0
    | _ :: "version" :: _ ->
      begin
        match Build_info.V1.version () with
        | Some v -> Printf.printf "%s\n" (Build_info.V1.Version.to_string v)
        | None -> Printf.printf "dev\n%!"
      end; exit 0
    | _ :: "libraries" :: _ ->
        List.iter
        (fun lib -> match Build_info.V1.Statically_linked_library.version lib with
        | Some v -> Printf.printf " - %s: %s\n" 
          (Build_info.V1.Statically_linked_library.name lib)
          (Build_info.V1.Version.to_string v)
        | None -> ()
        ) (Build_info.V1.Statically_linked_libraries.to_list ()); exit 0
    | _ :: "help" :: _ -> help (); exit 0
    | _ :: sub :: args -> subcommand := Some sub; loop args
end
