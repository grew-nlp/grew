(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                          *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Arg
open Log
open Conllx
open Libgrew

open Grew_utils

module Grew_args = struct

  type mode = Undefined | Transform | Grep | Count | Valid | Stat | Compile | Clean | Test
  let mode = ref Undefined

  let grs = ref None
  let dep_dir = ref None

  type output = Conllx of  Conllx_columns.t | Gr | Dot | Json | Multi_json
  let output = ref (Conllx Conllx_columns.default)

  let (input_data : string list ref) = ref []
  let (output_data : string option ref) = ref None
  let strat = ref "main"
  let timeout = ref None
  let (patterns : string list ref) = ref []
  let key = ref None
  let html = ref false

  let config = ref (Conllx_config.build "ud")  (* "ud" is used as default value. *)

  let grew_match_server = ref None
  let force = ref false

  let help () = List.iter (fun x -> Printf.printf "%s\n" x) [
    "----------------------------------------------------------";
    "usage: grew <subcommand> [<args>]";
    "";
    "subcommands are:";
    "  transform  Apply a GRS on a corpus";
    "  grep       Search for a pattern in a corpus";
    "  version    Print current version number";
    "  help <sub> Print help for the given subcommand";
    "";
    "see subcommands help for args";
    "For additional information, see https://grew.fr";
    "----------------------------------------------------------";
  ]

  let help_grep () = List.iter (fun x -> Printf.printf "%s\n" x) [
    "----------------------------------------------------------";
    "usage: grew grep [<args>]";
    "";
    "This subcommand searches for a pattern in a corpus.";
    "";
    "args are optionnal and can be change in the GUI:";
    "  -pattern <pat>   The pattern to search for";
    "  -i <corp>        The input data";
    "";
    "For additional information, see https://grew.fr";
    "----------------------------------------------------------";
  ]

  let help_transform () = List.iter (fun x -> Printf.printf "%s\n" x) [
    "----------------------------------------------------------";
    "usage: grew transform [<args>]";
    "";
    "This subcommand applies a grs to a graph or a corpus.";
    "";
    "Required arguments:";
    "  -grs <file>    The Graph Rewriting System to load";
    "  -i <file>      The input data (a graph or a corpus)";
    "  -o <file>      The output file";
    "";
    "Optionnal arguments:";
    "  -strat <name>  The stategy used in transformation (default=\"main\")";
    "  -cupt          If the option is present, a 11-column CoNLL format is produced";
    "  -gr            If the option is present, the gr output format is produced";
    "  -track_rules   If the option is present, data about the rules applied are given in output";
    "";
    "For additional information, see https://grew.fr";
    "----------------------------------------------------------";
  ]

  let rec loop = function
  | [] -> ()
  | "-grs" :: file :: args -> grs := Some file; loop args
  | "-i" :: files :: args -> input_data := !input_data @ (Str.split (Str.regexp " ") files); loop args
  | "-o" :: file :: args -> output_data := Some file; loop args
  | "-strat" :: s :: args -> strat := s; loop args
  | "-pattern" :: files :: args
  | "-patterns" :: files :: args -> patterns := !patterns @ (Str.split (Str.regexp " ") files); loop args
  | "-key" :: s :: args -> key := Some s; loop args
  | "-html" :: args -> html := true; loop args

  | "-timeout" :: f :: args -> timeout := Some (float_of_string f); Rewrite.set_timeout (Some (float_of_string f)); loop args
  | "-max_rules" :: i :: args -> Rewrite.set_max_rules (int_of_string i); loop args

  | "-quiet" :: args -> quiet := true; loop args

  | "-cupt" :: args -> output := Conllx (Conllx_columns.cupt); loop args
  | "-semcor" :: args -> output := Conllx (Conllx_columns.frsemcor); loop args
  | "-columns" :: desc :: args -> output := Conllx (Conllx_columns.build desc); loop args
  | "-gr" :: args -> output := Gr; loop args
  | "-dot" :: args -> output := Dot; loop args
  | "-json" :: args -> output := Json; loop args
  | "-multi_json" :: args -> output := Multi_json; loop args

  | "-grew_match_server" :: dir :: args -> grew_match_server := Some dir; loop args
  | "-force" :: args -> force := true; loop args

  | "-safe_commands" :: args -> Libgrew.set_safe_commands true; loop args
  | "-track_rules" :: args -> Libgrew.set_track_rules true; loop args
  | "-debug" :: args -> Libgrew.set_debug_mode true; loop args
  | "-dep_dir" :: dir :: args -> dep_dir := Some dir; loop args

  | "-config" :: value :: args -> config := handle (fun () -> Conllx_config.build value) (); loop args

  | "-rff" :: value :: args -> config := Conllx_config.remove_from_feats value !config; loop args

  | x :: args -> Log.fwarning "Invalid argument: %s, it is ignored!" x; loop args

  let parse () =
    match Array.to_list Sys.argv with
    | [] -> failwith "bug: Empty argv"
    | _ :: "gui" :: _ -> Printf.printf "The gui mode is not longer supported"
    | _ :: "transform" :: args -> mode := Transform; loop args
    | _ :: "grep" :: args -> mode := Grep; loop args
    | _ :: "count" :: args -> mode := Count; loop args
    | _ :: "valid" :: args -> mode := Valid; loop args
    | _ :: "stat" :: args -> mode := Stat; loop args
    | _ :: "compile" :: args -> mode := Compile; loop args
    | _ :: "clean" :: args -> mode := Clean; loop args
    | _ :: "version" :: _ ->
      Printf.printf "libgrew: %s\n" (Libgrew.get_version ());
      Printf.printf "grew: %s\n" VERSION;
    | _ :: "test" :: args -> mode := Test; loop args
    | _ :: "help" :: "transform" :: _ -> help_transform ()
    | _ :: "help" :: "grep" :: _ -> help_grep ()
    | _ :: "help" :: "help" :: _ -> Printf.printf "Such a complex feature is still in development!\n"
    | [_] -> help ()
    | _ :: "help" :: _ -> help ()
    | _ :: cmd :: _ -> Log.fwarning "Unknown command \"%s\"" cmd; help()
end
