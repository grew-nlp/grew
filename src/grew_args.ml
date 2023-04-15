(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                          *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Conll

open Grewlib

open Grew_cli_utils

module Grew_args = struct

  type mode = Undefined | Transform | Grep | Count | Valid | Stat | Compile | Clean | Test
  let mode = ref Undefined

  let grs = ref None
  let dep_dir = ref None

  type output = Conll of  Conll_columns.t | Dot | Json | Multi_json | Tsv
  let output = ref (Conll Conll_columns.default)

  let (input_data : string list ref) = ref []
  let (output_data : string option ref) = ref None
  let strat = ref "main"
  let timeout = ref None
  let (requests : string list ref) = ref []
  let html = ref false

  let (clustering : string list ref) = ref []

  let config = ref (Conll_config.build "ud")  (* "ud" is used as default value. *)

  let grew_match_server = ref None
  let force = ref false

  let help () = List.iter (fun x -> Printf.printf "%s\n%!" x) [
      "----------------------------------------------------------";
      "usage: grew <subcommand> [<args>]";
      "";
      "subcommands are:";
      "  transform  Apply a GRS on a corpus";
      "  grep       Search for a request in a corpus";
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
      "This subcommand searches for a request in a corpus.";
      "";
      "args are optionnal and can be change in the GUI:";
      "  -request <req>   The request to search for";
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
    | "-request" :: files :: args
    | "-requests" :: files :: args -> requests := !requests @ (Str.split (Str.regexp " ") files); loop args
    | "-pattern" :: files :: args
    | "-patterns" :: files :: args -> 
        Log.warning "-pattern and -patterns comman line args are deprecated, replaced by -request and -requests"; 
        requests := !requests @ (Str.split (Str.regexp " ") files); loop args
    | "-key" :: s :: args -> clustering := !clustering @ [s]; loop args
    | "-whether" :: s :: args ->
        Log.warning "Whether argument is deprecated, see https://grew.fr/usage/cli/#with-clustering";
        clustering := !clustering @ [Printf.sprintf "{%s}" s]; loop args
    | "-html" :: args -> html := true; loop args

    | "-timeout" :: f :: args -> timeout := Some (float_of_string f); Rewrite.set_timeout (Some (float_of_string f)); loop args
    | "-max_rules" :: i :: args -> Rewrite.set_max_rules (int_of_string i); loop args

    | "-quiet" :: args -> quiet := true; loop args

    | "-cupt" :: args -> output := Conll (Conll_columns.cupt); loop args
    | "-semcor" :: args -> output := Conll (Conll_columns.frsemcor); loop args
    | "-columns" :: desc :: args -> output := Conll (Conll_columns.build desc); loop args
    | "-dot" :: args -> output := Dot; loop args
    | "-json" :: args -> output := Json; loop args
    | "-tsv" :: args -> output := Tsv; loop args
    | "-multi_json" :: args -> output := Multi_json; loop args

    | "-grew_match_server" :: dir :: args -> grew_match_server := Some dir; loop args
    | "-force" :: args -> force := true; loop args

    | "-safe_commands" :: args -> Grewlib.set_safe_commands true; loop args
    | "-track_rules" :: args -> Grewlib.set_track_rules true; loop args
    | "-debug" :: args -> Grewlib.set_debug_mode true; loop args
    | "-dep_dir" :: dir :: args -> dep_dir := Some dir; loop args

    | "-config" :: value :: args -> config := handle (fun () -> Conll_config.build value) (); loop args

    | "-rff" :: value :: args -> config := Conll_config.remove_from_feats value !config; loop args

    | "-gr" :: args -> Log.warning "The GR file is no longer supported, please use JSON format"; loop args
    | x :: args -> Log.warning "Invalid argument: %s, it is ignored!" x; loop args

  let parse () =
    match Array.to_list Sys.argv with
    | [] -> failwith "bug: Empty argv"
    | _ :: "gui" :: _ -> Printf.printf "The gui mode is not longer supported, see http://transform.grew.fr"
    | _ :: "transform" :: args -> mode := Transform; loop args
    | _ :: "grep" :: args -> mode := Grep; loop args
    | _ :: "count" :: args -> mode := Count; loop args
    | _ :: "valid" :: args -> mode := Valid; loop args
    | _ :: "stat" :: args -> mode := Stat; loop args
    | _ :: "compile" :: args -> mode := Compile; loop args
    | _ :: "clean" :: args -> mode := Clean; loop args
    | _ :: "version" :: _ ->
      begin
        match Build_info.V1.version () with
        | Some v -> Printf.printf "%s\n" (Build_info.V1.Version.to_string v)
        | None -> Printf.printf "dev\n%!"
      end
    | _ :: "libraries" :: _ ->
        List.iter
        (fun lib -> match Build_info.V1.Statically_linked_library.version lib with
        | Some v -> Printf.printf " - %s: %s\n" 
          (Build_info.V1.Statically_linked_library.name lib)
          (Build_info.V1.Version.to_string v)
        | None -> ()
        ) (Build_info.V1.Statically_linked_libraries.to_list ())
    | _ :: "test" :: args -> mode := Test; loop args
    | _ :: "help" :: "transform" :: _ -> help_transform ()
    | _ :: "help" :: "grep" :: _ -> help_grep ()
    | _ :: "help" :: "help" :: _ -> Printf.printf "Such a complex feature is still in development!\n"
    | [_] -> help ()
    | _ :: "help" :: _ -> help ()
    | _ :: cmd :: _ -> Log.fail "Unknown command \"%s\"" cmd
end
