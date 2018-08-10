(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Arg
open Log
open Libgrew

module Grew_args = struct

  type mode = Undefined | Gui of string | Transform | Grep | Test
  let mode = ref Undefined

  let grs = ref None
  let gui_doc = ref false
  let dep_dir = ref None

  let (input_data : string list ref) = ref []
  let (output_file : string option ref) = ref None
  let strat = ref "main"
  let quiet = ref false
  let timeout = ref None
  let (pattern : string option ref) = ref None
  let html = ref false

  let help () = List.iter (fun x -> Printf.printf "%s\n" x) [
    "----------------------------------------------------------";
    "usage: grew <subcommand> [<args>]";
    "";
    "subcommands are:";
    "  transform  Apply a GRS on a corpus";
    "  gui        Run the Gtk interface";
    "  grep       Search for a pattern in a corpus";
    "  version    Print current version number";
    "  help <sub> Print help for the given subcommand";
    "";
    "see subcommands help for args";
    "For additional information, see http://grew.loria.fr";
    "----------------------------------------------------------";
  ]

  let help_gui () = List.iter (fun x -> Printf.printf "%s\n" x) [
    "----------------------------------------------------------";
    "usage: grew gui [<args>]";
    "";
    "This subcommand runs the GTK interface for Grew.";
    "It supposes that the opam package \"grew_gui\" is installed.";
    "";
    "args are optionnal and can be change in the GUI:";
    "  -grs <file>    The Graph Rewriting System to load";
    "  -i <file>      The input data (a graph or a corpus)";
    "  -strat <name>  The stategy used by default";
    "";
    "For additional information, see http://grew.loria.fr";
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
    "For additional information, see http://grew.loria.fr";
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
    "";
    "For additional information, see http://grew.loria.fr";
    "----------------------------------------------------------";
  ]

  let rec loop = function
  | [] -> ()
  | "-grs" :: file :: args -> grs := Some file; loop args
  | "-i" :: files :: args ->
      input_data := (Str.split (Str.regexp " ") files) @ !input_data; loop args
  | "-o" :: file :: args -> output_file := Some file; loop args
  | "-strat" :: s :: args -> strat := s; loop args
  | "-pattern" :: file :: args -> pattern := Some file; loop args
  | "-html" :: args -> html := true; loop args

  | "-timeout" :: f :: args -> timeout := Some (float_of_string f); Rewrite.set_timeout (Some (float_of_string f)); loop args
  | "-max_depth_det" :: i :: args -> Log.warning "max_depth_det not implemented, skip the arg"; loop args
  | "-max_depth_non_det" :: i :: args -> Log.warning "max_depth_non_det not implemented, skip the arg"; loop args

  | "-quiet" :: args -> quiet := true; loop args

  | "-safe_commands" :: args -> Libgrew.set_safe_commands true; loop args
  | "-debug" :: args -> Libgrew.set_debug_mode true; loop args
  | "-debug_loop" :: args -> Rewrite.set_debug_loop (); loop args
  | "-dep_dir" :: dir :: args -> dep_dir := Some dir; loop args

  | x -> Log.fwarning "Ignored arguments: %s" (String.concat " " x)

  let parse () =
    match Array.to_list Sys.argv with
    | [] -> failwith "bug: Empty argv"
    | _ :: "gui" :: args -> mode := Gui (String.concat " " args)
    | _ :: "transform" :: args -> mode := Transform; loop args
    | _ :: "grep" :: args -> mode := Grep; loop args
    | _ :: "version" :: _ -> Printf.printf "%s\n" VERSION;
    | _ :: "help" :: "gui" :: _ -> help_gui ()
    | _ :: "help" :: "transform" :: _ -> help_transform ()
    | _ :: "help" :: "grep" :: _ -> help_grep ()
    | _ :: "help" :: "help" :: _ -> Printf.printf "Such a complex feature is still in development!\n"
    | [_] -> help ()
    | _ :: "help" :: _ -> help ()
    | _ :: cmd :: _ -> Log.fwarning "Unknown command \"%s\"" cmd; help()
end
