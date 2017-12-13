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

  type mode = Gui | Transform | Grep | Test
  let mode = ref Gui

  let grs = ref None
  let gui_doc = ref false
  let fullscreen = ref false
  let old_grs = ref false

  let input_data = ref None
  let output_dir = ref None
  let output_file = ref None
  let strat = ref "main"
  let quiet = ref false
  let out_gr = ref false
  let out_conll = ref false
  let features = ref None
  let main_feat = ref None
  let timeout = ref None
  let title = ref None
  let pattern = ref None
  let node_id = ref None

  let usage =
    "grew has 3 running modes:\n"^
    "  * GUI MODE: a Gtk interface (this is the default mode)\n"^
    "  * CORPUS MODE: runs rewriting on all graphs of a directory\n"^
    "  * DET MODE: runs a deterministic grs on all graphs of a directory\n"^
    "\n"^
    "Options for mode selection:"

  let dump_version () =
    Printf.printf "grew:    %s\n" VERSION;
    Printf.printf "libgrew: %s\n" (Libgrew.get_version ())

  let obsolote mode = failwith (Printf.sprintf "The mode %s was removed, sorry!" mode)

  let args = [
    "-corpus", Unit (fun () -> obsolote "corpus"),      "                     enable corpus mode";
    "-det", Unit (fun () -> mode := Transform),         "                        enable det mode: rewrite a corpus with a deterministric grs";
    "-full", Unit (fun () -> obsolote "full"),         "                       [REMOVED] enable full mode: rewrite a corpus (conll output)";
    "-filter", Unit (fun () -> obsolote "filter"),         "                     [REMOVED] enable filter mode";
    "-grep", Unit (fun () -> mode := Grep),         "                       enable grep mode";
    "-test", Unit (fun () -> mode := Test),         "                       enable test mode";

    "-version", Unit (fun () -> dump_version(); exit 0),         "                       gives versions of code and libraries\n\nOptions for all modes";

    (* options for all modes *)
    "-grs", String (fun s -> grs := Some s),          "<grs_file>              chose the grs file to load";
    "-old_grs", Unit (fun () -> old_grs := true), "                       Use old grs parser";
    "-strat", String (fun s -> strat := s),                   "<strat>                 set the module strategy to use";
    "-seq", String (fun s -> strat := s),                   "<strat>                 [DEPRECATED] replaced by -strat option";
    "-timeout", Float (fun f -> timeout := Some f; Rewrite.set_timeout (Some f)),                   "<float>             set a timeout on rewriting";
    "-max_depth_det", Int (fun v -> Rewrite.set_max_depth_det v),                   "<int>         set the maximum depth of rewriting in a module in deterministric rewriting (default: 2000)";
    "-max_depth_non_det", Int (fun v -> Rewrite.set_max_depth_non_det v),                   "<int>     set the maximum depth of rewriting in a module in non-deterministric rewriting (default: 100)";
    "-features", String (fun s -> features := Some (Str.split (Str.regexp "; *") s)),            "<feat_name_list>   set the list of feature names to printf in dep format";
    "-main_feat", String (fun s -> main_feat := Some s),       "<feat_name_list>  set the list of feature names used in dep format to set the \"main word\"";
    "-debug", Unit (fun () -> libgrew_debug_mode ()),  "                      enable debug mode";
    "-debug_loop", Unit (fun () -> Rewrite.set_debug_loop ()),  "                 enable loop debug mode\n\nOptions for GUI mode";

    (* options for GUI mode *)
    "-fullscreen", Unit (fun () -> fullscreen := true), "                        fullscreen";
    "-doc", Unit (fun () -> gui_doc := true), "                        force to build the GRS doc\n\nOptions for corpus, det and cluster modes";

    (* options for corpus, det and cluster mode *)
    "-i", String (fun file -> input_data := Some file),  "<input_data>              set the input data (file or directory) where to find graph files (.gr or .conll) in corpus or det mode";
    "-f", String (fun file -> output_file := Some file), "<output_file>             set the output file where to put generate data (used with det and conll)";
    "-o", String (fun dir -> output_dir := Some dir), "<output_dir>              set the output dir where to generate files: normal forms graphs and/or documentation\n\nOptions for corpus and cluster modes";

    (* options for corpus and cluster mode *)
    "-title", String (fun t -> title := Some t),               "                      set the title for the generated page of statistics";
    "-q", Unit (fun () -> quiet := true), "                          do not print progression percent (for jenkins scripts)";
    "-out_gr",  Unit (fun () -> out_gr := true),         "                     generate gr output files for each rewriting normal form of the corpus";
    "-out_conll",  Unit (fun () -> out_conll := true),         "                  generate conll output files for each rewriting normal form of the corpus\n\nOptions for grep mode";

    (* options for grep mode *)
    "-pattern",  String (fun t -> pattern := Some t),              "<file>              choose the pattern file";
    "-node_id",  String (fun t -> node_id := Some t),              "<node_id>           choose the main node of the pattern";
  ]

  let parse () =
    Arg.parse args (fun s -> Printf.printf "%s" s) usage

end
