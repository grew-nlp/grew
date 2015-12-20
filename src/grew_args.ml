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

  type mode = Gui | Corpus | Cluster | Filter | Index | Det | Full | Annot | Grep
  let mode = ref Gui

  type html_mode =
    | No         (* only stat file is produced *)
    | Html       (* html an png files are produced only when rewriting history is not empty *)
    | Full_html  (* html an png files are always produced *)

  let absolute s =
    if Filename.is_relative s
    then Filename.concat (Sys.getcwd ()) s
    else s

  let grs = ref ""
  let gr = ref ""
  let gui_doc = ref false

  let input_data = ref ""
  let output_dir = ref None
  let output_file = ref None
  let seq = ref "main"
  let no_init = ref false
  let quiet = ref false
  let html = ref No
  let dot = ref false
  let out_gr = ref false
  let out_conll = ref false
  let features = ref None
  let main_feat = ref None
  let timeout = ref None
  let title = ref None
  let pattern = ref None

  let static_dir = ref None

  let start_cluster = ref false
  let cluster_set = ref false
  let graphs = ref []
  let first = ref 0
  let max = ref 0 (* TODO: remove this unused paramater *)

  let oar_nodes = ref 2
  let oar_walltime = ref "1:0:0"
  let oar_graph_by_node = ref 10

  let usage =
    "grew has 4 running modes:\n"^
    "  * GUI MODE: a Gtk interface (this is the default mode)\n"^
    "  * CORPUS MODE: runs rewriting on all graphs of a directory\n"^
    "  * DET MODE: runs a deterministic grs on all graphs of a directory\n"^
    "  * CLUSTER MODE: like corpus mode but to be runned of the cluster talc of the LORIA\n"^
    "\n"^
    "Options for mode selection:"

  let args = [
    "-corpus", Unit (fun () -> mode := Corpus),      "                     enable corpus mode";
    "-det", Unit (fun () -> mode := Det),         "                        enable det mode: rewrite a corpus with a deterministric grs";
    "-full", Unit (fun () -> mode := Full),         "                       enable full mode: rewrite a corpus (conll output)";
    "-cluster", Unit (fun () -> mode := Cluster),         "                    enable cluster mode";
    "-filter", Unit (fun () -> mode := Filter),         "                     enable filter mode";
    "-annot", Unit (fun () -> mode := Annot),         "                      enable annot mode";
    "-grep", Unit (fun () -> mode := Grep),         "                       enable grep mode";
    "-make_index", Unit (fun () -> mode := Index),  "                 just rebuild index.html in <output_dir> from the set of stat files in <input_dir>\n\nOptions for all modes";

    "-grs", String (fun s -> grs := absolute s),          "<grs_file>              chose the grs file to load";
    "-seq", String (fun s -> seq := s),                   "<seq>                   set the module sequence to use";
    "-timeout", Float (fun f -> timeout := Some f; Rewrite.set_timeout (Some f)),                   "<float>             set a timeout on rewriting";
    "-features", String (fun s -> features := Some (Str.split (Str.regexp "; *") s)),            "<feat_name_list>   set the list of feature names to printf in dep format";
    "-main_feat", String (fun s -> main_feat := Some s),       "<feat_name_list>  set the list of feature names used in dep format to set the \"main word\"\n\nOptions for GUI mode";

    (* options for GUI mode *)
    "-gr", String (fun s -> gr := absolute s), "<gr_file>                set the graph file (.gr or .conll) to use";
    "-doc", Unit (fun () -> gui_doc := true), "                        force to build the GRS doc\n\nOptions for corpus, det and cluster modes";

    (* options for corpus, det and cluster mode *)
    "-i", String (fun file -> input_data := absolute file),  "<input_data>              set the input data (file or directory) where to find graph files (.gr or .conll) in corpus or det mode";
    "-f", String (fun file -> output_file := Some (absolute file)), "<output_file>             set the output file where to put generate data (used with det and conll)";
    "-o", String (fun dir -> output_dir := Some (absolute dir)), "<output_dir>              set the output dir where to generate files: normal forms graphs and/or documentation\n\nOptions for corpus and cluster modes";

    (* options for corpus and cluster mode *)
    "-title", String (fun t -> title := Some t),               "                      set the title for the generated page of statistics";
    "-html", Unit (fun () -> html := Html),         "                       generate html files for each rewritten sentence";
    "-full_html", Unit (fun () -> html := Full_html),         "                  generate html files for each sentence of the corpus";
    "-dot", Unit (fun () -> dot := true), "                        use dot to draw solutions, requires html of full_html (default is dep2pict)";
    "-q", Unit (fun () -> quiet := true), "                          do not print progression percent (for jenkins scripts)";
    "-out_gr",  Unit (fun () -> out_gr := true),         "                     generate gr output files for each rewriting normal form of the corpus";
    "-out_conll",  Unit (fun () -> out_conll := true),         "                  generate conll output files for each rewriting normal form of the corpus";
    "-no_init", Unit (fun () -> no_init := true), "                    do not display initial graph (requires html of full_html)\n\nOptions for grep mode";

    (* options for grep mode *)
    "-pattern",  String (fun t -> pattern := Some t),              "<file>              chose the pattern file  \n\nOptions for cluster mode";

    (* options for cluster mode *)
    "-nodes", Int (fun f -> oar_nodes := f),              "<num>                 set the number of nodes to reserve on the cluster (default is 2)";
    "-walltime", String (fun s -> oar_walltime := s),     "<time>             set the reservation time on the cluster (default is \"1:0:0\")";
    "-graph_by_node", Int (fun f -> oar_graph_by_node := f),              "<num>         set the number of graphes sent at the same time to a cluster node (default is 10)\n\nHIDDEN FOR CURRENT USER (do not use)";

    (* Internally used options *)
    "-start_cluster", Unit (fun () -> start_cluster := true),"              require -cluster";
    "-cluster_set", Rest (fun l -> cluster_set := true; graphs := l::(!graphs)),"                require -cluster";
    "-first", Int (fun f -> first := f),                  "                      require -cluster_set";
    "-last", Int (fun f -> max := f),                  "                       require -cluster_set\n\n";

    "-static_dir", String (fun dir -> static_dir := Some dir), "<static_dir>              set the dir where static file can be found in annot mode";

  ]

  let parse () =
    Arg.parse args (fun s -> Printf.printf "%s" s) usage;
IFDEF DEP2PICT THEN
  ()
ELSE
  match !html with
    | No -> ()
    | _ ->
      (match !no_init with
        | true -> ()
        | false -> Log.message "Cannot display init graph without dep2pict --> no_init = true"; no_init := true);
      (match !dot with
        | true -> ()
        | false -> Log.message "Cannot display dep normal forms without dep2pict --> dot = true"; dot := true);
END

end
