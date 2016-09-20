(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Log

module Grew_config = struct

  type config = {
    mutable last_doc_zoom:int;
    mutable last_rewriting_history_zoom:int;
    mutable last_rule_zoom:int;
    mutable last_top_graph_zoom:int;
    mutable last_bottom_graph_zoom:int;

    mutable last_is_dep_top_graph:bool;
    mutable last_is_dep_bottom_graph:bool;

    mutable last_grs_position:int;
    mutable last_module_position:int;

    mutable main_feat:string;
  }

  let default_config = {
    last_doc_zoom = 100;
    last_rewriting_history_zoom = 100;
    last_rule_zoom = 100;
    last_top_graph_zoom = 100;
    last_bottom_graph_zoom = 100;
    last_is_dep_top_graph = true;
    last_is_dep_bottom_graph = true;
    last_grs_position = 400;
    last_module_position = 400;
    main_feat = "phon";
  }

  let current_config = ref default_config

  let read_config_file config_file =
    let config = ref {
      last_doc_zoom = 100;
      last_rewriting_history_zoom = 100;
      last_rule_zoom = 100;
      last_top_graph_zoom = 100;
      last_bottom_graph_zoom = 100;
      last_is_dep_top_graph = true;
      last_is_dep_bottom_graph = true;
      last_grs_position = 400;
      last_module_position = 400;
      main_feat = "phon";
    } in
    let in_ch = open_in config_file in
    try while true do
      let line = input_line in_ch in
      let splitted = Str.split (Str.regexp "=") line in

      let value_str = try (List.nth splitted 1) with _ -> "" in
      let value_int = try int_of_string (List.nth splitted 1) with _ -> 100 in
      let value_bool = try bool_of_string (List.nth splitted 1) with _ -> true in

      match (List.nth splitted 0) with
      | "LAST_DOC_ZOOM" -> config := {!config with last_doc_zoom=value_int}
      | "LAST_REWRITING_HISTORY_ZOOM" -> config := {!config with last_rewriting_history_zoom=value_int}
      | "LAST_RULE_ZOOM" -> config := {!config with last_rule_zoom=value_int}
      | "LAST_TOP_GRAPH_ZOOM" -> config := {!config with last_top_graph_zoom=value_int}
      | "LAST_BOTTOM_GRAPH_ZOOM" -> config := {!config with last_bottom_graph_zoom=value_int}
      | "LAST_IS_DEP_TOP_GRAPH" -> config := {!config with last_is_dep_top_graph=value_bool}
      | "LAST_IS_DEP_BOTTOM_GRAPH" -> config := {!config with last_is_dep_bottom_graph=value_bool}
      | "LAST_GRS_POSITION" -> config := {!config with last_grs_position=value_int}
      | "LAST_MODULE_POSITION" -> config := {!config with last_module_position=value_int}
      | "MAIN_FEAT" -> config := {!config with main_feat=value_str}
      | _ -> ()
    done;
      !config;
    with End_of_file ->
      close_in in_ch;
      !config

  let rec save_config () =
    let config = !current_config in
    let home = Sys.getenv "HOME" in
    let dirname = Filename.concat home ".grew" in
    (* le rep existe *)
    if (Sys.file_exists dirname && Sys.is_directory dirname)
    then
      begin
        try
          (* on peut le lire *)
          Unix.access dirname [Unix.W_OK];
          let config_file = Filename.concat dirname "config.txt" in

          let out_ch = open_out config_file in
          Printf.fprintf out_ch "LAST_DOC_ZOOM=%d\n%!" config.last_doc_zoom;
          Printf.fprintf out_ch "LAST_REWRITING_HISTORY_ZOOM=%d\n%!" config.last_rewriting_history_zoom;
          Printf.fprintf out_ch "LAST_RULE_ZOOM=%d\n%!" config.last_rule_zoom;
          Printf.fprintf out_ch "LAST_TOP_GRAPH_ZOOM=%d\n%!" config.last_top_graph_zoom;
          Printf.fprintf out_ch "LAST_BOTTOM_GRAPH_ZOOM=%d\n%!" config.last_bottom_graph_zoom;
          Printf.fprintf out_ch "LAST_IS_DEP_TOP_GRAPH=%b\n%!" config.last_is_dep_top_graph;
          Printf.fprintf out_ch "LAST_IS_DEP_BOTTOM_GRAPH=%b\n%!" config.last_is_dep_bottom_graph;
          Printf.fprintf out_ch "LAST_GRS_POSITION=%d\n%!" config.last_grs_position;
          Printf.fprintf out_ch "LAST_MODULE_POSITION=%d\n%!" config.last_module_position;
          Printf.fprintf out_ch "MAIN_FEAT=%s\n%!" config.main_feat;
          close_out out_ch
        with
          (* on ne peut pas ecrire dans le fichier de config *)
          | Unix.Unix_error (_,_,_) ->
            Log.fwarning "Can't write the config dir (%s). Your config was not saved!" dirname
      end
    else (* sinon le repertoire n'existe pas, on le crée et on recommence *)
      begin
        ignore(Sys.command("mkdir -p "^dirname));
        save_config ()
      end

  let rec read_config () =
    try
      let home = Sys.getenv "HOME" in
      let dirname = Filename.concat home ".grew" in
      (* le rep existe *)
      if (Sys.file_exists dirname && Sys.is_directory dirname)
      then
        begin
          try
            (* on peut le lire *)
            Unix.access dirname [Unix.R_OK];
            let config_file = Filename.concat dirname "config.txt" in
            (* le fichier de config existe *)
            if (Sys.file_exists config_file)
            then
              begin
                (* on le lit *)
                let config = read_config_file config_file in
                (* si on ne paut pas ecrire dans le repertoire, on en informe l'utilisateur *)
                (
                  try Unix.access dirname [Unix.W_OK]
                  with Unix.Unix_error (_,_,_) -> Log.warning "Can't write in config filder, you will not be able to save your config!"
                );
                current_config := config
              end
            else
              begin
                (* on enregistre une config par defaut et on l'utilise *)
                Log.warning "The config file is missing, will create an empty config file in config folder and use a last config!";
                current_config := default_config; save_config ();
              end
          with
            (* on ne peut pas le lire, on utilise la config par defaut *)
            | Unix.Unix_error (_,_,_) ->
              Log.fwarning "Can't read the config dir (%s). Will use a last config!" dirname;
              current_config := default_config
        end
      else
        begin
          (* on le crée, puis on recommence la recherche du fichier de config *)
          ignore (Sys.command("mkdir -p "^dirname));
          read_config ();
        end
    with
      | Not_found ->
        Log.warning "No HOME variable in your environment: configuration file disabled";
        current_config := default_config

  let print_config config =
    Printf.printf "LAST_DOC_ZOOM=%d\n%!" config.last_doc_zoom;
    Printf.printf "LAST_REWRITING_HISTORY_ZOOM=%d\n%!" config.last_rewriting_history_zoom;
    Printf.printf "LAST_RULE_ZOOM=%d\n%!" config.last_rule_zoom;
    Printf.printf "LAST_TOP_GRAPH_ZOOM=%d\n%!" config.last_top_graph_zoom;
    Printf.printf "LAST_BOTTOM_GRAPH_ZOOM=%d\n%!" config.last_bottom_graph_zoom;
    Printf.printf "LAST_IS_DEP_TOP_GRAPH=%b\n%!" config.last_is_dep_top_graph;
    Printf.printf "LAST_IS_DEP_BOTTOM_GRAPH=%b\n%!" config.last_is_dep_bottom_graph;
    Printf.printf "LAST_GRS_POSITION=%d\n%!" config.last_grs_position;
    Printf.printf "LAST_MODULE_POSITION=%d\n%!" config.last_module_position;
    Printf.printf "MAIN_FEAT=%s\n%!" config.main_feat;
end
