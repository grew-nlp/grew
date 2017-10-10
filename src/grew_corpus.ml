(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Log
open Conll

open Libgrew
open Dep2pict

open Grew_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)

let fail msg =
  let rule = String.make (String.length msg) '=' in
  Log.fwarning "\n%s\n%s\n%s" rule msg rule; exit 2

let handle fct () =
  try fct ()
  with
    | Libgrew.Error msg ->           fail msg
    | Corpus.File_not_found file ->  fail (sprintf "File not found: \"%s\"" file)
    | Corpus.Fail msg ->             fail msg

    | Libgrew.Bug msg ->             fail (sprintf "Libgrew.bug, please report: %s" msg)
    | exc ->                         fail (sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))


(* -------------------------------------------------------------------------------- *)
let transform () =
  handle (fun () ->
    match (!Grew_args.grs, !Grew_args.input_data, !Grew_args.output_file) with
      | (None,_,_) -> Log.message "No grs filespecified: use -grs option"; exit 1
      | (_,None,_) -> Log.message "No input data specified: use -i option"; exit 1
      | (_,_,None) -> Log.message "No output specified: use -f option"; exit 1
      | (Some grs_file, Some input, Some output_file) ->
      let out_ch = open_out output_file in
      let grs = (if !Grew_args.old_grs then Grs.load_old grs_file else Grs.load grs_file) in
      let domain = Grs.domain grs in


    (* get the list of files to rewrite *)
    let graph_array = Corpus.get_graphs ?domain input in
    let len = Array.length graph_array in

    Array.iteri
      (fun index (id, gr) ->
        Counter.print index len id;
        match Rewrite.simple_rewrite ~gr ~grs ~strat:!Grew_args.strat with
        | [one] -> fprintf out_ch "%s\n" (Graph.to_conll_string ?domain one)
        | l ->
          List.iteri (fun i gr ->
            let conll = Graph.to_conll ?domain gr in
            let conll_new_id = Conll.set_sentid (sprintf "%s_%d" id i) conll in
            fprintf out_ch "%s\n" (Conll.to_string conll_new_id)
            ) l
      ) graph_array;
    close_out out_ch;
    Counter.finish ()
  ) ()

(* -------------------------------------------------------------------------------- *)
  let grep () = handle
    (fun () ->
      match (!Grew_args.input_data, !Grew_args.pattern, !Grew_args.node_id) with
      | (None,_,_) -> Log.message "No input data specified: use -i option"; exit 1
      | (_,None,_) -> Log.message "No pattern file specified: use -pattern option"; exit 1;
      | (_,_,None) -> Log.message "No node_id specified: use -node_id option"; exit 1;
      | (Some data_file, Some pattern_file, Some node_id) ->

      let domain = match !Grew_args.grs with
      | None -> None
      | Some file -> Grs.domain (if !Grew_args.old_grs then Grs.load_old file else Grs.load file) in

      let pattern = Pattern.load ?domain pattern_file in

      if not (List.mem node_id (Pattern.pid_name_list pattern))
      then (Log.fmessage "The requested node_id \"%s\" is not defined in the pattern" node_id; exit 1)
      else

      (* get the array of graphs to explore *)
      let graph_array = Corpus.get_graphs ?domain data_file in

      match !Grew_args.output_dir with
        | None ->
            Array.iter
              (fun (name,graph) ->
                let matchings = Graph.search_pattern ?domain pattern graph in
                  List.iter
                    (fun matching ->
                      let node_matching = Graph.node_matching pattern graph matching in
                      let graph_node_id = List.assoc node_id node_matching in
                      printf "%s\t%g\n" name graph_node_id
                    ) matchings
              ) graph_array
        | Some dir ->
            if Sys.file_exists dir && not (Sys.is_directory dir)
            then (Log.fmessage "\"%s\" is a file" dir; exit 1);

            if not (Sys.file_exists dir) then Unix.mkdir dir 0o755;

            let buff = Buffer.create 32 in
            Array.iter
              (fun (name,graph) ->
                let matchings = Graph.search_pattern ?domain pattern graph in
                  List.iter
                    (fun matching ->
                      let node_matching = Graph.node_matching pattern graph matching in
                      let graph_node_id = List.assoc node_id node_matching in
                      let filename = Filename.concat dir (sprintf "%s__%g.svg" name graph_node_id) in
                      if Sys.file_exists filename
                        then (Log.message "Try to erase a previous file"; exit 1)
                        else
                          begin
                            let deco = Deco.build pattern matching in
                            let dep = Graph.to_dep ?domain ~deco graph in
                            let _ = Dep2pict.save_svg ~filename (Dep2pict.from_dep ~dep) in
                            let shift = Dep2pict.highlight_shift () in
                            bprintf buff "%s@%f\n" (Filename.basename filename) (match shift with None -> 0. | Some v -> v)
                          end
                    ) matchings
              ) graph_array;
              let out_ch = open_out (Filename.concat dir "list") in
              fprintf out_ch "%s" (Buffer.contents buff);
              close_out out_ch
    ) ()
