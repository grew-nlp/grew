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

open Grew_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)

let fail msg =
  let rule = String.make (String.length msg) '=' in
  Log.fwarning "\n%s\n%s\n%s" rule msg rule; exit 2

let handle fct () =
  try fct ()
  with
    | Conll_types.Error json ->      fail (Yojson.Basic.pretty_to_string json)
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
      | (_,_,None) -> Log.message "No output specified: use -o option"; exit 1
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
        | [one] -> fprintf out_ch "%s\n" (Graph.to_conll_string one)
        | l ->
          List.iteri (fun i gr ->
            let conll = Graph.to_conll gr in
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

      (match !Grew_args.dep_dir with
      | None -> ()
      | Some d -> ignore (Sys.command (sprintf "mkdir -p %s" d)));

      (* printf "%s\n" (String.concat "_" (Pattern.pid_name_list pattern)); *)
      let pattern_ids = Pattern.pid_name_list pattern in

      Array.iter
        (fun (name,graph) ->
          let matchings = Graph.search_pattern ?domain pattern graph in
            List.iter
              (fun matching ->
                let node_matching = Graph.node_matching pattern graph matching in
                let graph_node_ids = List.map snd node_matching in
                let deco = Deco.build pattern matching in
                let html = Graph.to_sentence ~deco graph in
                let id = sprintf "%s__%s"
                  name
                  (String.concat "_" (List.map2 (sprintf "%s:%g") pattern_ids graph_node_ids)) in
                (* let graph_node_id = List.assoc node_id node_matching in *)
                (* printf "%s\t%g\n" name graph_node_id; *)
                printf "%s@@%s\n" id html;
                (match !Grew_args.dep_dir with
                | None -> ()
                | Some dir ->
                  let dep = Graph.to_dep ~deco graph in
                  let filename = sprintf "%s.dep" id in
                  let out_ch = open_out (Filename.concat dir filename) in
                  fprintf out_ch "%s" dep;
                  close_out out_ch
                )
              ) matchings
        ) graph_array
    ) ()
