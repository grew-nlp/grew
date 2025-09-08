(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Grewlib

open Grew_cli_utils

(* -------------------------------------------------------------------------------- *)
let dep_counter = ref 0
let json_of_matching ~config request sent_id graph matching =
  let dep_file =
    match !Grew_cli_global.dep_dir with
    | None -> None
    | Some dir ->
      let deco = Matching.build_deco request matching in
      incr dep_counter;
      let id = sprintf "g_%05d" !dep_counter in
      let dep = Graph.to_dep ~config ~deco graph in
      let filename = Filename.concat dir (sprintf "%s.dep" id) in
      let out_ch = open_out filename in
      fprintf out_ch "%s" dep;
      close_out out_ch;
      Some ("dep_file", `String filename) in

  `Assoc (CCList.filter_map (fun x -> x)
    [
      Some ("sent_id", `String sent_id);
      Some ("matching", Matching.to_json request graph matching);
      (
        if !Grew_cli_global.html
        then 
          let deco = Matching.build_deco request matching in
          Some ("html", `String (Graph.to_sentence ~deco graph))
        else None
      );
      dep_file
      ])
  
(* -------------------------------------------------------------------------------- *)
let grep () =
  match !Grew_cli_global.requests with
  | [request_file] ->
    (match !Grew_cli_global.dep_dir with
    | None -> ()
    | Some d -> ignore (Sys.command (sprintf "mkdir -p %s" d)));

    let clustered_corpus ~config corpus =
      let request = request_load_or_parse ~config request_file in
      let clustert_item2_list = List.map (Request.parse_cluster_item ~config request) !Grew_cli_global.clustering in
      Corpus.search
        ~config 
        [] 
        (fun sent_id graph matching acc -> 
          (json_of_matching ~config request sent_id graph matching) :: acc
        )
        request 
        clustert_item2_list
        corpus in

    let clustered = match Input.parse () with
      | Mono corpus -> clustered_corpus ~config:!Grew_cli_global.config corpus
      | Multi corpus_desc_list -> 
        Clustered.build_layer
        (fun corpus_desc ->
          let config = Corpus_desc.get_config corpus_desc in 
          match Corpus_desc.load_corpus_opt corpus_desc with
            | None -> error ~fct:"Grew.count" "cannot load corpus `%s`" (Corpus_desc.get_id corpus_desc)
            | Some corpus -> clustered_corpus ~config corpus
        )
        (fun corpus_desc -> Some (Corpus_desc.get_id corpus_desc))
        corpus_desc_list in

    let final_json = Clustered.fold_layer
      []
      (fun json_list -> `List json_list)
      []
      (fun string_opt sub acc -> (CCOption.get_or ~default:"__undefined__" string_opt, sub) :: acc)
      (fun x -> `Assoc x)
      clustered in
    Printf.printf "%s\n" (Yojson.Basic.pretty_to_string final_json)

  | l -> Log.echo_help := true; error ~fct:"Grew.grep" "exactly one request required for grep mode (%d given)" (List.length l)
