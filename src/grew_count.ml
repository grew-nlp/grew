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

open Grew_cli_global
open Grew_cli_utils

(* -------------------------------------------------------------------------------- *)
let count () =
  let clustered_corpus ~config corpus = 
    Clustered.build_layer
    (fun file_request -> 
      let request = request_load_or_parse ~config file_request in
      let clustert_item2_list = List.map (Request.parse_cluster_item ~config request) !Grew_cli_global.clustering in
      Corpus.search ~config 0 (fun _ _ _ x -> x+1) request clustert_item2_list corpus
    )
    (fun file_request -> Some file_request)
    !Grew_cli_global.requests in

  let input = Input.parse () in
  let count_clustered = match input with
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
  match (!Grew_cli_global.output, input, !Grew_cli_global.requests, !Grew_cli_global.clustering) with

    (* TSV + Multi + Several requests + No clustering *)
    | (Tsv, Multi corpus_desc_list, _, []) ->
      printf "Corpus";
      List.iter (fun p -> printf "\t%s" (p |> Filename.basename |> Filename.remove_extension)) !Grew_cli_global.requests;
      printf "\n";
      List.iter
        (fun corpus_desc -> 
          let corpus_id = Corpus_desc.get_id corpus_desc in
          printf "%s" corpus_id;

          List.iter
            (fun p -> printf "\t%d" (Clustered.get_opt 0 [Some corpus_id; Some p] count_clustered)
            ) !Grew_cli_global.requests;
          printf "\n";
        ) corpus_desc_list

    (* TSV + Multi + One request + One clustering   --> count each cluster in each corpus *)
    | (Tsv, Multi corpus_desc_list, [pat], [_]) ->
      let all_keys = Clustered.get_all_keys 2 count_clustered in
        printf "Corpus";
        List.iter (fun k -> printf "\t%s" (CCOption.map_or ~default:"__undefined__" CCFun.id k)) all_keys;
        printf "\n";
        List.iter (
          fun corpus_desc ->
            let corpus_id = Corpus_desc.get_id corpus_desc in
            printf "%s" corpus_id;
            List.iter
              (fun key ->
                printf "\t%d" (Clustered.get_opt 0 [Some corpus_id; Some pat; key] count_clustered)
              ) all_keys;
            printf "\n%!"
        ) corpus_desc_list;

     (* TSV + Mono + Several requests + No clustering *)
     | (Tsv, Mono _, _, []) ->
      let req_ids = Clustered.get_all_keys 0 count_clustered in
      List.iter 
        (fun req_id -> printf "%s\t%d\n%!"
          (match req_id with None -> "__undefined__" | Some s -> s)
          (Clustered.get_opt 0 [req_id] count_clustered)
        ) req_ids

     (* TSV + Mono + Several requests + One clustering *)
     | (Tsv, Mono _, _, [_]) ->
      let req_ids = Clustered.get_all_keys 0 count_clustered in
      let clust_values = Clustered.get_all_keys 1 count_clustered in
      printf "Request\t%s\n" (String.concat "\t" (List.map (CCOption.get_or ~default: "__undefined__") clust_values));
      List.iter 
        (fun req_id -> printf "%s\t%s\n%!"
          (CCOption.get_or ~default: "__undefined__" req_id)
          (String.concat "\t" (List.map (fun v -> string_of_int (Clustered.get_opt 0 [req_id; v] count_clustered)) clust_values))
        ) req_ids

      (* TSV + Mono + One request + Two clusterings *)
     | (Tsv, Mono _, [_], [_; _]) ->
      let req_id = List.hd (Clustered.get_all_keys 0 count_clustered) in
      let clust_values_1 = Clustered.get_all_keys 1 count_clustered in
      let clust_values_2 = Clustered.get_all_keys 2 count_clustered in
      printf "Clust_1\\Clust_2\t%s\n" (String.concat "\t" (List.map (CCOption.get_or ~default: "__undefined__") clust_values_2));
      List.iter 
        (fun v1 -> printf "%s\t%s\n%!"
          (CCOption.get_or ~default: "__undefined__" v1)
          (String.concat "\t" (List.map (fun v2 -> string_of_int (Clustered.get_opt 0 [req_id; v1; v2] count_clustered)) clust_values_2))
        ) clust_values_1

    | (Tsv, _, _, _) -> 
          Log.warning 
          "The `tsv` ouput is not available with the given arguments.
         See https://grew.fr/usage/cli/#count for more details."
    
    (* JSON output *)
    | _ ->
      let json = Clustered.fold_layer
        0
        (fun x -> `Int x)
        []
        (fun string_opt sub acc -> (CCOption.get_or ~default:"__undefined__" string_opt, sub) :: acc)
        (fun x -> `Assoc x)
        count_clustered in
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json)
