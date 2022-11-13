(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2021 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Printf
open Conllx
open Grew_types
open Libgrew

open Grew_cli_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)
type input =
  | Multi of Corpus_desc.t list
  | Mono of Corpus.t


let parse_input () =
  let config = !Grew_args.config in
  match !Grew_args.input_data with
  | [] -> Mono (Corpus.from_stdin ~config ())
  | l -> 
      try Multi (CCList.flat_map Corpus_desc.load_json !Grew_args.input_data)
      with Libgrew.Error _ ->
        (* TODO add specific error for compile/ clean *)
        match l with
        | [one] ->
          begin
            try
              match Unix.stat one with
              | { Unix.st_kind = Unix.S_DIR } -> Mono (Corpus.from_dir ~config one)
              | _ -> Mono (Corpus.from_file ~config one)
            with Unix.Unix_error _ -> error ~fct:"Grew.parse_input" "File not found `%s`" one
          end
        | files ->
          let sub_corpora =
            List.fold_left
              (fun acc file ->
                 try
                   let subcorpus = Corpus.from_file ~config file in
                   subcorpus :: acc
                 with Unix.Unix_error _ -> error ~fct:"Grew.parse_input" "File not found `%s`" file
              ) [] files in
          Mono (Corpus.merge sub_corpora)

(* -------------------------------------------------------------------------------- *)
let transform () =
  let config = !Grew_args.config in
  let grs = match !Grew_args.grs with
    | None -> Grs.empty
    | Some file -> Grs.load ~config file in

  let corpus = match parse_input () with
  | Mono c -> c
  | Multi _ -> error ~fct:"Grew.transform" "transform mode cannot be used with multi-corpora input data" in

  let len = Corpus.size corpus in

  let out_ch = match !Grew_args.output_data with
    | Some output_file -> open_out output_file
    | None -> stdout in

  let (next_graph, final) = match !Grew_args.output with
    | Grew_args.Conllx columns ->
      fprintf out_ch "%s\n" (Conllx_columns.to_string columns);
      (
        (fun graph -> fprintf out_ch "%s\n" (graph |> Graph.to_json |> Conllx.of_json |> Conllx.to_string ~config ~columns)),
        (fun () -> ())
      )
    | Grew_args.Dot ->
      let flag = ref false in
      let buff = Buffer.create 32 in
      (
        (fun graph ->
          if !flag
          then (error ~fct:"Grew.transform" "`dot` output cannot be used when there is more than one graph in the output")
          else (flag := true; bprintf buff "%s" (Graph.to_dot ~config graph))),
        (fun () -> fprintf out_ch "%s\n" (Buffer.contents buff))
      )
    | Grew_args.Json ->
      let data = ref [] in
      (
        (fun graph -> data := (graph |> Graph.to_json) :: !data),
        (fun () ->
          let json = match !data with
            | [one] -> one
            | _ -> `List (List.rev !data) in
          fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string json))
      )
    | Grew_args.Multi_json ->
      let data = ref [] in
        (
          (fun graph -> data := (graph |> Graph.to_json) :: !data),
          (fun () -> 
            match (!Grew_args.output_data, !data) with
              | (None,_) -> error ~fct:"Grew.transform" "-multi_json implies -o"
              | (Some out_file, l) ->
                let base = match Filename.chop_suffix_opt ~suffix:".json" out_file with
                  | Some b -> b
                  | None -> out_file in
                List.iteri
                  (fun i json -> 
                     let sub_out_ch = open_out (sprintf "%s__%d.json" base i) in
                     fprintf sub_out_ch "%s\n" (Yojson.Basic.pretty_to_string json)
                  ) l)
        )
    | Tsv -> error ~fct:"Grew.transform" "`tsv` output cannot be used in transform mode" in

  Corpus.iteri
    (fun index sent_id gr ->
      Counter.print index len sent_id;
      match Rewrite.simple_rewrite ~config gr grs !Grew_args.strat with
        | [one] -> next_graph one
        | l -> List.iteri (fun i graph -> next_graph (Graph.set_meta "sent_id" (sprintf "%s_%d" sent_id i) graph)) l
    ) corpus;
    Counter.finish ();
    final ();
    match !Grew_args.output_data with
      | Some output_file -> close_out out_ch
      | None -> ()

(* -------------------------------------------------------------------------------- *)
let grep () =
  match !Grew_args.requests with
  | [request_file] ->
    let clustered_corpus ~config corpus =
      let request = Request.load ~config request_file in
      Corpus.search 
        ~config 
        [] 
        (fun sent_id graph matching acc -> `Assoc [
          ("sent_id", `String sent_id);
          ("matching", Matching.to_json request graph matching)
          ] :: acc
        )
        request 
        !Grew_args.clustering 
        corpus in

    let clustered = match parse_input () with
      | Mono corpus -> clustered_corpus ~config:!Grew_args.config corpus
      | Multi corpus_desc_list -> 
        Clustered.build_layer
        (fun corpus_desc ->
          let config = Corpus_desc.get_config corpus_desc in 
          match Corpus_desc.load_corpus_opt corpus_desc with
            | None -> error ~fct:"Grew.count" "cannot load corpus `%s`" (Corpus_desc.get_id corpus_desc)
            | Some corpus -> clustered_corpus ~config corpus
        )
        (fun corpus_desc -> Some (Corpus_desc.get_id corpus_desc))
        [] corpus_desc_list in

    let final_json = Clustered.fold_layer
      (fun json_list -> `List json_list)
      []
      (fun string_opt sub acc -> (CCOption.get_or ~default:"__undefined__" string_opt, sub) :: acc)
      (fun x -> `Assoc x)
      clustered in
    Printf.printf "%s\n" (Yojson.Basic.pretty_to_string final_json)

  | l -> error ~fct:"Grew.grep" "1 request expected for grep mode (%d given)" (List.length l)

(* -------------------------------------------------------------------------------- *)
let compile () =
  let corpus_desc_list = match parse_input () with
  | Mono _ -> error ~fct:"Grew.compile" "compile mode requires multi-corpora input data"
  | Multi l -> l in
  List.iter
    (fun corpus_desc ->
      Corpus_desc.compile ~force:!Grew_args.force ?grew_match:!Grew_args.grew_match_server corpus_desc
    ) corpus_desc_list

(* -------------------------------------------------------------------------------- *)
let clean () =
  let corpus_desc_list = match parse_input () with
  | Mono _ -> error ~fct:"Grew.clean" "clean mode requires multi-corpora input data"
  | Multi l -> l in
  List.iter
    (fun corpus_desc ->
      Corpus_desc.clean corpus_desc
    ) corpus_desc_list

(* -------------------------------------------------------------------------------- *)
let count () =

  let clustered_corpus ~config corpus = 
    Clustered.build_layer
    (fun file_request -> 
      let request = Request.load ~config file_request in
      Corpus.search ~config 0 (fun _ _ _ x -> x+1) request !Grew_args.clustering corpus
    )
    (fun file_request -> Some file_request)
    0 !Grew_args.requests in

  let input = parse_input () in
  let count_clustered = match input with
    | Mono corpus -> clustered_corpus ~config:!Grew_args.config corpus
    | Multi corpus_desc_list ->
    Clustered.build_layer
      (fun corpus_desc ->
        let config = Corpus_desc.get_config corpus_desc in 
        match Corpus_desc.load_corpus_opt corpus_desc with
          | None -> error ~fct:"Grew.count" "cannot load corpus `%s`" (Corpus_desc.get_id corpus_desc)
          | Some corpus -> clustered_corpus ~config corpus
      )
      (fun corpus_desc -> Some (Corpus_desc.get_id corpus_desc))
      0 corpus_desc_list in
  match (!Grew_args.output, input, !Grew_args.requests, !Grew_args.clustering) with

    (* TSV + Multi + Several requests + No clustering *)
    | (Grew_args.Tsv, Multi corpus_desc_list, _, []) ->
      printf "Corpus\t# sentences";
      List.iter (fun p -> printf "\t%s" (p |> Filename.basename |> Filename.remove_extension)) !Grew_args.requests;
      printf "\n";
      List.iter
        (fun corpus_desc -> 
          let corpus_id = Corpus_desc.get_id corpus_desc in
          printf "%s" corpus_id;

          (* reloading of the corpus in order to get the size: TODO change grew count output to avoid this. *)
          match Corpus_desc.load_corpus_opt corpus_desc with
            | None -> error ~fct:"Grew.count" "The corpus %s is not compiled" (Corpus_desc.get_id corpus_desc)
            | Some data -> printf "\t%d" (Corpus.size data);

          List.iter
            (fun p -> printf "\t%d" (Clustered.get_opt 0 [Some corpus_id; Some p] count_clustered)
            ) !Grew_args.requests;
          printf "\n";
        ) corpus_desc_list

    (* TSV + Multi + One request + One clustering   --> count each cluster in each corpus *)
    | (Grew_args.Tsv, Multi corpus_desc_list, [pat], [_]) ->
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
     | (Grew_args.Tsv, Mono _, _, []) ->
      let req_ids = Clustered.get_all_keys 0 count_clustered in
      List.iter 
        (fun req_id -> printf "%s\t%d\n%!"
          (match req_id with None -> "__undefined__" | Some s -> s)
          (Clustered.get_opt 0 [req_id] count_clustered)
        ) req_ids

     (* TSV + Mono + Several requests + One clustering *)
     | (Grew_args.Tsv, Mono _, _, [_]) ->
      let req_ids = Clustered.get_all_keys 0 count_clustered in
      let clust_values = Clustered.get_all_keys 1 count_clustered in
      printf "Request\t%s\n" (String.concat "\t" (List.map (CCOption.get_or ~default: "__undefined__") clust_values));
      List.iter 
        (fun req_id -> printf "%s\t%s\n%!"
          (CCOption.get_or ~default: "__undefined__" req_id)
          (String.concat "\t" (List.map (fun v -> string_of_int (Clustered.get_opt 0 [req_id; v] count_clustered)) clust_values))
        ) req_ids

      (* TSV + Mono + One request + Two clusterings *)
     | (Grew_args.Tsv, Mono _, [_], [_; _]) ->
      let req_id = List.hd (Clustered.get_all_keys 0 count_clustered) in
      let clust_values_1 = Clustered.get_all_keys 1 count_clustered in
      let clust_values_2 = Clustered.get_all_keys 2 count_clustered in
      printf "Clust_1\\Clust_2\t%s\n" (String.concat "\t" (List.map (CCOption.get_or ~default: "__undefined__") clust_values_2));
      List.iter 
        (fun v1 -> printf "%s\t%s\n%!"
          (CCOption.get_or ~default: "__undefined__" v1)
          (String.concat "\t" (List.map (fun v2 -> string_of_int (Clustered.get_opt 0 [req_id; v1; v2] count_clustered)) clust_values_2))
        ) clust_values_1

    | (Grew_args.Tsv, _, _, _) -> 
          Log.warning 
          "The `tsv` ouput is not available with the given arguments.
         See https://grew.fr/usage/cli/#count for more details."
    
    (* JSON output *)
    | _ ->
      let json = Clustered.fold_layer
        (fun x -> `Int x)
        []
        (fun string_opt sub acc -> (CCOption.get_or ~default:"__undefined__" string_opt, sub) :: acc)
        (fun x -> `Assoc x)
        count_clustered in
      Printf.printf "%s\n" (Yojson.Basic.pretty_to_string json)

(* -------------------------------------------------------------------------------- *)
let stat () =
  let corpus_desc_list = match parse_input () with
  | Mono _ -> error ~fct:"Grew.stat" "stat mode requires multi-corpora input data"
  | Multi l -> l in
  match !Grew_args.requests with
    | [] -> Log.warning "No request given (expected one json file with requests)"
    | _::_::_ -> Log.warning "Too much requests given (expected one json file with requests)"
    | [one] ->
      let (pat_descs, json) = Stat.load_json one in
      let bare_lines =
        List.map
          (fun corpus_desc ->
            match Corpus_desc.load_corpus_opt corpus_desc with
              | None -> error ~fct:"Grew.stat" "The corpus %s is not compiled" (Corpus_desc.get_id corpus_desc)
              | Some corpus -> 
                let config = Corpus_desc.get_config corpus_desc in
                (
                  Corpus_desc.get_id corpus_desc,
                  List.map (
                    fun pat_desc ->
                      let request = 
                        (* NB: request should be reparsed for each corpora, because config may change *)
                        try Request.parse ~config (String.concat " " pat_desc.Stat.code)
                        with Libgrew.Error msg ->
                          error
                            ~fct:"Grew.stat"
                            ~data:(`String (String.concat " " pat_desc.Stat.code))
                            "cannot parse request associated with desc: %s" pat_desc.Stat.desc in
                      Corpus.fold_left 
                        (fun acc _ graph ->
                          acc + (List.length (Matching.search_request_in_graph ~config request graph))
                        ) 0 corpus
                  ) pat_descs
                )
          ) corpus_desc_list in

      let stats = `List (
         List.map (fun (desc, occs) -> `List (`String desc :: (List.map (fun i -> `Int i) occs))) bare_lines
      ) in

      let ratio = `List (
        List.map (fun (desc, occs) -> `List (`String desc :: (Stat.compute_ratios occs))) bare_lines
        ) in

     let final_json = `Assoc [
        ("requests", json);
        ("stats", stats);
        ("ratio", ratio);
      ] in
      match !Grew_args.output_data with
       | None -> printf "%s\n" (Yojson.Basic.pretty_to_string final_json)
       | Some f -> Yojson.Basic.to_file f final_json

(* -------------------------------------------------------------------------------- *)
let valid () =
  let corpus_desc_list = match parse_input () with
  | Mono _ -> error ~fct:"Grew.valid" "valid mode requires multi-corpora input data"
  | Multi l -> l in
  match !Grew_args.output_data with
    | None -> error ~fct:"valid" "an output directory is required (use -o option)"
    | Some dir ->
      ensure_dir dir;
      let validator_list = List.map Validation.load_json !Grew_args.requests in
      List.iter
        (fun corpus_desc ->
          if not !quiet then printf "%s\n" (Corpus_desc.get_id corpus_desc);
          Validation.check ~dir validator_list corpus_desc
        ) corpus_desc_list

(* -------------------------------------------------------------------------------- *)
let _ =
  Printexc.record_backtrace true;

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Undefined -> ()
  | Grew_args.Transform -> handle transform ()
  | Grew_args.Grep -> handle grep ()
  | Grew_args.Compile -> handle compile ()
  | Grew_args.Clean -> handle clean ()
  | Grew_args.Count-> handle count ()
  | Grew_args.Stat-> handle stat ()
  | Grew_args.Valid-> handle valid ()
  | Grew_args.Test -> failwith "No test defined"

