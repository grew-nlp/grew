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
let load_corpus_desc_list () = CCList.flat_map Corpus_desc.load_json !Grew_args.input_data

(* -------------------------------------------------------------------------------- *)
let load_corpus () =
  let config = !Grew_args.config in
  match !Grew_args.input_data with
  | [] -> Corpus.from_stdin ~config ()
  | [one] ->
    begin
      try
        match Unix.stat one with
        | { Unix.st_kind = Unix.S_DIR } -> Corpus.from_dir ~config one
        | _ -> Corpus.from_file ~config one
      with Unix.Unix_error _ -> Log.fail "%s" (sprintf "File not found `%s`" one)
    end
  | files ->
    let sub_corpora =
      List.fold_left
        (fun acc file ->
           try
             let subcorpus = Corpus.from_file ~config file in
             subcorpus :: acc
           with Unix.Unix_error _ -> Log.fail "%s" (sprintf "File not found `%s`" file)
        ) [] files in
    Corpus.merge sub_corpora

(* -------------------------------------------------------------------------------- *)
let transform () =
  let config = !Grew_args.config in
  let grs = match !Grew_args.grs with
    | None -> Grs.empty
    | Some file -> Grs.load ~config file in

  let corpus = load_corpus () in
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
          then (Log.fail "`dot` output cannot be used when there is more than one graph in the output")
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
              | (None,_) -> Log.fail "%s" "-multi_json implies -o"
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
    | Tsv -> Log.fail "`tsv` output cannot be used in transform mode" in

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
  let config = !Grew_args.config in
  match !Grew_args.patterns with
  | [pattern_file] ->
    let pattern = Pattern.load ~config pattern_file in
    let build_key_list graph matching =
      List.map 
        (fun clust_item -> Matching.get_clust_value_opt ~config clust_item pattern graph matching)
        !Grew_args.clustering in
    let corpus = load_corpus () in
    let clustered =
      Corpus.fold_left
        (fun acc name graph ->
          let matchings = Matching.search_pattern_in_graph ~config pattern graph in
          List.fold_left
            (fun acc2 matching ->
              let key_list = build_key_list graph matching in
              let json_matching = `Assoc
              [
                ("sent_id", `String name);
               ("matching", Matching.to_json pattern graph matching)
              ] in
              Clustered.update (fun x -> json_matching :: x) key_list [] acc2
            ) acc matchings
        ) (Clustered.empty []) corpus in
    let final_json = Clustered.fold_layer
      (fun json_list -> `List json_list)
      []
      (fun string_opt sub acc -> (CCOption.get_or ~default:"__undefined__" string_opt, sub) :: acc)
      (fun x -> `Assoc x)
      clustered in
    Printf.printf "%s\n" (Yojson.Basic.pretty_to_string final_json)
  | l -> Log.fail "1 pattern expected for grep mode (%d given)" (List.length l)

(* -------------------------------------------------------------------------------- *)
let compile () =
  List.iter
    (fun corpus_desc ->
      Corpus_desc.compile ~force:!Grew_args.force ?grew_match:!Grew_args.grew_match_server corpus_desc
    ) (load_corpus_desc_list ())

(* -------------------------------------------------------------------------------- *)
let clean () =
  List.iter
    (fun corpus_desc ->
      Corpus_desc.clean corpus_desc
    ) (load_corpus_desc_list ())

(* -------------------------------------------------------------------------------- *)
let count () =
  let corpus_desc_list = load_corpus_desc_list () in
  let count_clustered = 
    Clustered.build_layer
      (fun corpus_desc ->
        let config = Corpus_desc.get_config corpus_desc in 
        match Corpus_desc.load_corpus_opt corpus_desc with
          | None -> failwith "TODO: cannot get corpus"
          | Some corpus -> 
            Clustered.build_layer
              (fun file_pattern -> 
                let pattern = Pattern.load ~config file_pattern in
                Corpus.search ~config 0 (fun _ x -> x+1) pattern !Grew_args.clustering corpus
              )
              (fun file_pattern -> Some file_pattern)
              0 !Grew_args.patterns 
      )
      (fun corpus_desc -> Some (Corpus_desc.get_id corpus_desc))
      0 corpus_desc_list in
  match (!Grew_args.output, !Grew_args.patterns, !Grew_args.clustering) with
    | (Grew_args.Tsv, _, []) ->
      printf "Corpus\t# sentences";
      List.iter (fun p -> printf "\t%s" (p |> Filename.basename |> Filename.remove_extension)) !Grew_args.patterns;
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
            ) !Grew_args.patterns;
          printf "\n";
        ) corpus_desc_list

    (* with clustering --> one pattern only, count each cluster in each corpus *)
    | (Grew_args.Tsv, [pat], [cluster_item]) ->
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

    | (Grew_args.Tsv, _,_) -> 
          Log.warning 
          "The `tsv` ouput is not available with the current request.
         It is avaible with: (one pattern, one clutering item) or with (several patterns, no clustering_item).
         See https://grew.fr/usage/cli/#count for more deatils."

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
  match !Grew_args.patterns with
    | [] -> Log.warning "No pattern given (expected one json file with patterns)"
    | _::_::_ -> Log.warning "Too much patterns given (expected one json file with patterns)"
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
                      let pattern = 
                        (* NB: pattern should be reparsed for each corpora, because config may change *)
                        try Pattern.parse ~config (String.concat " " pat_desc.Stat.code)
                        with Libgrew.Error msg ->
                          error
                            ~fct:"Grew.stat"
                            ~data:(`String (String.concat " " pat_desc.Stat.code))
                            "cannot parse pattern associated with desc: %s" pat_desc.Stat.desc in
                      Corpus.fold_left 
                        (fun acc _ graph ->
                          acc + (List.length (Matching.search_pattern_in_graph ~config pattern graph))
                        ) 0 corpus
                  ) pat_descs
                )
          ) (load_corpus_desc_list ()) in

      let stats = `List (
         List.map (fun (desc, occs) -> `List (`String desc :: (List.map (fun i -> `Int i) occs))) bare_lines
      ) in

      let ratio = `List (
        List.map (fun (desc, occs) -> `List (`String desc :: (Stat.compute_ratios occs))) bare_lines
        ) in

     let final_json = `Assoc [
        ("patterns", json);
        ("stats", stats);
        ("ratio", ratio);
      ] in
      match !Grew_args.output_data with
       | None -> printf "%s\n" (Yojson.Basic.pretty_to_string final_json)
       | Some f -> Yojson.Basic.to_file f final_json

(* -------------------------------------------------------------------------------- *)
let valid () =
  match !Grew_args.output_data with
    | None -> error ~fct:"valid" "an output directory is required (use -o option)"
    | Some dir ->
      ensure_dir dir;
      let validator_list = List.map Validation.load_json !Grew_args.patterns in
      List.iter
        (fun corpus_desc ->
          if not !quiet then printf "%s\n" (Corpus_desc.get_id corpus_desc);
          Validation.check ~dir validator_list corpus_desc
        ) (load_corpus_desc_list ())

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

