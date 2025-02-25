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
open Conll
open Grewlib

open Grew_cli_global
open Grew_cli_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)
let transform () =
  let config = !Grew_cli_global.config in
  let fix = if !text_from_tokens then Conll.text_from_tokens else CCFun.id in
  let grs = match !Grew_cli_global.grs with
    | None -> Grs.empty
    | Some file -> Grs.load ~config file in

  let corpus = match Input.parse () with
  | Mono c -> c
  | Multi _ -> error ~fct:"Grew.transform" "transform mode cannot be used with multi-corpora input data" in

  let len = Corpus.size corpus in

  let out_ch = match !Grew_cli_global.output_data with
    | Some output_file -> open_out output_file
    | None -> stdout in

  let (next_graph, final) = match !Grew_cli_global.output with
    | Conll columns ->
      fprintf out_ch "%s\n" (Conll_columns.to_string columns);
      (
        (fun graph -> fprintf out_ch "%s\n" (graph |> Graph.to_json |> Conll.of_json |> fix |> Conll.to_string ~config ~columns)),
        (fun () -> ())
      )
    | Dot ->
      let flag = ref false in
      let buff = Buffer.create 32 in
      (
        (fun graph ->
          if !flag
          then (error ~fct:"Grew.transform" "`dot` output cannot be used when there is more than one graph in the output")
          else (flag := true; bprintf buff "%s" (Graph.to_dot ~config graph))),
        (fun () -> fprintf out_ch "%s\n" (Buffer.contents buff))
      )
    | Json ->
      let data = ref [] in
      (
        (fun graph -> data := (graph |> Graph.to_json) :: !data),
        (fun () ->
          let json = match !data with
            | [one] -> one
            | _ -> `List (List.rev !data) in
          fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string json))
      )
    | Multi_json ->
      let data = ref [] in
        (
          (fun graph -> data := (graph |> Graph.to_json) :: !data),
          (fun () -> 
            match (!Grew_cli_global.output_data, !data) with
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
      match Rewrite.simple_rewrite ~config gr grs !Grew_cli_global.strat with
        | [one] -> next_graph one
        | l -> List.iteri (fun i graph -> next_graph (Graph.set_meta "sent_id" (sprintf "%s_%d" sent_id i) graph)) l
    ) corpus;
    Counter.finish ();
    final ();
    match !Grew_cli_global.output_data with
      | Some _ -> close_out out_ch
      | None -> ()

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
      let request = Request.load ~config request_file in
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

  | l -> error ~fct:"Grew.grep" "1 request expected for grep mode (%d given)" (List.length l)

(* -------------------------------------------------------------------------------- *)
let count () =

  let clustered_corpus ~config corpus = 
    Clustered.build_layer
    (fun file_request -> 
      let request = Request.load ~config file_request in
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

let load_corpusbank () =
  match getenv_opt "CORPUSBANK" with
  | None -> error "No CORPUSBANK defined"
  | Some dir -> Corpusbank.load dir

let build_filter () =
  Corpusbank.build_filter (!Grew_cli_global.anonymous_args)

(* -------------------------------------------------------------------------------- *)
let _ =
  Printexc.record_backtrace true;

  (* parsing command line args *)
  handle Grew_args.parse ();

  let run () = match !Grew_cli_global.subcommand with
  | Some "transform" -> transform ()
  | Some "grep" -> grep ()
  | Some "count" -> count ()

  | Some "validate" ->
    let corpusbank = load_corpusbank () in
    let filter = build_filter () in
    Corpusbank.iter ~filter
      (fun corpus_id corpus_desc ->
        try Corpus_desc.validate ~env:!env corpus_desc
        with Grewlib.Error msg -> Log.warning "--> %s skipped (%s)" corpus_id msg
      ) corpusbank

  | Some "compile" ->
    let corpusbank =
      match !input_data with
      | [] -> load_corpusbank ()
      | l -> Corpusbank.read_files l in
    let filter = build_filter () in
    Corpusbank.compile ~filter corpusbank
  
  | Some "clean" ->
    let corpusbank =
      match !input_data with
      | [] -> load_corpusbank ()
      | l -> Corpusbank.read_files l in
    let filter = build_filter () in
    let filtered =
      Corpusbank.fold ~filter
       (fun _ corpus_desc acc -> corpus_desc::acc) 
       corpusbank [] in
    let really_clean () = List.iter Corpus_desc.clean filtered in
    if !Grew_cli_global.force
    then really_clean ()
    else
      let nb = List.length filtered in
      if nb <= 10
      then
        really_clean ()
      else
        let _ = Printf.printf "This will clean %d corpora, are you sure [y/N]?\n%!" nb in 
        let answer = read_line () in
        if answer = "y" || answer = "Y" 
        then really_clean ()
        else printf "Aborted\n"

  | Some "status" -> 
    let corpusbank = load_corpusbank () in
    let filter = build_filter () in
    Corpusbank.print_status ~verbose:!verbose ~filter corpusbank

  | Some "build" -> 
    let corpusbank = load_corpusbank () in
    let filter = build_filter () in
    Corpusbank.build ~filter corpusbank

  | Some "search" ->
    let corpusbank = load_corpusbank () in
    let filter = build_filter () in
    let filtered =
      Corpusbank.fold ~filter
       (fun _ corpus_desc acc -> corpus_desc::acc) 
       corpusbank [] in
    let num = List.length filtered in
    Printf.printf "TOTAL: %d corp%s found\n" num (if num > 1 then "ora" else "us");
    List.iter
      (fun corpus_desc-> Printf.printf " ➔ %s\n%!" (Corpus_desc.get_id corpus_desc))
      filtered
  | Some "show" ->
    let corpusbank = load_corpusbank () in
    let filter = build_filter () in
    Corpusbank.iter ~filter
      (fun _ corpus_desc -> Corpus_desc.show corpus_desc)
      corpusbank

  | Some cmd -> error "Unknown command: \"%s\"" cmd
  | None -> error "Missing subcommand"

  in handle run ()
