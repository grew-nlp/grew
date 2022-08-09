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
  handle
    (fun () ->
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
           (fun graph -> fprintf out_ch "%s\n" (graph |> Graph.to_json |> Conllx.of_json |> Conllx.to_string ~config ~columns)),
           (fun () -> ())
         | Grew_args.Gr ->
           (fun graph -> fprintf out_ch "%s\n" (Graph.to_gr ~config graph)),
           (fun () -> ())
         | Grew_args.Dot ->
           let flag = ref false in
           let buff = Buffer.create 32 in
           (fun graph ->
              if !flag
              then (Log.fail "`dot` output cannot be used when there is more than one graph in the output")
              else (flag := true; bprintf buff "%s" (Graph.to_dot ~config graph))
           ),
           (fun () -> fprintf out_ch "%s\n" (Buffer.contents buff))
         | Grew_args.Json ->
           let data = ref [] in
           (fun graph -> data := (graph |> Graph.to_json) :: !data),
           (fun () ->
              let json = match !data with
                | [one] -> one
                | _ -> `List (List.rev !data) in
              fprintf out_ch "%s\n" (Yojson.Basic.pretty_to_string json)
           )
         | Grew_args.Multi_json ->
           let data = ref [] in
           (fun graph -> data := (graph |> Graph.to_json) :: !data),
           (fun () -> 
              match (!Grew_args.output_data, !data) with
              | (None,_) -> Log.fail "%s" "-multi_json implies -o"
              | (Some out_file, l) ->
                let base = 
                  match Filename.chop_suffix_opt ~suffix:".json" out_file with
                  | Some b -> b
                  | None -> out_file in
                List.iteri
                  (fun i json -> 
                     let sub_out_ch = open_out (sprintf "%s__%d.json" base i) in
                     fprintf sub_out_ch "%s\n" (Yojson.Basic.pretty_to_string json)
                  ) l
           ) in

       Corpus.iteri
         (fun index sent_id gr ->
            Counter.print index len sent_id;
            match Rewrite.simple_rewrite ~config gr grs !Grew_args.strat with
            | [one] -> next_graph one
            | l ->
              List.iteri
                (fun i graph -> next_graph (Graph.set_meta "sent_id" (sprintf "%s_%d" sent_id i) graph)
                ) l
         ) corpus;
       Counter.finish ();
       final ();
       match !Grew_args.output_data with
       | Some output_file -> close_out out_ch
       | None -> ()
    ) ()


(* -------------------------------------------------------------------------------- *)
let get_value ~config clusts pattern graph matching =
  List.map 
    (function
      | Key key -> Matching.get_value_opt ~config key pattern graph matching
      | Whether whether -> 
          let basic = Pattern.parse_basic ~config pattern ("{" ^ whether ^ "}") in
          if Matching.whether ~config basic pattern graph matching then Some "Yes" else Some "No"
      | No_clust -> assert false
    ) clusts

let new_grep_with_clusts clusts =
  let config = !Grew_args.config in
  match !Grew_args.patterns with
  | [pattern_file] ->
    let pattern = Pattern.load ~config pattern_file in
    let build_key_list = get_value ~config clusts pattern in
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
    Clustered.fold_layer
      (fun json_list -> `List json_list)
      []
      (fun string_opt sub acc -> (CCOption.get_or ~default:"undefined" string_opt, sub) :: acc)
      (fun x -> `Assoc x)
      clustered
  | l -> Log.fail "1 pattern expected for grep mode (%d given)" (List.length l)

let grep_with_clust () = Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (new_grep_with_clusts [!Grew_args.clust1]))

let grep_without_key () = Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (new_grep_with_clusts []))

(* -------------------------------------------------------------------------------- *)
let grep () =
  handle
    (fun () ->
       match !Grew_args.clust1 with
       | No_clust -> grep_without_key ()
       | _ -> grep_with_clust ()
    ) ()

(* -------------------------------------------------------------------------------- *)
let compile () =
  handle
    (fun () ->
       List.iter
         (fun json_file ->
            let corpus_desc_list = Corpus_desc.load_json json_file in
            List.iter
              (fun corpus_desc ->
                 Corpus_desc.compile ~force:!Grew_args.force ?grew_match:!Grew_args.grew_match_server corpus_desc
              ) corpus_desc_list
         ) !Grew_args.input_data
    ) ()

(* -------------------------------------------------------------------------------- *)
let clean () =
  handle
    (fun () ->
       List.iter
         (fun json_file ->
            let corpus_desc_list = Corpus_desc.load_json json_file in
            List.iter
              (fun corpus_desc ->
                 Corpus_desc.clean corpus_desc
              ) corpus_desc_list
         ) !Grew_args.input_data
    ) ()

(* -------------------------------------------------------------------------------- *)
let count () =
  handle
    (fun () ->
       match (!Grew_args.patterns, !Grew_args.clust1) with

       (* no key --> count each pattern in each corpus *)
       | (_, No_clust) ->
         printf "Corpus\t# sentences";
         List.iter (fun p -> printf "\t%s" (p |> Filename.basename |> Filename.remove_extension)) !Grew_args.patterns;
         printf "\n";

         List.iter (
           fun json_file ->
             let corpus_desc_list = Corpus_desc.load_json json_file in
             List.iter
               (fun corpus_desc ->
                  match Corpus_desc.load_corpus_opt corpus_desc with
                  | None -> error ~fct:"Grew.count" "The corpus %s is not compiled" (Corpus_desc.get_id corpus_desc)
                  | Some data -> 
                    let config = Corpus_desc.get_config corpus_desc in
                    (* NB: pattern loading depends on the config -> reload for each corpus!  *)
                    let patterns = List.map (Pattern.load ~config) !Grew_args.patterns in
                    printf "%s" (Corpus_desc.get_id corpus_desc);
                    printf "\t%d" (Corpus.size data);

                    List.iter
                      (fun pattern ->
                         let count =
                           Corpus.fold_left (fun acc _ graph ->
                               acc + (List.length (Matching.search_pattern_in_graph ~config pattern graph))
                             ) 0 data in
                         printf "\t%d" count
                      ) patterns;
                    printf "\n%!"
               ) corpus_desc_list
         ) (!Grew_args.input_data)

       (* with clustering --> one pattern only, count each cluster in each corpus *)
       | ([pat], _) ->
         let maps = 
           CCList.flat_map (
             fun json_file ->
               let corpus_desc_list = Corpus_desc.load_json json_file in
               List.map
                 (fun corpus_desc ->
                    match Corpus_desc.load_corpus_opt corpus_desc with
                    | None -> error ~fct:"Grew.count" "The corpus %s is not compiled" (Corpus_desc.get_id corpus_desc)
                    | Some data -> 
                      let config = Corpus_desc.get_config corpus_desc in
                      (* NB: pattern loading depends on the config -> reload for each corpus!  *)
                      let pattern = Pattern.load ~config pat in
                      let get_value = match !Grew_args.clust1 with
                        | Key key -> 
                          fun graph matching -> CCOption.get_or ~default:"undefined" (Matching.get_value_opt ~config key pattern graph matching)
                        | Whether clust_value -> 
                          let basic = Pattern.parse_basic ~config pattern ("{" ^ clust_value ^ "}") in
                          fun graph matching -> if Matching.whether ~config basic pattern graph matching then "Yes" else "No"
                        | No_clust -> assert false in
                      let dist = 
                        Corpus.fold_left 
                          (fun acc _ graph ->
                             let matchings = Matching.search_pattern_in_graph ~config pattern graph in
                             List.fold_left 
                               (fun acc2 matching ->
                                  let value = get_value graph matching in
                                  match String_map.find_opt value acc2 with
                                  | None -> String_map.add value 1 acc2
                                  | Some old -> String_map.add value (old+1) acc2
                               ) acc matchings
                          ) String_map.empty data in
                      (Corpus_desc.get_id corpus_desc, dist)
                 ) corpus_desc_list
           ) (!Grew_args.input_data) in

         let all_keys = List.fold_left 
             (fun acc (_,map) ->
                String_map.fold 
                  (fun k _ acc2 ->
                     String_set.add k acc2
                  ) map acc
             ) String_set.empty maps in

         printf "Corpus";
         String_set.iter (fun k -> printf "\t%s" k)  all_keys;
         printf "\n";

         List.iter (
           fun (corpus_name, map) ->
             printf "%s" corpus_name;
             String_set.iter
               (fun key ->
                  printf "\t%d" (match String_map.find_opt key map with Some v -> v | None -> 0)
               ) all_keys;
             printf "\n%!"
         ) maps

       | (l,_) -> Log.warning "When the 'key' parameter is used, exactly one pattern is expected (%d given)" (List.length l)
    ) ()

(* -------------------------------------------------------------------------------- *)
let compute_ratios l =  
  let sum = float (List.fold_left (+) 0 l) in
  List.map (fun i -> `Float ((Float.round (10000. *. float i /. sum)) /. 100.)) l

let stat () =
  handle
    (fun () ->
       match !Grew_args.patterns with
       | [] -> Log.warning "No pattern given (expected one json file with patterns)"
       | _::_::_ -> Log.warning "Too much patterns given (expected one json file with patterns)"
       | [one] ->
         let (pat_descs, json) = Stat.load_json one in

         let corpora =
           List.fold_left
             (fun acc conf_file ->
                Corpus_desc.load_json conf_file @ acc
             ) [] !Grew_args.input_data in

         let bare_lines =
           List.map
             (fun corpus_desc ->
                match Corpus_desc.load_corpus_opt corpus_desc with
                | None -> error ~fct:"Grew.stat" "The corpus %s is not compiled" (Corpus_desc.get_id corpus_desc)
                | Some corpus -> 
                  let config = Corpus_desc.get_config corpus_desc in
                  (Corpus_desc.get_id corpus_desc,
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
                       Corpus.fold_left (fun acc _ graph ->
                           acc + (List.length (Matching.search_pattern_in_graph ~config pattern graph))
                         ) 0 corpus
                   ) pat_descs
                  )
             ) corpora in

         let stats = `List (
             List.map (fun (desc, occs) -> `List (`String desc :: (List.map (fun i -> `Int i) occs))) bare_lines
           ) in

         let ratio = `List (
             List.map 
               (fun (desc, occs) -> 
                  `List (`String desc :: (compute_ratios occs))
               ) bare_lines
           ) in

         let final_json = `Assoc [
             ("patterns", json);
             ("stats", stats);
             ("ratio", ratio);
           ] in
         match !Grew_args.output_data with
         | None -> printf "%s\n" (Yojson.Basic.pretty_to_string final_json)
         | Some f -> Yojson.Basic.to_file f final_json
    ) ()

(* -------------------------------------------------------------------------------- *)
let valid () =
  handle
    (fun () ->
       match !Grew_args.output_data with
       | None -> error ~fct:"valid" "an output directory is required (use -o option)"
       | Some dir ->
         match ensure_dir dir with
         | Some pble -> error ~fct:"valid" "%s" pble
         | None ->
           let validator_list = List.map Validation.load_json !Grew_args.patterns in
           List.iter
             (fun conf_file ->
                List.iter
                  (fun corpus_desc ->
                     if not !quiet then printf "%s\n" (Corpus_desc.get_id corpus_desc);
                     Validation.check ~dir validator_list corpus_desc
                  ) (Corpus_desc.load_json conf_file)
             ) !Grew_args.input_data
    ) ()


(* -------------------------------------------------------------------------------- *)
(* -------------------------------------------------------------------------------- *)
let _ =
  Printexc.record_backtrace true;

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Undefined -> ()
  | Grew_args.Transform -> transform ()
  | Grew_args.Count-> count ()
  | Grew_args.Valid-> valid ()
  | Grew_args.Stat-> stat ()
  | Grew_args.Compile -> compile ()
  | Grew_args.Clean -> clean ()
  | Grew_args.Grep -> grep ()
  | Grew_args.Test -> failwith "No test defined"

