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
open Log
open Conllx
open Libgrew

open Grew_utils
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
      with Unix.Unix_error _ -> fail (sprintf "File not found `%s`" one)
    end
  | files ->
    let sub_corpora =
      List.fold_left
        (fun acc file ->
           try
             let subcorpus = Corpus.from_file ~config file in
             subcorpus :: acc
           with Unix.Unix_error _ -> fail (sprintf "File not found `%s`" file)
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
              then (Log.fmessage "`dot` output cannot be used when there is more than one graph in the output"; exit 1)
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
              | (None,_) -> fail "-multi_json implies -o"
              | (Some out_file, l) ->
                let base = 
                  match Filename.chop_suffix_opt ".json" out_file with
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
let grep () = handle
    (fun () ->
       let config = !Grew_args.config in
       match !Grew_args.patterns with
       | [pattern_file] ->

         let pattern = Pattern.load ~config pattern_file in

         (* get the array of graphs to explore *)
         let corpus = load_corpus () in

         (match !Grew_args.dep_dir with
          | None -> ()
          | Some d -> ignore (Sys.command (sprintf "mkdir -p %s" d)));

         (* printf "%s\n" (String.concat "_" (Pattern.pid_name_list pattern)); *)
         let pattern_ids = Pattern.pid_name_list pattern in

         let final_json =
           Corpus.fold_left
             (fun acc name graph ->
                let matchings = Graph.search_pattern ~config pattern graph in
                List.fold_left
                  (fun acc2 matching ->
                     let assoc_nodes = Matching.nodes pattern graph matching in
                     let graph_node_names = List.map snd assoc_nodes in
                     let deco = Deco.build pattern matching in

                     (* write the dep file if needed *)
                     let dep_file =
                       match !Grew_args.dep_dir with
                       | None -> None
                       | Some dir ->
                         let id = sprintf "%s__%s"
                             name
                             (String.concat "_" (List.map2 (sprintf "%s:%s") pattern_ids graph_node_names)) in
                         let dep = Graph.to_dep ~config ~deco graph in
                         let filename = Filename.concat dir (sprintf "%s.dep" id) in
                         let out_ch = open_out filename in
                         fprintf out_ch "%s" dep;
                         close_out out_ch;
                         Some filename in

                     let json_matching = Matching.to_json pattern graph matching in

                     let opt_list = [
                       Some ("sent_id", `String name);
                       Some ("matching", json_matching);
                       (
                         if !Grew_args.html
                         then Some ("html", `String (Graph.to_sentence ~deco graph))
                         else None
                       );
                       (
                         match dep_file with
                         | None -> None
                         | Some f -> Some ("dep_file", `String f)
                       )
                     ] in
                     let json = `Assoc (CCList.filter_map (fun x -> x) opt_list) in
                     json :: acc2
                  ) acc matchings
             ) [] corpus in
         Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (`List final_json))
       | l -> Log.fmessage "1 pattern expected for grep mode (%d given)" (List.length l); exit 1;

    ) ()


(* -------------------------------------------------------------------------------- *)
let compile () =
  handle
    (fun () ->
       let grew_match = match !Grew_args.grew_match_server with
         | Some dir -> Some (Filename.concat dir "_meta")
         | None -> None in
       List.iter
         (fun json_file ->
            let corpus_desc_list = Corpus_desc.load_json json_file in
            List.iter
              (fun corpus_desc ->
                 Corpus_desc.compile ~force:!Grew_args.force ?grew_match corpus_desc
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
let load_marshal corpus_desc =
  let id = Corpus_desc.get_id corpus_desc in
  let directory = Corpus_desc.get_directory corpus_desc in
  let marshal_file = (Filename.concat directory id) ^ ".marshal" in
  let in_ch = open_in_bin marshal_file in
  let data = (Marshal.from_channel in_ch : Corpus.t) in
  let _ = close_in in_ch in
  data


module String_map = Map.Make (String)
(* -------------------------------------------------------------------------------- *)
let count () =
  handle
    (fun () ->
       match (!Grew_args.patterns, !Grew_args.key) with

       (* no key --> count each pattern in each corpus *)
       | (_, None) ->
         printf "Corpus\t# sentences";
         List.iter (fun p -> printf "\t%s" (p |> Filename.basename |> Filename.remove_extension)) !Grew_args.patterns;
         printf "\n";

         List.iter (
           fun json_file ->
             let corpus_desc_list = Corpus_desc.load_json json_file in
             List.iter
               (fun corpus_desc ->
                  let config = Corpus_desc.get_config corpus_desc in
                  (* NB: pattern loading depends on the config -> reload for each corpus!  *)
                  let patterns = List.map (Pattern.load ~config) !Grew_args.patterns in
                  let data = load_marshal corpus_desc in
                  printf "%s" (Corpus_desc.get_id corpus_desc);
                  printf "\t%d" (Corpus.size data);

                  List.iter
                    (fun pattern ->
                       let count =
                         Corpus.fold_left (fun acc _ graph ->
                             acc + (List.length (Graph.search_pattern ~config pattern graph))
                           ) 0 data in
                       printf "\t%d" count
                    ) patterns;
                  printf "\n%!"
               ) corpus_desc_list
         ) (!Grew_args.input_data)

       (* with key --> one pattern only, count each cluster in each corpus *)
       | ([pat], Some key) ->
         let maps = 
           CCList.flat_map (
             fun json_file ->
               let corpus_desc_list = Corpus_desc.load_json json_file in
               List.map
                 (fun corpus_desc ->
                    let config = Corpus_desc.get_config corpus_desc in
                    (* NB: pattern loading depends on the config -> reload for each corpus!  *)
                    let pattern = Pattern.load ~config pat in
                    let data = load_marshal corpus_desc in

                    let dist = 
                      Corpus.fold_left 
                        (fun acc _ graph ->
                           let matchings = Graph.search_pattern ~config pattern graph in
                           List.fold_left 
                             (fun acc2 matching ->
                                let value = 
                                  match Matching.get_value_opt ~config key pattern graph matching with
                                  | None -> "undefined"
                                  | Some v -> v in
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

       | (l,_) -> Log.fwarning "When the 'key' parameter is used, exactly one pattern is expected (%d given)" (List.length l)
    ) ()

(* -------------------------------------------------------------------------------- *)
let stat () =
  handle
    (fun () ->
       match !Grew_args.patterns with
       | [] -> Log.fwarning "No pattern given (expected one json file with patterns)"
       | _::_::_ -> Log.fwarning "Too much patterns given (expected one json file with patterns)"
       | [one] ->
         let (pat_descs, json) = Stat.load_json one in

         let corpora =
           List.fold_left
             (fun acc conf_file ->
                Corpus_desc.load_json conf_file @ acc
             ) [] !Grew_args.input_data in

         let lines =
           List.map
             (fun corpus_desc ->
                let corpus = load_marshal corpus_desc in
                let config = Corpus_desc.get_config corpus_desc in
                `List (

                  `String (Corpus_desc.get_id corpus_desc) ::
                  List.map (
                    fun pat_desc ->
                      let pattern =
                        try Pattern.parse ~config (String.concat " " pat_desc.Stat.code)
                        with Libgrew.Error msg ->
                          error
                            ~fct:"Grew.stat"
                            ~data:(`String (String.concat " " pat_desc.Stat.code))
                            "cannot parse pattern associated with desc: %s" pat_desc.Stat.desc in
                      let count =
                        Corpus.fold_left (fun acc _ graph ->
                            acc + (List.length (Graph.search_pattern ~config pattern graph))
                          ) 0 corpus in
                      `Int count
                  ) pat_descs
                )
             ) corpora in
         let final_json = `Assoc [
             ("patterns", json);
             ("stats", `List lines)
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
  Log.set_active_levels [`INFO; `MESSAGE; `WARNING];
  Log.set_write_to_log_file false;

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

