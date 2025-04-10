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

