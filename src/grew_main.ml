(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.fr                                          *)
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
  Log.fmessage "-------------------------------------";
  Log.fmessage "%s" msg;
  Log.fmessage "-------------------------------------";
  exit 2

let handle fct () =
  try fct ()
  with
  | Conll_error json ->            fail (Yojson.Basic.pretty_to_string json)
  | Libgrew.Error msg ->           fail msg
  | Corpus_.File_not_found file -> fail (sprintf "File not found: \"%s\"" file)
  | Corpus_.Fail msg ->            fail msg
  | Sys_error msg ->               fail (sprintf "System error: %s" msg)
  | Yojson.Json_error msg ->       fail (sprintf "Json error: %s" msg)
  | Libgrew.Bug msg ->             fail (sprintf "Libgrew.bug, please report: %s" msg)
  | exc ->                         fail (sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))

(* -------------------------------------------------------------------------------- *)
let transform () =
  handle (fun () ->
      let grs = match !Grew_args.grs with
        | None -> Grs.empty
        | Some file -> Grs.load file in

      let domain = Grs.domain grs in

      let graph_array = Corpus_.input ?domain () in
      let len = Array.length graph_array in

      let out_ch = match !Grew_args.output_file with
        | Some output_file -> open_out output_file
        | None -> stdout in

      let out_graph ?new_sent_id graph = match (!Grew_args.output, new_sent_id) with
        | (Grew_args.Conll,None) -> fprintf out_ch "%s\n" (Graph.to_conll_string graph)
        | (Grew_args.Conll,Some nsi) -> fprintf out_ch "%s\n" (graph |> Graph.to_conll |> Conll.set_sentid nsi |> Conll.to_string)
        | (Grew_args.Cupt, None) -> fprintf out_ch "%s\n" (Graph.to_conll_string ~cupt:true graph)
        | (Grew_args.Cupt,Some nsi) -> fprintf out_ch "%s\n" (graph |> Graph.to_conll |> Conll.set_sentid nsi |> Conll.to_string ~cupt:true)
        | (Grew_args.Gr, None) -> fprintf out_ch "%s\n" (Graph.to_gr graph)
        | (Grew_args.Gr, Some nsi) -> fprintf out_ch "# sent_id = %s\n%s\n" nsi (Graph.to_gr graph)
        | (Grew_args.Dot, None) -> fprintf out_ch "%s\n" (Graph.to_dot graph)
        | (Grew_args.Dot, Some nsi) -> fprintf out_ch "# sent_id = %s\n%s\n" nsi (Graph.to_dot graph) in

      Array.iteri
        (fun index (id, gr) ->
           Counter.print index len id;
           match Rewrite.simple_rewrite ~gr ~grs ~strat:!Grew_args.strat with
           | [one] -> out_graph one
           | l ->
             List.iteri (fun i gr ->
                 let conll = Graph.to_conll gr in
                 let conll_new_id = Conll.set_sentid (sprintf "%s_%d" id i) conll in
                 fprintf out_ch "%s\n" (Conll.to_string conll_new_id)
               ) l
        ) graph_array;
      Counter.finish ();
      match !Grew_args.output_file with
      | Some output_file -> close_out out_ch
      | None -> ()
    ) ()

(* -------------------------------------------------------------------------------- *)
let grep () = handle
    (fun () ->
       match !Grew_args.pattern with
       | None -> Log.message "No pattern file specified: use -pattern option"; exit 1;
       | Some pattern_file ->

         let domain = match !Grew_args.grs with
           | None -> None
           | Some file -> Grs.domain (Grs.load file) in

         let pattern = Pattern.load ?domain pattern_file in

         (* get the array of graphs to explore *)
         let graph_array = Corpus_.input ?domain () in

         (match !Grew_args.dep_dir with
          | None -> ()
          | Some d -> ignore (Sys.command (sprintf "mkdir -p %s" d)));

         (* printf "%s\n" (String.concat "_" (Pattern.pid_name_list pattern)); *)
         let pattern_ids = Pattern.pid_name_list pattern in

         let final_json =
           Array.fold_left
             (fun acc (name,graph) ->
                let matchings = Graph.search_pattern ?domain pattern graph in
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
                         let dep = Graph.to_dep ~deco graph in
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
             ) [] graph_array in
         Printf.printf "%s\n" (Yojson.Basic.pretty_to_string (`List final_json))
    ) ()


let compile () =
  handle
    (fun () ->
       List.iter (Corpus.compile ?grew_match:!Grew_args.grew_match) !Grew_args.input_data
    ) ()

let clean () =
  handle
    (fun () ->
       List.iter Corpus.clean !Grew_args.input_data
    ) ()



let _ =
  Printexc.record_backtrace true;
  Log.set_active_levels [`INFO; `MESSAGE; `WARNING];
  Log.set_write_to_log_file false;

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Undefined -> ()
  | Grew_args.Transform -> transform ()
  | Grew_args.Compile -> compile ()
  | Grew_args.Clean -> clean ()
  | Grew_args.Grep -> grep ()
  | Grew_args.Test -> failwith "No test defined"
  | Grew_args.Gui args ->
    let gui_exec = if Sys.argv.(0) = "grew_dev" then "grew_gui_dev" else "grew_gui" in
    match Unix.system (gui_exec ^ " " ^ args) with
    | Unix.WEXITED 127 -> Log.message "It seems that grew_gui is not installed on your system. See [http://grew.fr/gtk] for more information"
    | _ -> ()



