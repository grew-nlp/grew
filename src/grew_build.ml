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
open Grew_cli_utils

let transform config columns grs strat input_file out_file =
  let input_corpus = Corpus.from_file ~config input_file in
  let out_ch = open_out out_file in
  Corpus.iteri
  (fun _ _ gr ->
    (* Counter.print index len sent_id; *)
    match Rewrite.simple_rewrite ~config gr grs strat with
      | [graph] -> fprintf out_ch "%s\n" (graph |> Graph.to_json |> Conll.of_json |> Conll.to_string ~config ~columns)
      | _ -> error "More than one normal form (input_file=%s)" input_file
  ) input_corpus;
  (* Counter.finish (); *)
  (* final (); *)
    close_out out_ch

let rec build_derived corpus_desc =
  let corpus_id = Corpus_desc.get_id corpus_desc in
    let columns = Corpus_desc.get_field_opt "columns" corpus_desc |> CCOption.map_or Conll_columns.build ~default:Conll_columns.default in
    match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc, Corpus_desc.get_field_opt "strat" corpus_desc) with
    | (None, _, _) -> () (* this is a native corpus *)
    | (Some _, None, _) -> Log.red "ERROR: in description for corpus_id: `%s`, src but no grs" corpus_id
    | (Some src_corpus_id, Some grs_file, strat_opt) ->
      match Corpusbank.get_corpus_desc_opt src_corpus_id with
      | None -> Log.red "ERROR: no description for src_corpus_id: `%s`" src_corpus_id
      | Some src_corpus_desc -> 
        (* first, recursively build until native corpus *)
        let () = build_derived src_corpus_desc in

        let directory = Corpus_desc.get_directory corpus_desc in
        let () = 
          match Sys.file_exists directory with
          | false -> Unix.mkdir directory 0o755
          | true ->
            match Sys.is_directory directory with
            | true -> ()
            | false -> error "Cannot build directory `%s` for corpus `%s` (a file with the same name exists!)" directory corpus_id in

        (* WARNING: suppose that the grs used the target corpus config!!! *)
        let config = Corpus_desc.get_config corpus_desc in

        let grs = Grs.load ~config grs_file in
        let grs_timestamp = grs |> Grs.get_timestamp_opt |> CCOption.get_exn_or "Bug grs_timestamp" in
        let strat = strat_opt |> CCOption.get_or ~default:"main" in

        let src_files = Corpus_desc.get_files src_corpus_desc in
        let old_tar_files = ref (String_set.of_list (Corpus_desc.get_files corpus_desc)) in
        let () = List.iter
          (fun src ->
            let tar_basename = Filename.basename src in (* TODO: handle replacement in names like ud -> sud *)
            let tar = Filename.concat directory tar_basename in 
            old_tar_files := String_set.remove tar !old_tar_files;
            if max grs_timestamp (File.last_modif src) > (File.last_modif tar)
            then
              begin
                eprintf "update %s from %s\n%!" tar src;
                transform config columns grs strat src tar
              end
            else
              eprintf "%s is uptodate\n%!" tar
          ) src_files in

        let () = String_set.iter
          (fun f ->
            eprintf "remove file %s\n%!" f;
            Unix.unlink f
          ) !old_tar_files in
        ()
