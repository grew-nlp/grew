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
open Grew_build

let grs_timestamps = ref String_map.empty

let get_grs_timestamp grs_file =
  match String_map.find_opt grs_file !grs_timestamps with
  | Some s -> s
  | None ->
    (* Load with a default config... just for getting the timestamp --> config must be in GRS!! *)
    let grs = Grs.load ~config:(Conll_config.build "sud") grs_file in
    let grs_timestamp = grs |> Grs.get_timestamp_opt |> CCOption.get_exn_or "Bug grs_timestamp" in
    grs_timestamps := String_map.add grs_file grs_timestamp !grs_timestamps;
    grs_timestamp

type status =
  | Native
  | Uptodate
  | Need_rebuild of string list (* list of sub messages *)
  | Error of string

let compute_status () =
  let all = Corpusbank.get_desc_map () in

  (* let update _corpus_id _corpus_desc acc = acc in *)
  let rec update corpus_id acc =
    match Corpusbank.get_corpus_desc_opt corpus_id with
    | None -> String_map.add corpus_id (Error (sprintf "No desc found for `%s`" corpus_id)) acc
    | Some corpus_desc ->
      match String_map.find_opt corpus_id acc with
      | Some _ -> acc (* already computed in a previous recursive call *)
      | None ->
        match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc) with
        | (None, None) -> String_map.add corpus_id Native acc
        | (None, Some _) -> error "corpus `%s` is described with a `grs` but without `src`" corpus_id
        | (Some _, None) -> error "corpus `%s` is described with a `src` but without `grs`" corpus_id
        | (Some src_corpus_id, Some grs_file) ->
          let new_acc = update src_corpus_id acc in
          begin
            match String_map.find src_corpus_id new_acc with
            | Error m -> String_map.add corpus_id (Error (sprintf "depend on `%s` [%s]" src_corpus_id m)) acc
            | Need_rebuild _ -> String_map.add corpus_id (Need_rebuild [sprintf "depend on `%s`" src_corpus_id]) acc
            | _ -> 
              match Corpusbank.get_corpus_desc_opt src_corpus_id with
              | None -> String_map.add corpus_id (Error (sprintf "No desc found for source corpus `%s` in `%s`" src_corpus_id corpus_id)) acc
              | Some src_corpus_desc ->
                try
                  let src_files = Corpus_desc.get_files src_corpus_desc in
                  let directory = Corpus_desc.get_directory corpus_desc in
                  let old_tar_files = ref (String_set.of_list (Corpus_desc.get_files corpus_desc)) in
                  let grs_timestamp = get_grs_timestamp grs_file in
                  let msg_list = List.fold_left
                    (fun acc src ->
                      let tar_basename = Filename.basename src in (* TODO: handle replacement in names like ud -> sud *)
                      let tar = Filename.concat directory tar_basename in 
                      old_tar_files := String_set.remove tar !old_tar_files;
                      if max grs_timestamp (File.last_modif src) > (File.last_modif tar)
                      then (sprintf "file `%s` need to be rebuilt" tar) :: acc
                      else acc
                    ) [] src_files in 
                  match (msg_list, !old_tar_files |> String_set.to_seq |> List.of_seq) with
                  | ([], []) -> String_map.add corpus_id Uptodate acc
                  | (msg_list, unwanted_files) -> 
                    let unwanted_msg_list = List.map (fun f -> sprintf "file `%s` must be removed" f) unwanted_files in
                    String_map.add corpus_id (Need_rebuild (msg_list @ unwanted_msg_list)) acc
                with Grewlib.Error msg -> String_map.add corpus_id (Error (sprintf "For corpus_id `%s`, %s" corpus_id msg)) acc
          end
  in

    String_map.fold (
      fun corpus_id _ acc -> update corpus_id acc
    ) all String_map.empty

let status () =
  let status = compute_status () in
  let ok_cpt = ref 0 and rebuild_cpt = ref 0 and error_cpt = ref 0 in 
  String_map.iter (
    fun corpus_id _ ->
      match String_map.find corpus_id status with
      | Need_rebuild msg_list -> 
          incr rebuild_cpt;
          Log.magenta "%s need rebuild\n" corpus_id;
          List.iter (fun msg -> printf "  • %s\n%!" msg) msg_list
      | Error msg ->
        incr error_cpt;
        Log.red "%s --> %s\n" corpus_id msg
      | Native ->
        incr ok_cpt;
        if !verbose then Log.green "%s --> OK (native)\n" corpus_id
      | Uptodate ->
        incr ok_cpt;
        if !verbose then Log.green "%s --> OK (derived)\n" corpus_id
  ) status;
  printf "----------------------------------\n";
  printf "total:      %d\n" (!ok_cpt + !rebuild_cpt + !error_cpt);
  Log.green "ok:         %d\n" !ok_cpt;
  Log.magenta "to rebuild: %d\n" !rebuild_cpt;
  Log.red "error:      %d\n" !error_cpt;
  printf "----------------------------------\n"

(* -------------------------------------------------------------------------------- *)
let compile () =
  let corpus_desc_map = Corpusbank.get_desc_map () in
  let filter = Corpusbank.build_filter () in
  String_map.iter
    (fun corpus_id corpus_desc ->
      if filter corpus_id
      then
        try Corpus_desc.compile ~force:!Grew_cli_global.force corpus_desc
        with Grewlib.Error msg -> Log.warning "--> %s skipped (%s)" corpus_id msg
    ) corpus_desc_map

(* -------------------------------------------------------------------------------- *)
let clean () =
  let corpus_desc_map = Corpusbank.get_desc_map () in
  let filter = Corpusbank.build_filter () in
  let filtered =
     String_map.fold 
     (fun corpus_id corpus_desc acc -> if filter corpus_id then corpus_desc::acc else acc) 
     corpus_desc_map [] in
  let really_clean () = List.iter Corpus_desc.clean filtered in
  if !Grew_cli_global.force
  then really_clean ()
  else
    let nb = List.length filtered in
    if nb > 10
    then
      let _ = Printf.printf "This will clean %d corpora, are you sure [y/N]?\n%!" nb in 
      let answer = read_line () in
      if answer = "y" || answer = "Y" 
      then really_clean ()
      else printf "Aborted\n"

(* -------------------------------------------------------------------------------- *)
let search () =
  let corpus_desc_map = Corpusbank.get_desc_map () in
  let filter = Corpusbank.build_filter () in
    String_map.iter
    (fun corpus_id _ -> 
      if filter corpus_id then Printf.printf "%s\n%!" corpus_id)
    corpus_desc_map

(* -------------------------------------------------------------------------------- *)
let build () = 
  let corpus_desc_map = Corpusbank.get_desc_map () in
  let filter = Corpusbank.build_filter () in
    String_map.iter
    (fun corpus_id corpus_desc -> 
      if filter corpus_id then build_derived corpus_desc)
    corpus_desc_map

(* -------------------------------------------------------------------------------- *)
let show () = 
  let corpus_desc_map = Corpusbank.get_desc_map () in
  let filter = Corpusbank.build_filter () in
    String_map.iter
    (fun corpus_id corpus_desc -> 
      if filter corpus_id 
      then
        begin
          Log.green "%s\n" corpus_id;
          match Corpus_desc.to_json corpus_desc with
          | `Assoc l ->
            List.iter 
            (fun (k,v) -> 
              Printf.printf "%s --> %s\n%!" k (Yojson.Basic.pretty_to_string v)
            ) l
          | _ -> assert false
        end
    )
    corpus_desc_map
