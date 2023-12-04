open Printf
(* open Yojson.Basic.Util *)
open Conll
open Grewlib
open Grew_cli_utils
open Grew_args

let green x = Printf.ksprintf (ANSITerminal.printf [ANSITerminal.green] "%s") x
let red x = Printf.ksprintf (ANSITerminal.printf [ANSITerminal.red] "%s") x
let magenta x = Printf.ksprintf (ANSITerminal.printf [ANSITerminal.magenta] "%s") x

(* get the last modif time of a [file] relative to the [base] folder *)
let last_modif file =
  try
    let stat = Unix.stat file in
    stat.Unix.st_mtime
  with Unix.Unix_error _ -> Float.min_float
let corpusbank = "/Users/guillaum/resources/corpusbank"

let desc_map = ref None

(* lazy loading of corpus desc files *)
let get_desc_map () =
  match  !Grew_args.corpusbank with
  | None -> error "%s" "No corpusbank defined"
  | Some corpusbank -> 
  match !desc_map with
  | Some data -> data
  | None ->
    try
      let all_files = Sys.readdir corpusbank in
      let data = Array.fold_left
        (fun acc file ->
          if Filename.extension file = ".json"
          then
            begin
              let descs = Corpus_desc.load_json (Filename.concat corpusbank file) in
              List.fold_left
                (fun acc2 desc ->
                  let id = Corpus_desc.get_id desc in
                  if String_map.mem id acc2
                  then failwith (sprintf "Duplicate definition of corpus_id `%s`" id)
                  else String_map.add id desc acc2
                ) acc descs
          end
          else acc
        ) String_map.empty all_files in
      desc_map := Some data;
      data
    with Sys_error _ -> failwith (sprintf "corpusbank directory `%s` not found:" corpusbank)

let get_corpus_desc_opt corpus_id = String_map.find_opt corpus_id (get_desc_map ())




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
  let all = get_desc_map () in

  (* let update _corpus_id _corpus_desc acc = acc in *)
  let rec update corpus_id acc =
    match get_corpus_desc_opt corpus_id with
    | None -> String_map.add corpus_id (Error (sprintf "No desc found for `%s`" corpus_id)) acc
    | Some corpus_desc ->
    match String_map.find_opt corpus_id acc with
    | Some _ -> acc (* already computed in a previous recursive call *)
    | None ->
        match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc) with
        | (None, None) -> String_map.add corpus_id Native acc
        | (Some src_corpus_id, Some grs_file) ->
          let new_acc = update src_corpus_id acc in
          begin
            match String_map.find src_corpus_id new_acc with
            | Error m -> String_map.add corpus_id (Error (sprintf "depend on `%s` [%s]" src_corpus_id m)) acc
            | Need_rebuild _ -> String_map.add corpus_id (Need_rebuild [sprintf "depend on `%s`" src_corpus_id]) acc
            | _ -> 
              match get_corpus_desc_opt src_corpus_id with
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
                      if max grs_timestamp (last_modif src) > (last_modif tar)
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
    | _ -> failwith "type error" in

    String_map.fold (
      fun corpus_id _ acc -> update corpus_id acc
    ) all String_map.empty

let dump_status () =
  let status = compute_status () in
  let ok_cpt = ref 0 and rebuild_cpt = ref 0 and error_cpt = ref 0 in 
  String_map.iter (
    fun corpus_id _ ->
      match String_map.find corpus_id status with
      | Need_rebuild msg_list -> 
          incr rebuild_cpt;
          magenta "%s need rebuild\n" corpus_id;
          List.iter (fun msg -> printf "  • %s\n" msg) msg_list
      | Error msg ->
        incr error_cpt;
        red "%s --> %s\n" corpus_id msg
      | Native ->
        incr ok_cpt;
        green "%s --> OK (native)\n" corpus_id
      | Uptodate ->
        incr ok_cpt;
        green "%s --> OK (derived)\n" corpus_id
  ) status;
  printf "----------------------------------\n";
  printf "total:      %d\n" (!ok_cpt + !rebuild_cpt + !error_cpt);
  green "ok:         %d\n" !ok_cpt;
  magenta "to rebuild: %d\n" !rebuild_cpt;
  red "error:      %d\n" !error_cpt;
  printf "----------------------------------\n"

  let transform config columns grs strat input_file out_file = 
    let input_corpus = Corpus.from_file ~config input_file in
    let out_ch = open_out out_file in
    Corpus.iteri
    (fun _ _ gr ->
      (* Counter.print index len sent_id; *)
      match Rewrite.simple_rewrite ~config gr grs strat with
        | [graph] -> fprintf out_ch "%s\n" (graph |> Graph.to_json |> Conll.of_json |> Conll.to_string ~config ~columns)
        | _ -> failwith "Not one output!!!"
    ) input_corpus;
    (* Counter.finish (); *)
    (* final (); *)
      close_out out_ch

  let rec build_derived corpus_id =
    match get_corpus_desc_opt corpus_id with
    | None -> red "ERROR: no description for corpus_id: `%s`" corpus_id
    | Some corpus_desc ->
      let columns = Corpus_desc.get_field_opt "columns" corpus_desc |> CCOption.map_or Conll_columns.build ~default:Conll_columns.default in
      match (Corpus_desc.get_field_opt "src" corpus_desc, Corpus_desc.get_field_opt "grs" corpus_desc, Corpus_desc.get_field_opt "strat" corpus_desc) with
      | (None, _, _) -> () (* this is a native corpus *)
      | (Some _, None, _) -> red "ERROR: in description for corpus_id: `%s`, src but no grs" corpus_id
      | (Some src_corpus_id, Some grs_file, strat_opt) ->
        match get_corpus_desc_opt src_corpus_id with
        | None -> red "ERROR: no description for src_corpus_id: `%s`" src_corpus_id
        | Some src_corpus_desc -> 
          (* first, recursively build until native corpus *)
          let () = build_derived src_corpus_id in
  
          let directory = Corpus_desc.get_directory corpus_desc in
          let () = 
            match Sys.file_exists directory with
            | false -> Unix.mkdir directory 0o755
            | true ->
              match Sys.is_directory directory with
              | true -> ()
              | false -> failwith "a file with the same name exists!" in
  
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
              if max grs_timestamp (last_modif src) > (last_modif tar)
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
