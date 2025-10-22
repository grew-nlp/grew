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
open Grewlib

open Grew_cli_global
open Grew_cli_utils

let load_corpusbank () =
  match !input_data with
  | [] ->
    begin
      match getenv_opt "CORPUSBANK" with
      | None -> error "No CORPUSBANK defined"
      | Some dir -> Corpusbank.load dir
     end
  | l -> Corpusbank.read_files l
let build_filter () =
  Corpusbank.build_filter (!Grew_cli_global.anonymous_args)

let filtered_list filter corpusbank =
  Corpusbank.fold ~filter
  (fun _ corpus_desc acc -> corpus_desc::acc) 
  corpusbank []

let filtered_count filter corpusbank =
  Corpusbank.fold ~filter
  (fun _ _ acc -> acc + 1) 
  corpusbank 0

let validate () = 
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.iter ~filter
    (fun corpus_id corpus_desc ->
      try Corpus_desc.validate ~env:!env corpus_desc
      with Grewlib.Error msg -> Log.warning "--> %s skipped (%s)" corpus_id msg
    ) corpusbank

let build_tables () = 
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.iter ~filter
    (fun corpus_id corpus_desc ->
      try Corpus_desc.build_tables ~env:!env corpus_desc
      with Grewlib.Error msg -> Log.warning "--> %s skipped (%s)" corpus_id msg
    ) corpusbank

let compile () =
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.compile ~force:!Grew_cli_global.force ~filter corpusbank

let clean () =
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  let filtered = filtered_list filter corpusbank in
  let really_clean () = List.iter Corpus_desc.clean filtered in
  if !Grew_cli_global.force
  then really_clean ()
  else
    let nb = List.length filtered in
    if nb <= 10
    then
      really_clean ()
    else
      let _ = printf "This will clean %d corpora, are you sure [y/N]?\n%!" nb in 
      let answer = read_line () in
      if answer = "y" || answer = "Y" 
      then really_clean ()
      else printf "Aborted\n"

let status () = 
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.print_status ~verbose:!verbose ~filter corpusbank

let build () =
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.build ~force:!Grew_cli_global.force  ~filter corpusbank

let search () =
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  let filtered = filtered_list filter corpusbank in
  let num = List.length filtered in
  printf "TOTAL: %d corp%s found\n" num (if num > 1 then "ora" else "us");
  List.iter
    (fun corpus_desc-> printf " ➔ %s\n%!" (Corpus_desc.get_id corpus_desc))
    filtered

let show () =
  let corpusbank = load_corpusbank () in
  let filter = build_filter () in
  Corpusbank.iter ~filter
    (fun _ corpus_desc -> Corpus_desc.show corpus_desc)
    corpusbank
