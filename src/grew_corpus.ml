(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
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

let fail kind msg loc_opt =
  let text = match loc_opt with
  | None -> msg
  | Some loc -> sprintf "[%s] %s" (Loc.to_string loc) msg in
  let rule = String.make (String.length text) '=' in
  Log.fwarning "\n%s\n%s\n%s" rule text rule; exit 2

let handle fct () =
  try fct ()
  with
    | Libgrew.File_not_found file ->        fail "IO" (sprintf "File not found: \"%s\"" file) None
    | Libgrew.Bug (msg,loc_opt) ->          fail "Bug" msg loc_opt
    | Libgrew.Build (msg,loc_opt) ->        fail "Build" msg loc_opt
    | Libgrew.Run (msg,loc_opt) ->          fail "Run" msg loc_opt
    | Libgrew.Parsing_err (msg,loc_opt) ->  fail "Parse" msg loc_opt

    | Corpus.File_not_found file ->         fail "IO" (sprintf "File not found: \"%s\"" file) None

    | Corpus.Fail msg ->                    fail "Load corpus" msg None
    | exc ->                                fail "Uncaught exception, please report" (Printexc.to_string exc) None

(* -------------------------------------------------------------------------------- *)
let init () =
  handle (fun () ->
    let output_dir = match !Grew_args.output_dir with
      | None -> Log.message "No output_dir specified: use -o option"; exit 1
      | Some dir -> dir in

    (* remove previous file or dir <output_dir> *)
    if Sys.file_exists output_dir
    then
      if Sys.is_directory output_dir
      then ignore (Sys.command("rm -rf " ^ output_dir))
      else Unix.unlink output_dir;

    (* create a fresh <output_dir> *)
    Unix.mkdir output_dir 0o777;

    (* load grs file *)
    let grs = Grs.load !Grew_args.grs in
    let domain = Grs.get_domain grs in

    (* generate documentation in the [doc] folder *)
    let _ = Grs.build_html_doc ~corpus:true (Filename.concat output_dir "doc") grs in

    (* get the list of graphs to rewrite *)
    let graph_array = Corpus.get_graphs ?domain !Grew_args.input_data in
    let base_names = Array.map fst graph_array in

    (* put the list of files to consider in the [index] file *)
    Rewrite.save_index ~dirname:output_dir ~base_names;

    (* put the grs file in the output folder *)
    let _ = Sys.command (sprintf "cp %s %s" !Grew_args.grs output_dir) in

    let sentences = ref [] in
    let len = Array.length graph_array in

    Array.iteri
      (fun index (base_name, gr) ->
        Counter.print index len base_name;
        let output_base = Filename.concat output_dir base_name in

        (* a flag used by write_error to know if html is needed *)
        let html = !Grew_args.html <> Grew_args.No in

        let stat_file = sprintf "%s.stat" output_base in
        let prev_opt =
          if !Grew_args.html = Grew_args.Full_html && index > 0
          then Some (fst (graph_array.(index-1)))
          else None
        and next_opt =
          if !Grew_args.html = Grew_args.Full_html && index < len-1
          then Some (fst (graph_array.(index+1)))
          else None in
        let header = Html.build_header prev_opt next_opt in

        try
          let rh = Rewrite.rewrite ~gr ~grs ~seq:!Grew_args.seq in
          Rewrite.write_stat stat_file rh;

          match !Grew_args.html with
            | Grew_args.No
            | Grew_args.Html when Rewrite.is_empty rh ->
              sentences := (false, base_name, 1, Graph.to_sentence ?main_feat:!Grew_args.main_feat gr) :: !sentences
            | _ ->
              sentences := (true, base_name, Rewrite.num_sol rh, Graph.to_sentence ?main_feat:!Grew_args.main_feat gr) :: !sentences;
              (* output gr files in corpus mode *)
              if !Grew_args.out_gr then Rewrite.save_gr ?domain output_base rh;

              (* output conll files in corpus mode *)
              if !Grew_args.out_conll then Rewrite.save_conll ?domain output_base rh;

              Rewrite.write_html
                ?domain
                ~no_init: !Grew_args.no_init
                ?filter: !Grew_args.features
                ?main_feat: !Grew_args.main_feat
                ~dot: !Grew_args.dot
                ~out_gr: !Grew_args.out_gr
                ~header
                rh
                output_base
        with

        | Libgrew.File_not_found file -> Html.write_error ?domain ~header ~html ~init:gr output_base (sprintf "The file %s doesn't exist!" file)
        | Libgrew.Bug (msg,loc_opt)
        | Libgrew.Build (msg,loc_opt)
        | Libgrew.Run (msg,loc_opt)
        | Libgrew.Parsing_err (msg,loc_opt) ->
          match loc_opt with
          | None -> Html.write_error ?domain ~header ~html ~init:gr output_base msg
          | Some loc -> Html.write_error ?domain ~header ~html ~init:gr output_base (sprintf "%s %s" msg (Loc.to_string loc))

      ) graph_array;
    Counter.finish ();

    let title = match !Grew_args.title with
      | Some s -> s
      | None -> sprintf "Grew corpus on input dir '%s'" (Filename.basename !Grew_args.input_data) in

    (* TODO all confluent in Grs module *)
    Rewrite.html_sentences ~title output_dir (List.rev !sentences);

    Rewrite.make_index
      ~title
      ~grs_file: !Grew_args.grs
      ~html: (match !Grew_args.html with Grew_args.No -> false | _ -> true)
      ~grs
      ~seq: !Grew_args.seq
      ~input_dir: !Grew_args.input_data
      ~output_dir
      ~base_names
  ) ()

(* -------------------------------------------------------------------------------- *)
let multi_conll ?(keep_empty_rh=false) () =
  handle (fun () ->
    let out_ch = match !Grew_args.output_file with
      | None -> Log.message "No output_file specified: use -f option"; exit 1
      | Some file -> open_out file in

    (* load grs file *)
    let grs = Grs.load !Grew_args.grs in
    let domain = Grs.get_domain grs in

    (* get the list of files to rewrite *)
    let graph_array = Corpus.get_graphs ?domain !Grew_args.input_data in
    let len = Array.length graph_array in

    Array.iteri
      (fun index (base_name, gr) ->
        Counter.print index len base_name;
        let rh = Rewrite.rewrite ~gr ~grs ~seq:!Grew_args.seq in
        match Rewrite.conll_dep_string ?domain ~keep_empty_rh rh with
          | None -> ()
          | Some string -> fprintf out_ch "%s\n" string
      ) graph_array;
    close_out out_ch;
    Counter.finish ()
  ) ()

(* -------------------------------------------------------------------------------- *)
let det () =
  handle (fun () ->
    if !Grew_args.input_data = ""
    then (Log.message "No input data specified: use -i option"; exit 1);

    match (!Grew_args.output_dir, !Grew_args.output_file) with
      | (None, None) -> Log.message "No output specified: use -o or -f option"; exit 1
      | (Some _, Some _) -> Log.message "Ambiguous output: you cannot use -o and -f options together"; exit 1
      | (None, Some output_file) -> multi_conll ~keep_empty_rh:true ()
      | (Some output_dir, None) ->
        if not (!Grew_args.out_gr || !Grew_args.out_conll)
        then (Log.message "No output format specified: use -out_gr or -out_conll option"; exit 1);

        (* remove previous file or dir <output_dir> *)
        if Sys.file_exists output_dir
        then
          if Sys.is_directory output_dir
          then ignore (Sys.command("rm -rf " ^ output_dir))
          else Unix.unlink output_dir;

        (* create a fresh <output_dir> *)
        Unix.mkdir output_dir 0o777;

        (* load grs file *)
        let grs = Grs.load !Grew_args.grs in
        let domain = Grs.get_domain grs in

        (* get the list of graphs to rewrite *)
        let graph_array = Corpus.get_graphs ?domain !Grew_args.input_data in
        let len = Array.length graph_array in

        Array.iteri
          (fun index (base_name, gr) ->
            Counter.print index len base_name;
            let output_base = Filename.concat output_dir base_name in
            let rh = Rewrite.rewrite ~gr ~grs ~seq:!Grew_args.seq in
            if !Grew_args.out_gr then Rewrite.save_det_gr ?domain output_base rh;
            if !Grew_args.out_conll then Rewrite.save_det_conll ?domain output_base rh
          ) graph_array;
        Counter.finish ()
      ) ()

(* -------------------------------------------------------------------------------- *)
let full () =
  handle (fun () ->
    if !Grew_args.input_data = ""
    then (Log.message "No input data specified: use -i option"; exit 1);

    match (!Grew_args.output_dir, !Grew_args.output_file) with
      | (None, None) -> Log.message "No output specified: use -o or -f option"; exit 1
      | (Some _, Some _) -> Log.message "Ambiguous output: you cannot use -o and -f options together"; exit 1
      | (None, Some output_file) -> multi_conll ~keep_empty_rh:true ()
      | (Some output_dir, None) ->
        if not (!Grew_args.out_gr || !Grew_args.out_conll)
        then (Log.message "No output format specified: use -out_gr or -out_conll option"; exit 1);

        (* remove previous file or dir <output_dir> *)
        if Sys.file_exists output_dir
        then
          if Sys.is_directory output_dir
          then ignore (Sys.command("rm -rf " ^ output_dir))
          else Unix.unlink output_dir;

        (* create a fresh <output_dir> *)
        Unix.mkdir output_dir 0o777;

        (* load grs file *)
        let grs = Grs.load !Grew_args.grs in
        let domain = Grs.get_domain grs in

        (* get the list of graphs to rewrite *)
        let graph_array = Corpus.get_graphs ?domain !Grew_args.input_data in
        let len = Array.length graph_array in

        Array.iteri
          (fun index (base_name, gr) ->
            Counter.print index len base_name;
            let output_base = Filename.concat output_dir base_name in
            let rh = Rewrite.rewrite ~gr ~grs ~seq:!Grew_args.seq in
            if !Grew_args.out_gr then failwith "Not yet";
            if !Grew_args.out_conll then ignore (Rewrite.save_full_conll ?domain output_base rh)
          ) graph_array;
        Counter.finish ()
      ) ()


(* -------------------------------------------------------------------------------- *)
  let dump_error kind msg loc_opt =
    let loc_string = match loc_opt with
    | None -> ""
    | Some loc -> Loc.to_string loc in
      printf "ERR: %s [%s] at %s\n" kind msg loc_string

  let grep () = handle
    (fun () ->
      match (!Grew_args.grs, !Grew_args.input_data, !Grew_args.pattern) with
      | ("",_,_) -> Log.message "No grs file specified: use -grs option"; exit 1
      | (_,"",_) -> Log.message "No input data specified: use -i option"; exit 1
      | (_,_,None) -> Log.message "No pattern file specified: use -pattern option"; exit 1;
      | (grs_file, data_file, Some pattern_request) ->

        match Str.split (Str.regexp "|") pattern_request with
        | [] -> Log.message "Empty pattern_request"; exit 1
        | pattern_file :: id_list ->

          let domain = Grs.get_domain (Grs.load grs_file) in

          (* get the list of graphs to explore *)
          let graph_array = Corpus.get_graphs ?domain data_file in

          let pattern = Pattern.load ?domain pattern_file in

          Array.iter
            (fun (name,graph) ->
              let matchings = Graph.search_pattern ?domain pattern graph in
              List.iter
                (fun matching ->
                  let node_matching = Graph.node_matching pattern graph matching in
                  printf "%s" name;
                  List.iter
                    (fun id ->
                      try printf "\t%d" (List.assoc id node_matching)
                      with Not_found -> Log.fmessage "Identifier %s not found in pattern %s" id pattern_file; exit 1
                    ) id_list;
                  printf "\n%!";
                ) matchings;
          ) graph_array
        ) ()
