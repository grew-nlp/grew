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

open Grew_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)

let fail kind msg loc_opt =
  let loc_string = match loc_opt with
  | None -> ""
  | Some loc -> Libgrew.string_of_loc loc in
    printf "\n====== Error: %s ======\n%s %s\n===================================\n" kind msg loc_string;
    exit 2

let handle fct () =
  try fct ()
  with

    | Libgrew.File_dont_exists file ->      fail "IO" (sprintf "File not found: \"%s\"" file) None
    | Libgrew.Bug (msg,loc_opt) ->          fail "Bug" msg loc_opt
    | Libgrew.Build (msg,loc_opt) ->        fail "Build" msg loc_opt
    | Libgrew.Run (msg,loc_opt) ->          fail "Run" msg loc_opt
    | Libgrew.Parsing_err (msg,loc_opt) ->  fail "Parse" msg loc_opt
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
    let grs = Libgrew.load_grs !Grew_args.grs in

    (* generate documentation in the [doc] folder *)
    let _ = Libgrew.build_html_doc ~corpus:true (Filename.concat output_dir "doc") grs in

    (* get the list of graphs to rewrite *)
    let graph_list = Corpus.get_graphs !Grew_args.input_data in
    let base_names = List.map fst graph_list in

    (* put the list of files to consider in the [index] file *)
    Libgrew.save_index ~dirname:output_dir ~base_names;

    (* put the grs file in the output folder *)
    let _ = Sys.command (sprintf "cp %s %s" !Grew_args.grs output_dir) in

    let sentences = ref [] in
    let graph_array = Array.of_list graph_list in
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
          let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
          Libgrew.write_stat stat_file rh;

          match !Grew_args.html with
            | Grew_args.No
            | Grew_args.Html when Libgrew.is_empty rh ->
              sentences := (false, base_name, 1, Libgrew.to_sentence ?main_feat:!Grew_args.main_feat gr) :: !sentences
            | _ ->
              sentences := (true, base_name, Libgrew.num_sol rh, Libgrew.to_sentence ?main_feat:!Grew_args.main_feat gr) :: !sentences;
              (* output gr files in corpus mode *)
              if !Grew_args.out_gr then Libgrew.save_gr output_base rh;

              (* output conll files in corpus mode *)
              if !Grew_args.out_conll then Libgrew.save_conll output_base rh;

              Libgrew.write_html
                ~no_init: !Grew_args.no_init
                ?filter: !Grew_args.features
                ?main_feat: !Grew_args.main_feat
                ~dot: !Grew_args.dot
                ~out_gr: !Grew_args.out_gr
                ~header
                rh
                output_base
        with

        | Libgrew.File_dont_exists file -> Html.write_error ~header ~html ~init:gr output_base (sprintf "The file %s doesn't exist!" file)
        | Libgrew.Bug (msg,loc_opt)
        | Libgrew.Build (msg,loc_opt)
        | Libgrew.Run (msg,loc_opt)
        | Libgrew.Parsing_err (msg,loc_opt) ->
          match loc_opt with
          | None -> Html.write_error ~header ~html ~init:gr output_base msg
          | Some loc -> Html.write_error ~header ~html ~init:gr output_base (sprintf "%s %s" msg (Libgrew.string_of_loc loc))

      ) graph_array;
    Counter.finish ();

    let title = match !Grew_args.title with
      | Some s -> s
      | None -> sprintf "Grew corpus on input dir '%s'" (Filename.basename !Grew_args.input_data) in

    (* TODO all confluent in Grs module *)
    Libgrew.html_sentences ~title output_dir (List.rev !sentences);

    Libgrew.make_index
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
    let grs = Libgrew.load_grs !Grew_args.grs in

    (* get the list of files to rewrite *)
    let graph_list = Corpus.get_graphs !Grew_args.input_data in
    let len = List.length graph_list in

    List.iteri
      (fun index (base_name, gr) ->
        Counter.print index len base_name;
        let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
        match Libgrew.conll_dep_string ~keep_empty_rh rh with
          | None -> ()
          | Some string -> fprintf out_ch "%s\n" string
      ) graph_list;
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
        let grs = Libgrew.load_grs !Grew_args.grs in

        (* get the list of graphs to rewrite *)
        let graph_list = Corpus.get_graphs !Grew_args.input_data in
        let len = List.length graph_list in

        List_.iteri
          (fun index (base_name, gr) ->
            Counter.print index len base_name;
            let output_base = Filename.concat output_dir base_name in
            let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
            if !Grew_args.out_gr then Libgrew.save_det_gr output_base rh;
            if !Grew_args.out_conll then Libgrew.save_det_conll output_base rh
          ) graph_list;
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
        let grs = Libgrew.load_grs !Grew_args.grs in

        (* get the list of graphs to rewrite *)
        let graph_list = Corpus.get_graphs !Grew_args.input_data in
        let len = List.length graph_list in

        List_.iteri
          (fun index (base_name, gr) ->
            Counter.print index len base_name;
            let output_base = Filename.concat output_dir base_name in
            let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
            if !Grew_args.out_gr then failwith "Not yet";
            if !Grew_args.out_conll then ignore (Libgrew.save_full_conll output_base rh)
          ) graph_list;
        Counter.finish ()
      ) ()


(* -------------------------------------------------------------------------------- *)
  let dump_error kind msg loc_opt =
    let loc_string = match loc_opt with
    | None -> ""
    | Some loc -> Libgrew.string_of_loc loc in
      printf "ERR: %s [%s] at %s\n" kind msg loc_string

  let grep () =
    handle (fun () ->
    if !Grew_args.input_data = ""
    then (Log.message "No input data specified: use -i option"; exit 1);

    (* TODO: init features and labels: load a grs file *)
    let grs = Libgrew.load_grs !Grew_args.grs in

    (* get the list of graphs to explore *)
    let graph_array = Array.map
      (fun (name, instance) -> (name, Libgrew.graph_of_instance instance))
      (Array.of_list (Corpus.get_graphs !Grew_args.input_data)) in
    let len = Array.length graph_array in
    printf "MSG:%d graphs loaded from '%s'\n%!" len !Grew_args.input_data;

    let patt = ref None in
    let index = ref 0 in

    let () = Unix.set_nonblock Unix.stdin in
    while true do
      begin
        match !patt with
          | None -> let _ = Unix.select [] [] [] 0.1 in ()
          | Some pattern ->
            let (name, graph) = graph_array.(!index) in
            let matchings = Libgrew.match_in_graph pattern graph in
            List.iter
              (fun matching ->
                let deco = Libgrew.match_deco pattern matching in
                let dep = Libgrew.to_dep_graph ~deco graph in
                let svg_file = Svg.dep_to_tmp dep in
                printf "MSG: found pattern in graph [%s] --> %s\n%!" name svg_file;
              ) matchings;
            incr index;
            if !index = len
            then (printf "MSG: Finish\n%!"; patt := None; index := 0)
      end;

      let next_command =
        try Some (read_line ())
        with _ -> None in

      begin
        match (!patt, next_command) with
        | (_, None) -> ()
        | (None, Some file) ->
          if Sys.file_exists file
          then
            (try patt := Some (Libgrew.load_pattern file)
            with
              | Libgrew.File_dont_exists file ->      dump_error "IO" (sprintf "File not found: \"%s\"" file) None
              | Libgrew.Bug (msg,loc_opt) ->          dump_error "Bug" msg loc_opt
              | Libgrew.Build (msg,loc_opt) ->        dump_error "Build" msg loc_opt
              | Libgrew.Run (msg,loc_opt) ->          dump_error "Run" msg loc_opt
              | Libgrew.Parsing_err (msg,loc_opt) ->  dump_error "Parse" msg loc_opt
              | exc ->                                dump_error "Uncaught exception, please report" (Printexc.to_string exc) None)
          else printf "ERR: File \"%s\" not found\n" file
        | (Some _, Some "STOP") -> (printf "MSG: Abort\n%!"; patt := None; index := 0)
        | _ -> printf "MSG: Cannot handle parallel commands\n"
      end
    done
  ) ()

(* -------------------------------------------------------------------------------- *)
let make_index () =
  let output_dir = match !Grew_args.output_dir with
    | None -> Log.message "No output_dir specified: use -o option!"; exit 1
    | Some dir -> dir in

  let title = match !Grew_args.title with
  | Some s -> s
  | None -> sprintf "Index for file in input_data '%s'" !Grew_args.input_data in
  let grs = Libgrew.load_grs !Grew_args.grs in
  let base_names = File.read (Filename.concat output_dir "index") in
  Libgrew.make_index
    ~title: title
    ~grs_file: !Grew_args.grs
    ~html: (match !Grew_args.html with Grew_args.No -> false | _ -> true)
    ~grs
    ~seq: !Grew_args.seq
    ~input_dir: !Grew_args.input_data
    ~output_dir:output_dir
    ~base_names

(* -------------------------------------------------------------------------------- *)
let annot () =
  handle (fun () ->
    let title = match !Grew_args.title with
    | Some s -> s
    | None -> sprintf "Annotation task in file \"%s\" on data \"%s\"" !Grew_args.grs (Filename.basename !Grew_args.input_data) in

    let annot_dir = match !Grew_args.output_dir with
      | None -> Log.message "No annot_dir specified: use -o option"; exit 1
      | Some dir -> dir in

    let static_dir = match !Grew_args.static_dir with
      | None -> Log.message "No static_dir specified. \".\" will be used."; "."
      | Some dir -> dir in

    (* remove previous file or dir <annot_dir> *)
    if Sys.file_exists annot_dir
    then
      if Sys.is_directory annot_dir
      then ignore (Sys.command("rm -rf " ^ annot_dir))
      else Unix.unlink annot_dir;

    (* create a fresh <annot_dir> *)
    Unix.mkdir annot_dir 0o777;

    (* load grs file *)
    let grs = Libgrew.load_grs !Grew_args.grs in

    (* get the list of graphs to rewrite *)
    let graph_list = Corpus.get_graphs !Grew_args.input_data in
    let len = List.length graph_list in

    let bn_rh_list =
      List_.mapi
        (fun index (base_name, gr) ->
          Counter.print index len base_name;
          (base_name, Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq)
        ) graph_list in
    Counter.finish ();
    Libgrew.write_annot ~title static_dir annot_dir bn_rh_list
  ) ()
