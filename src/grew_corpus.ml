open Printf

open Libgrew
open Log

open Grew_utils
open Grew_args

(* -------------------------------------------------------------------------------- *)
let write_error ?(header="") ~html ?init basename msg =
  let stat_file = sprintf "%s.stat" basename in

  let out_ch = open_out stat_file in
  fprintf out_ch "ERROR\n";
  fprintf out_ch "%s" msg;
  close_out out_ch;

  if html
  then Libgrew.error_html
      ~no_init: !Grew_args.no_init
      ?main_feat: !Grew_args.main_feat
      ~dot: !Grew_args.dot
      ~header
      msg
      ?init
      basename

(* -------------------------------------------------------------------------------- *)
let build_header prev_opt next_opt =
  match prev_opt, next_opt with
    | None, None -> ""
    | Some p, None -> sprintf "<a href=\"%s.html\">Previous</a>" p;
    | None, Some n -> sprintf "<a href=\"%s.html\">Next</a>" n;
    | Some p, Some n -> sprintf "<a href=\"%s.html\">Previous</a> -- <a href=\"%s.html\">Next</a>" p n

type data =
  | File of string
  | Conll of (int * string) list

(* -------------------------------------------------------------------------------- *)
let get_data () = (* get the list of files to rewrite *)
  if Sys.is_directory !Grew_args.input_data
  then
    begin
      let all_files = Array.to_list (Sys.readdir !Grew_args.input_data) in
      let graph_files =
        List.filter
          (fun f -> (Filename.check_suffix f ".gr") || (Filename.check_suffix f ".conll"))
          all_files in
      let graph_array = Array.of_list graph_files in
      let _ = Array.sort Pervasives.compare graph_array in
      Array.map (fun f -> (Filename.chop_extension f, File f)) graph_array
      end
  else
    begin
      let in_ch = open_in !Grew_args.input_data in
      (* if the input file contains an UTF-8 byte order mark (EF BB BF), skip 3 bytes, else get back to 0 *)
      (match input_byte in_ch with 0xEF -> seek_in in_ch 3 | _ -> seek_in in_ch 0);

      let cpt = ref 0 in
      let name = ref None in
      let res = ref [] in
      let last = ref [] in
      let line_num = ref 0 in

      let save_one () =
        match !name with
          | None -> ()
          | Some n ->
            res := (n, Conll (List.rev !last)) :: !res;
            last := [];
            name := None in
      try
        while true do
          incr line_num;
          match (!name, input_line in_ch) with
            | (_, "") -> save_one ()
            | Some oc, line -> last :=  (!line_num, line) :: !last
            | None, line ->
              (match Str.split (Str.regexp "\t") line with
                | [_;_;_;_;_;fs_string;_;_;_;_] ->
                  begin
                    try
                      let fs = List.map
                        (fun feat_string ->
                          match Str.split (Str.regexp "=") feat_string with
                            | [name;value] -> (name,value)
                            | _ -> failwith (Printf.sprintf "#1 >>%S<<\n%!" feat_string)
                      ) (Str.split (Str.regexp "|") fs_string) in
                      name := Some (List.assoc "sentid" fs)
                    with _ ->
                      Log.fwarning "File: %s, line:%d, unidentified sentence\n" !Grew_args.input_data !line_num;
                  end;
                  incr cpt;
                  last :=  (!line_num, line) :: !last
                | _ -> Printf.printf "Illegal Conll line >>>%s<<<<\n%!" line; exit 3
              )
        done; assert false

      with End_of_file ->
        save_one ();
        Array.of_list (List.rev !res)
    end

(* -------------------------------------------------------------------------------- *)
let init () =
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

  (* get the list of files to rewrite *)
  let data = get_data () in
  let base_names = List.map fst (Array.to_list data) in

  (* put the list of files to consider in the [index] file *)
  Libgrew.save_index ~dirname:output_dir ~base_names;

  (* put the grs file in the output folder *)
  let _ = Sys.command (sprintf "cp %s %s" !Grew_args.grs output_dir) in

  let sentences = ref [] in
  let len = Array.length data in

  Array.iteri
    (fun index (base_name, one) ->
      Counter.print index len base_name;
      let output_base = Filename.concat output_dir base_name in

      (* a flag used by write_error to know if html is needed *)
      let html = !Grew_args.html <> Grew_args.No in

      let stat_file = sprintf "%s.stat" output_base in
      let prev_opt =
        if !Grew_args.html = Grew_args.Full_html && index > 0
        then Some (fst (data.(index-1)))
        else None
      and next_opt =
        if !Grew_args.html = Grew_args.Full_html && index < len-1
        then Some (fst (data.(index+1)))
        else None in
      let header = build_header prev_opt next_opt in

      try
        let gr = match one with
          | File graph_file -> Libgrew.load_graph (Filename.concat !Grew_args.input_data graph_file)
          | Conll line_list -> Libgrew.of_conll !Grew_args.input_data line_list in
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

              match one with
                | File graph_file ->
                  Libgrew.write_html
                    ~no_init: !Grew_args.no_init
                    ?filter: !Grew_args.features
                    ?main_feat: !Grew_args.main_feat
                    ~dot: !Grew_args.dot
                    ~out_gr: !Grew_args.out_gr
                    ~header
                    ~graph_file
                    rh
                    output_base
                | Conll conll_txt ->
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
	  | Libgrew.Bug (msg,_) -> write_error ~header ~html ~init:gr output_base msg
	  | Libgrew.File_dont_exists file -> write_error ~header ~html ~init:gr output_base (sprintf "The file %s doesn't exist!" file)
	  | Libgrew.Build (msg,loc)
	  | Libgrew.Run (msg,loc) -> write_error ~header ~html ~init:gr output_base (sprintf "%s%s" msg (Grew_utils.string_of_loc loc))
              with
                | Libgrew.Parsing_err msg -> write_error ~header ~html output_base msg
                | Libgrew.Build (msg,loc) -> write_error ~header ~html output_base (sprintf "%s%s" msg (Grew_utils.string_of_loc loc))
    ) data;
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

(* -------------------------------------------------------------------------------- *)
let multi_conll ?(keep_empty_rh=false) () =
  let out_ch = match !Grew_args.output_file with
    | None -> Log.message "No output_file specified: use -f option"; exit 1
    | Some file -> open_out file in

  (* load grs file *)
  let grs = Libgrew.load_grs !Grew_args.grs in

  (* get the list of files to rewrite *)
  let data = get_data () in
  let len = Array.length data in

  Array.iteri
    (fun index (base_name, one) ->
      Counter.print index len base_name;
      try
        let gr = match one with
          | File graph_file -> Libgrew.load_graph (Filename.concat !Grew_args.input_data graph_file)
          | Conll line_list -> Libgrew.of_conll !Grew_args.input_data line_list in
        let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
        match Libgrew.conll_dep_string ~keep_empty_rh rh with
          | None -> ()
          | Some string -> fprintf out_ch "%s\n" string
      with
	| Libgrew.Bug (msg,_) ->
          printf "\n====== Error: Libgrew.Bug ======\n%s\n===================================\n" msg;
          exit 2
	| Libgrew.File_dont_exists file ->
          printf "\n====== Error: Libgrew.File_dont_exists ======\n%s\n===================================\n" file;
          exit 2
	| Libgrew.Build (msg,loc) ->
          printf "\n====== Error: Libgrew.Build ======\n%s %s\n===================================\n" msg (Grew_utils.string_of_loc loc);
          exit 2
	| Libgrew.Run (msg,loc) ->
          printf "\n====== Error: Libgrew.run ======\n%s %s\n================================\n" msg (Grew_utils.string_of_loc loc);
          exit 2
        | Libgrew.Parsing_err msg ->
          printf "\n====== Error: Libgrew.Parsing_err ======\n%s\n===================================\n" msg;
          exit 2
    ) data;
  close_out out_ch;
  Counter.finish ()

(* -------------------------------------------------------------------------------- *)
let init () =
  try
    init ()
  with
    | Libgrew.Bug (msg,_) ->
      printf "\n====== Error: Libgrew.Bug ======\n%s\n===================================\n" msg;
      exit 2
    | Libgrew.File_dont_exists file ->
      printf "\n====== Error: Libgrew.File_dont_exists ======\n%s\n===================================\n" file;
      exit 2
    | Libgrew.Build (msg,loc) ->
      printf "\n======= Error: Libgrew.Build =======\n%s %s\n====================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Run (msg,loc) ->
      printf "\n====== Error: Libgrew.run ======\n%s %s\n================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Parsing_err msg ->
      printf "\n====== Error: Libgrew.Parsing_err ======\n%s\n===================================\n" msg;
      exit 2

(* -------------------------------------------------------------------------------- *)

let det () =
  try
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

        (* get the list of files to rewrite *)
        let data = get_data () in
        let len = Array.length data in

        Array.iteri
          (fun index (base_name, one) ->
            Counter.print index len base_name;
            let output_base = Filename.concat output_dir base_name in
            let gr = match one with
              | File graph_file -> Libgrew.load_graph (Filename.concat !Grew_args.input_data graph_file)
              | Conll line_list -> Libgrew.of_conll !Grew_args.input_data line_list in
            let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
            if !Grew_args.out_gr then Libgrew.save_det_gr output_base rh;
            if !Grew_args.out_conll then Libgrew.save_det_conll output_base rh
          ) data;
        Counter.finish ()
  with
    | Libgrew.Bug (msg,_) ->
      printf "\n====== Error: Libgrew.Bug ======\n%s\n===================================\n" msg;
      exit 2
    | Libgrew.File_dont_exists file ->
      printf "\n====== Error: Libgrew.File_dont_exists ======\n%s\n===================================\n" file;
      exit 2
    | Libgrew.Build (msg,loc) ->
      printf "\n====== Error: Libgrew.Build ======\n%s %s\n===================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Run (msg,loc) ->
      printf "\n====== Error: Libgrew.run ======\n%s %s\n================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Parsing_err msg ->
      printf "\n====== Error: Libgrew.Parsing_err ======\n%s\n===================================\n" msg;
      exit 2

(* -------------------------------------------------------------------------------- *)
let make_index () =
  let output_dir = match !Grew_args.output_dir with
    | None -> Log.message "No output_dir specified: use -o option!"; exit 1
    | Some dir -> dir in

  let title = match !Grew_args.title with
  | Some s -> s
  | None -> sprintf "Index for file in input_data '%s'" !Grew_args.input_data in
  let grs = Libgrew.load_grs !Grew_args.grs in
  let base_names = Grew_utils.read_file (Filename.concat output_dir "index") in
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

  (* get the list of files to rewrite *)
  let data = get_data () in
  let base_names = List.map fst (Array.to_list data) in

  let sentences = ref [] in
  let len = Array.length data in

  try
    let bn_rh_list =
      Array.to_list
        (Array.mapi
           (fun index (base_name, one) ->
             Counter.print index len base_name;
             let gr = match one with
               | File graph_file -> Libgrew.load_graph (Filename.concat !Grew_args.input_data graph_file)
               | Conll line_list -> Libgrew.of_conll !Grew_args.input_data line_list in
             (base_name, Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq)
           ) data
        ) in
    Counter.finish ();
    Libgrew.write_annot static_dir annot_dir bn_rh_list

  with
    | Libgrew.Bug (msg,_) ->
      printf "\n====== Error: Libgrew.Bug ======\n%s\n===================================\n" msg;
      exit 2
    | Libgrew.File_dont_exists file ->
      printf "\n====== Error: Libgrew.File_dont_exists ======\n%s\n===================================\n" file;
      exit 2
    | Libgrew.Build (msg,loc) ->
      printf "\n====== Error: Libgrew.Build ======\n%s %s\n===================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Run (msg,loc) ->
      printf "\n====== Error: Libgrew.run ======\n%s %s\n================================\n" msg (Grew_utils.string_of_loc loc);
      exit 2
    | Libgrew.Parsing_err msg ->
      printf "\n====== Error: Libgrew.Parsing_err ======\n%s\n===================================\n" msg;
      exit 2
