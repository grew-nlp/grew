open Log

open Printf

open Cluster

open Libgrew
open Grew_utils
open Grew_args

let _ = Printexc.record_backtrace true

(* a generic function to report some args from FRONTAL to MASTER and from MASTER to SLAVE *)
let report_args () =
  Printf.sprintf " -grs %s -seq %s %s %s %s"
    !Grew_args.grs
    !Grew_args.seq
    (match !Grew_args.main_feat with None -> "" | Some mf -> sprintf "-main_feat %s" mf)
    (match !Grew_args.timeout with None -> "" | Some delay -> sprintf "-timeout %f" delay)
    (match !Grew_args.title with None -> "" | Some t -> sprintf "-title \"%s\"" t)

(*============================================================================*)
(* SLAVE code                                                                 *)
(*============================================================================*)
let exec_next () =
  let output_dir = match !Grew_args.output_dir with
    | None -> Log.message "No output_dir specified: use -o option!"; exit 1
    | Some dir -> dir in

  let input_data = !Grew_args.input_data in
  let grs = Libgrew.load_grs !Grew_args.grs in
  let html = !Grew_args.html <> Grew_args.No in

  let base_array = Array.of_list (Grew_utils.read_file (Filename.concat output_dir "index")) in
  let base_index base_name = Array_.dicho_find base_name base_array in

  List.iter 
    (fun graph_file ->
      let base_name = Filename.chop_extension (Filename.basename graph_file) in

      let index = base_index base_name in
      
      let prev_opt = 
        if !Grew_args.html = Grew_args.Full_html && index > 0
        then Some (base_array.(index-1))
        else None
      and next_opt = 
        if !Grew_args.html = Grew_args.Full_html && index < (Array.length base_array)-1
        then Some (base_array.(index+1))
        else None in
      
      let header = Grew_corpus.build_header prev_opt next_opt in

      let output_base = Filename.concat output_dir base_name in
      
      try
        let gr = Libgrew.load_graph (Filename.concat input_data graph_file) in
        try
          let rh = Libgrew.rewrite ~gr ~grs ~seq:!Grew_args.seq in
          
          Libgrew.write_stat (sprintf "%s.stat" output_base) rh;
          
          if !Grew_args.html <> Grew_args.No
          then 
            Libgrew.write_html
              ~no_init: !Grew_args.no_init
              ?main_feat: !Grew_args.main_feat
              ~dot: !Grew_args.dot
              ~header
              ~graph_file
              rh
              output_base
        with
	| Libgrew.Bug (msg,_) -> Grew_corpus.write_error ~header ~html ~init:gr output_base msg
	| Libgrew.File_dont_exists file -> Grew_corpus.write_error ~header ~html ~init:gr output_base (sprintf "The file %s doesn't exist!" file)
	| Libgrew.Build (msg,loc) 
	| Libgrew.Run (msg,loc) -> Grew_corpus.write_error ~header ~html ~init:gr output_base (sprintf "%s%s" msg (Grew_utils.string_of_loc loc))
      with 
        | Libgrew.Parsing_err msg -> Grew_corpus.write_error ~header ~html output_base msg
        | Libgrew.Build (msg,loc) -> Grew_corpus.write_error ~header ~html output_base (sprintf "%s%s" msg (Grew_utils.string_of_loc loc))
    ) !Grew_args.graphs

(*============================================================================*)
(* MASTER code                                                                *)
(*============================================================================*)
let start () =
  let output_dir = match !Grew_args.output_dir with
    | None -> Log.message "No output_dir specified: use -o option!"; exit 1
    | Some dir -> dir in
  
  (* get the list of files to rewrite *)
  let all_files = Array.to_list (Sys.readdir !Grew_args.input_data) in
  let graph_files =
    List.filter 
      (fun f -> (Filename.check_suffix f ".gr") || (Filename.check_suffix f ".conll")) 
      all_files in
  let graph_array = Array.of_list graph_files in
  let _ = Array.sort Pervasives.compare graph_array in  
  let len = Array.length graph_array in
  let base_names = Array.to_list (Array.map (fun f -> Filename.chop_extension f) graph_array) in
  
  (* put the list of files to consider in the [index] file *)
  Libgrew.save_index ~dirname:output_dir ~base_names;
  
  let counter = ref 0 in

  let next () =
    let first = !counter in
    let list = ref [] in
    for i=0 to !Grew_args.oar_graph_by_node - 1 do
      if !counter < len
      then list := graph_array.(!counter) :: !list
      else
        if !list = []
        then raise Cluster.No_more_partial;
      incr counter;
    done;
    let command = 
      Printf.sprintf "%s/grew -cluster %s -i %s -o %s %s -first %d -last %d -cluster_set %s"
        INSTALL_DIR
        (match !Grew_args.html with Grew_args.No -> "" | Grew_args.Html -> "-html" | Grew_args.Full_html -> "-full_html")
        !Grew_args.input_data
        output_dir
        (report_args ())
        first
        !counter
        (String.concat " " (List.rev !list))
    in
    Printf.sprintf "%s\n%!" command;
  in
  
  (* clustering start here *)
  Cluster.start ~next;

  (*==========================================================================*)
  (* Merge results                                                            *)
  (*==========================================================================*)
  
  let grs = Libgrew.load_grs ~doc_output_dir:(Filename.concat output_dir "doc") !Grew_args.grs in

  let title = match !Grew_args.title with
  | Some s -> s
  | None -> sprintf "Grew cluster on input_data '%s'" !Grew_args.input_data in

  let base_list = Grew_utils.read_file (Filename.concat output_dir "index") in
  Libgrew.html_sentences ~title output_dir
    (List.map
       (fun base ->
         let sentence = 
           try
             let gr = 
               try Libgrew.load_graph (Filename.concat !Grew_args.input_data (base^".conll"))
               with Sys_error _ -> Libgrew.load_graph (Filename.concat !Grew_args.input_data (base^".gr")) in
             Libgrew.to_sentence ?main_feat:!Grew_args.main_feat gr 
           with 
             | Libgrew.Parsing_err msg -> sprintf "Parsing error when loading '%s' graph: %s" base msg
             | Libgrew.Build (msg,loc) -> sprintf "Building error when loading '%s' graph: %s" base msg in
         if Sys.file_exists (Filename.concat output_dir (base ^ ".html")) 
         then
           begin
             let in_ch = open_in (Filename.concat output_dir (base ^ ".stat")) in
             let amb = match Str.split (Str.regexp ":") (input_line in_ch) with
               | ["NUM_SOL"; s] -> int_of_string s
               | _ -> -1 in
             (true, base, amb, sentence) 
           end
         else (false, base, 1, sentence)
       ) base_list
    );

  Libgrew.make_index
    ~title: title
    ~grs_file: !Grew_args.grs
    ~html: (!Grew_args.html <> Grew_args.No)
    ~grs
    ~seq: !Grew_args.seq
    ~input_data: !Grew_args.input_data
    ~output_dir:output_dir
    ~base_names;
    
  (*============================================*)
  (* Build of the tar.gz with all files         *)
  (*============================================*)
  
  let base_output = Filename.basename (Str.global_replace (Str.regexp "/$") "" output_dir) in
  let up_dir = Filename.dirname (Str.global_replace (Str.regexp "/$") "" output_dir) in

  let command = 
    Printf.sprintf "rm -rf %s/*.stat && cd %s && tar -czf %s.tar.gz %s/* && rm -rf %s/*.png %s/*.gr %s/*.conll %s/*.html %s/*.css"
      base_output up_dir base_output base_output base_output base_output base_output base_output base_output in

  let out = (Filename.concat output_dir "grew_cluster.stdout") in
  let out_ch = open_out out in
  
  match Sys.command command with
    | 0 -> 
      let user = try Sys.getenv "USER" with Not_found -> "GRID_5000_LOGIN" in
      
      Printf.fprintf out_ch "\nDone!\n\n%!";
      Printf.fprintf out_ch "You can download output with:\nscp -P 2222 %s@grid5000.loria.fr:%s.tar.gz YOU@YOUR_MACHINE:/THE_PATH_WHERE_TO_DOWNLOAD_THE_TARGZ\n\n%!"
        user
        (Filename.concat up_dir base_output);
    | error_code -> Printf.fprintf out_ch "\nA error code '%d' with the command '%s'\n\n%!" error_code command;
  close_out out_ch

(*============================================================================*)

(*============================================================================*)
(* FRONTAL code                                                                *)
(*============================================================================*)
let init () =
  if (!Grew_args.cluster_set)
  then (exec_next ();) 
  else
    if (!Grew_args.start_cluster) 
    then (start ();) 
    else 

      (* FRONTAL specific code starts here *)
      begin
        let output_dir =
          match !Grew_args.output_dir with
            | None -> Log.message "No output_dir specified: use -o option!"; exit 1
            | Some dir -> dir in
        
        (* remove old [output_dir] (if any) *)
        ignore(Sys.command("rm -rf " ^ output_dir));

        (* create a fresh [output_dir] *)
        Unix.mkdir output_dir 0o777;
        
        let command =
          Printf.sprintf 
            "%s/grew -cluster -graph_by_node %d %s -start_cluster -i %s -o %s %s" 
            INSTALL_DIR
            (!Grew_args.oar_graph_by_node)
            (match !Grew_args.html with Grew_args.No -> "" | Grew_args.Html -> "-html" | Grew_args.Full_html -> "-full_html")
            !Grew_args.input_data
            output_dir
            (report_args ()) in
        
        Cluster.oarsub 
          ~walltime: !Grew_args.oar_walltime
          ~nodes: !Grew_args.oar_nodes 
          ~command;
        
        (* loop until the file "grew_cluster.stdout" appears in the [output_dir] *)
        let stop = ref false in
        while (not !stop) do
          stop := Sys.file_exists (Filename.concat output_dir "grew_cluster.stdout");
          ignore(Unix.select [] [] [] 2.);
        done;
        
        let in_ch = open_in (Filename.concat output_dir "grew_cluster.stdout") in
        try while true do
            Printf.printf "%s\n%!" (input_line in_ch);
          done; assert(false);
        with End_of_file ->
          close_in in_ch;
      end
