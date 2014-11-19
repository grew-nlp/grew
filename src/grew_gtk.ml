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

open Libgrew
open GMain
open Log

open Grew_glade
open Grew_rew_display
open Grew_utils
open Grew_args
open Grew_config

let warning_dialog
    ?(message_type=`WARNING)
    ?(icon_name="ERROR")
    ?(buttons=GWindow.Buttons.ok)
    ?(message="")
    ?(callback=(fun e -> ())) () =
  let dlg = GWindow.message_dialog
    ~message_type
    ~modal:true
    ~urgency_hint:true
    ~type_hint:`NORMAL
    ~buttons
    ~icon_name
    ~show:true
    ~message
    () in
  let _ = dlg#connect#response ~callback:(fun e -> callback e; dlg#destroy ()) in
  dlg#show ();
  dlg

(* ==================================================================================================== *)
(* code taken from lablgtk2 examples *)
let ask_for_file_to_open filter parent =
  let res = ref None in
  let dialog = GWindow.file_chooser_dialog
    ~action:`OPEN
    ~title:"Open File"
    ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `OPEN `OPEN ;
  dialog#add_filter filter;
  begin match dialog#run () with
    | `OPEN -> res := dialog#filename
    | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ();
  !res
(* ==================================================================================================== *)

(* ==================================================================================================== *)
let ask_for_file_to_save filter parent =
  let res = ref None in
  let dialog = GWindow.file_chooser_dialog
    ~action:`SAVE
    ~title:"Save File"
    ~parent () in
  dialog#add_button_stock `CANCEL `CANCEL ;
  dialog#add_select_button_stock `SAVE `SAVE ;
  dialog#add_filter filter;
  begin match dialog#run () with
    | `SAVE -> res := dialog#filename
    | `DELETE_EVENT | `CANCEL -> ()
  end ;
  dialog#destroy ();
  !res

(* ==================================================================================================== *)


(* ==================================================================================================== *)
module Resources = struct
  let current_grs = ref None
  let current_grs_file = ref None
  let current_gr = ref None
  let current_gr_file = ref None

  (* -------------------------------------------------------------------------------- *)
  let load_grs () =
    match !current_grs_file with
      | None -> Log.message "No grs file defined"
      | Some grs_file ->
        Log.fmessage "Loading grs file: '%s'" grs_file;
        let grs = Libgrew.load_grs grs_file in
        current_grs := Some grs

  (* -------------------------------------------------------------------------------- *)
  (* must be called after a grs is loaded *)
  let load_gr () =
    match !current_gr_file with
      | None -> Log.message "No gr file defined"
      | Some file ->
        Log.fmessage "Loading gr file: '%s'" file;
        current_gr := Some (Libgrew.load_graph file)

  (* -------------------------------------------------------------------------------- *)
  exception Cannot_rewrite of string
  let rewrite seq =
    match (!current_grs, !current_gr) with
      | (Some grs, Some gr) -> Libgrew.display gr grs seq
(*         let t = Timer.create () in
        let res = Libgrew.display gr grs seq in
        printf "====> %f\n%!" (Timer.get t);
        res *)
      | (None, _) -> raise (Cannot_rewrite "No grs file loaded")
      | (_, None) -> raise (Cannot_rewrite "No graph file loaded")
end (* module Resources *)
(* ==================================================================================================== *)

(* ------------------------------------------------------------ *)
let filter_features = ref true
let current_features = ref []

let get_current_filter () =
  match (!filter_features, !current_features) with
    | (false, _) -> None
    | (true, l) -> Some (List.map fst (List.filter snd l))

let feat_set label value =
  let rec loop = function
    | [] -> []
    | (x,_)::t when x=label -> (x,value)::t
    | c::t -> c::(loop t) in
  current_features := loop !current_features

let (config_vbox : GPack.box option ref) = ref None (* the flag is true iff the config windows is open *)

let fill_vbox vbox =
  List.iter
    (fun (label,active) ->
      let item = GButton.check_button ~label ~active ~packing:vbox#add () in
      let _ = item#connect#toggled ~callback:(fun () -> feat_set label item#active) in
      ()
    ) !current_features


(* when GRS change *)
let update_features () =
  (match Libgrew.feature_names () with
    | None -> Log.warning "Cannot update features"
    | Some feats -> current_features := List.map (fun x -> (x,true)) feats);

  match !config_vbox with
    | None -> ()
    | Some vbox -> (* if the config windows is opened, it must be updated *)
      List.iter (fun item -> item#destroy ()) vbox#children;
      fill_vbox vbox

(* ------------------------------------------------------------ *)
let display_config_window () =
  if !config_vbox = None
  then
    begin
      let win = new config_window () in
      config_vbox := Some win#config_vbox;

      fill_vbox win#config_vbox;

      let () = win#main_feat#set_text !Grew_config.current_config.Grew_config.main_feat in

      let _ = win#main_feat#connect#changed
        ~callback: (fun () -> !Grew_config.current_config.Grew_config.main_feat <- win#main_feat#text) in

      let _ = win#btn_config_close#connect#clicked ~callback:
        (fun () -> config_vbox := None; win#toplevel#destroy ()) in
      let _ = win#toplevel#event#connect#delete ~callback: (fun _ -> config_vbox := None; false) in
      win#toplevel#show ()
    end


type save = Png | Pdf_dep | Pdf_dot | Dep | Dot | Gr | Conll
type side = Top | Bottom

let string_of_side = function Top -> "top" | Bottom -> "bottom"
(* ==================================================================================================== *)
let init () =

  let doc_dir = ref None in

  let _ = Grew_config.read_config () in

  (match !Grew_args.main_feat with
    | None -> ()
    | Some s -> !Grew_config.current_config.Grew_config.main_feat <- s);

  let empty_html = "<html><body><font color=red fontname=Arial>Nothing to display</font></body></html>" in

  let _ = GMain.Main.init () in
  let grew_window = new grew_window () in

  let _ = grew_window#btn_preferences#connect#clicked ~callback: (fun _ -> display_config_window ()) in (* XXX *)

  (*   let _ = GMenu.tearoff_item ~packing:grew_window#feature_menu#add () in
       let feature_item = GMenu.check_menu_item ~label:"filter" ~packing:grew_window#feature_menu#add () in
       let _ = feature_item#connect#toggled ~callback:(fun () -> filter_features := feature_item#active) in
       let _ = GMenu.separator_item ~packing:grew_window#feature_menu#add () in
  *)

  (** WEBKITS CREATIONS *)
  let doc_webkit = GWebView.web_view ~packing:grew_window#doc_view#add () in
  let grs_webkit = GWebView.web_view ~packing:grew_window#grs_view#add () in
  let module_webkit = GWebView.web_view ~packing:grew_window#module_view#add () in
  let error_webkit = GWebView.web_view ~packing:grew_window#err_view_scroll#add () in

  (* [graph_*_webkit] are put inside an event_box to handle right_click for contextual menu *)
  let top_event_box = GBin.event_box ~packing:grew_window#graph_view_top#add () in
  let _ = top_event_box#event#add [`BUTTON_PRESS] in
  let graph_top_webkit = GWebView.web_view ~packing:top_event_box#add () in

  let bottom_event_box = GBin.event_box ~packing:grew_window#graph_view_bottom#add () in
  let _ = bottom_event_box#event#add [`BUTTON_PRESS] in
  let graph_bottom_webkit = GWebView.web_view ~packing:bottom_event_box#add () in

  doc_webkit#set_full_content_zoom true;
  grs_webkit#set_full_content_zoom true;
  module_webkit#set_full_content_zoom true;
  graph_top_webkit#set_full_content_zoom true;
  graph_bottom_webkit#set_full_content_zoom true;
  error_webkit#set_full_content_zoom true;

  (* ensure UTF-8 encoding *)
  doc_webkit#set_custom_encoding "UTF-8";
  grs_webkit#set_custom_encoding "UTF-8";
  module_webkit#set_custom_encoding "UTF-8";
  graph_top_webkit#set_custom_encoding "UTF-8";
  graph_bottom_webkit#set_custom_encoding "UTF-8";
  error_webkit#set_custom_encoding "UTF-8";

  (* By default disable the contextual menu on webview *)
  let web_settings_def = GWebSettings.web_settings () in
  web_settings_def#set_enable_default_context_menu false;
  grs_webkit#set_settings web_settings_def;
  module_webkit#set_settings web_settings_def;
  graph_top_webkit#set_settings web_settings_def;
  graph_bottom_webkit#set_settings web_settings_def;
  error_webkit#set_settings web_settings_def;

  (* Doc is usual html view: the contextual menu is enable *)
  let web_settings_doc = GWebSettings.web_settings () in
  web_settings_doc#set_enable_default_context_menu true;
  doc_webkit#set_settings web_settings_doc;

  doc_webkit#set_zoom_level ((float_of_int !Grew_config.current_config.Grew_config.last_doc_zoom) /. 100.);
  grs_webkit#set_zoom_level ((float_of_int !Grew_config.current_config.Grew_config.last_rewriting_history_zoom) /. 100.);
  module_webkit#set_zoom_level ((float_of_int !Grew_config.current_config.Grew_config.last_rule_zoom) /. 100.);
  graph_top_webkit#set_zoom_level ((float_of_int !Grew_config.current_config.Grew_config.last_top_graph_zoom) /. 100.);
  graph_bottom_webkit#set_zoom_level ((float_of_int !Grew_config.current_config.Grew_config.last_bottom_graph_zoom) /. 100.);

  grew_window#doc_zoom#adjustment#set_value (float_of_int !Grew_config.current_config.Grew_config.last_doc_zoom);
  grew_window#grs_zoom#adjustment#set_value (float_of_int !Grew_config.current_config.Grew_config.last_rewriting_history_zoom);
  grew_window#graph_top_zoom#adjustment#set_value (float_of_int !Grew_config.current_config.Grew_config.last_top_graph_zoom);
  grew_window#graph_bottom_zoom#adjustment#set_value (float_of_int !Grew_config.current_config.Grew_config.last_bottom_graph_zoom);
  grew_window#module_zoom#adjustment#set_value (float_of_int !Grew_config.current_config.Grew_config.last_rule_zoom);

  let refresh_doc_webkit () =
    match !doc_dir with
      | Some dir ->
        doc_webkit#load_uri ("file://"^(Filename.concat dir "index.html"));
        grew_window#build_doc#misc#hide ()
      | None ->
        doc_webkit#load_html_string empty_html "";
        grew_window#build_doc#misc#show () in

  let build_doc () =
    match !Resources.current_grs with
      | None -> ()
      | Some grs ->
        let temp_dir_name = Filename.get_temp_dir_name () in
        let dir = Filename.concat temp_dir_name "grew" in
        Libgrew.build_html_doc dir grs;
        doc_dir := Some dir;
        refresh_doc_webkit () in

  let reset ()  =
    (* empty all webkits *)
    graph_top_webkit#load_html_string empty_html "";
    graph_bottom_webkit#load_html_string empty_html "";
    module_webkit#load_html_string empty_html "";
    grs_webkit#load_html_string empty_html "";
    refresh_doc_webkit ();

    Grew_rew_display.current_bottom_graph := "";
    Grew_rew_display.current_top_graph := "";

    (* reset the default panes *)
    grew_window#vpaned_doc#misc#show ();
    grew_window#err_view_scroll#misc#hide ();
    grew_window#vpane_right#set_position 30;
    grew_window#btn_show_module#set_active false;
    grew_window#vpaned_doc#set_position 30;
    grew_window#btn_show_doc#set_active false;
    grew_window#vpaned_left#set_position 30;
    grew_window#btn_show_grs#set_active false in

  let show_error msg =
    grew_window#err_view_scroll#misc#show ();
    let (error_file,out_ch) = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".html" in
    Printf.fprintf out_ch
      "<html><body><font color=red fontname=Arial>%s</font></body></html>"
      (Str.global_replace (Str.regexp "\n") "<br/>\n" msg);
    close_out out_ch;
    error_webkit#load_uri ("file://"^error_file) in

  let error_handling fct () =
    try fct ()
    with
      | Libgrew.Parsing_err msg -> show_error msg
      | Libgrew.File_dont_exists file -> show_error (Printf.sprintf "The file %s doesn't exist!" file)
      | Libgrew.Build (msg,loc) ->
        begin match loc with
          | None -> show_error msg
          | Some (file,line) -> show_error (Printf.sprintf "%s\nfile: %s\nline: %d" msg file line)
        end
      | Libgrew.Run (msg,loc) ->
        begin match loc with
          | None -> show_error msg
          | Some (file,line) -> show_error (Printf.sprintf "%s\nfile: %s\nline: %d" msg file line)
        end
      | Libgrew.Bug (msg,_) -> show_error msg
      | exc -> show_error (Printexc.to_string exc)
  in

  (** CALLBACKS *)

  (* force html doc building with the "Build HTML doc" button *)
  let _ = grew_window#build_doc#connect#clicked (fun () -> error_handling build_doc ()) in

  let seq_combo = ref grew_window#seq_list in

  let seq_list = ref [] in

  (* -------------------------------------------------------------------------------- *)
  let load_gr () =
    match !Resources.current_grs_file with
      | None -> ()
      | _ ->
        reset();
        Resources.load_gr ();
        (match !Resources.current_gr_file with
          | Some f -> grew_window#graph_label#set_label (Filename.basename f)
          | None -> ()
        );

        match !Resources.current_gr with
          | None -> ()
          | Some instance ->
            let graph = Libgrew.graph_of_instance instance in
            Grew_rew_display.graph_map := [("init", (graph, ("", "", None)))];
            Grew_rew_display.current_top_graph := "init";

            let svg_file =
              if grew_window#btn_gr_top_dot#active
              then Grew_rew_display.svg_dot_temp_file ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) graph
              else Grew_rew_display.svg_dep_temp_file ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) graph in

            grew_window#vpaned_doc#misc#show ();
            grew_window#err_view_scroll#misc#hide ();
            graph_top_webkit#load_uri ("file://"^svg_file) in
  (* end: load_gr *)
  (* -------------------------------------------------------------------------------- *)
  let gr_or_conll_filter = GFile.filter ~name:"Graph *.(gr|conll)" ~patterns:["*.gr"; "*.conll"] () in

  (* click on the gr file name *)
  let _ = grew_window#graph_button#connect#clicked
    (fun () ->
      match ask_for_file_to_open gr_or_conll_filter grew_window#toplevel with
        | None -> ()
        | Some f -> Resources.current_gr_file := Some f; error_handling load_gr ()
    ) in

  (* click on the gr refresh button *)
  let _ = grew_window#btn_refresh_gr#connect#clicked ~callback: (fun () -> error_handling load_gr ()) in

  (* -------------------------------------------------------------------------------- *)
  let load_grs () =
    reset ();
    Resources.load_grs ();
    match !Resources.current_grs with
      | None ->
          let _ = grew_window#btn_run#misc#set_sensitive false in
          show_error "No grs file loaded"
      | Some grs ->
        let _ = grew_window#btn_run#misc#set_sensitive true in
        seq_list := Libgrew.get_sequence_names grs;
        List.iter
          (fun c -> grew_window#seq_list_viewport#remove c)
          grew_window#seq_list_viewport#children;

        let (a,_) = GEdit.combo_box_text ~strings:(!seq_list) ~packing:grew_window#seq_list_viewport#add () in
        seq_combo := a;
        !seq_combo#set_active 0;

        (* next line: avoid to lose your seq choice when reloading grs file *)
        let _ = !seq_combo#connect#changed
          ~callback:
            (fun () ->
              try Grew_args.seq := List.nth !seq_list (!seq_combo#active)
              with Invalid_argument("List.nth") -> ()
            ) in

        grew_window#grs_label#set_label
          (match !Resources.current_grs_file with None -> "No Grs loaded" | Some f -> Filename.basename f);

        (match List_.index !Grew_args.seq !seq_list with
          | None -> ()
          | Some i -> !seq_combo#set_active i);

        update_features ();

        doc_dir := None;
        refresh_doc_webkit ();

        (* always reload a gr file after a grs change *)
        error_handling load_gr () in
  (* end: load_grs *)
  (* -------------------------------------------------------------------------------- *)




  (* click on the grs file name *)
  let grs_filter = GFile.filter ~name:"Graph Rewriting System (*.grs)" ~patterns:["*.grs"] () in
  let _ = grew_window#grs_button#connect#clicked
    (fun () ->
      match ask_for_file_to_open grs_filter grew_window#toplevel with
        | None -> ()
        | Some new_grs ->
          Resources.current_grs_file := Some new_grs;
          error_handling load_grs ()
    ) in

  (* click on the grs refresh button *)
  let _ = grew_window#btn_refresh_grs#connect#clicked ~callback: (fun () -> error_handling load_grs ()) in


  let check_positions () =
    if (grew_window#vpaned_doc#position < 30)
    then (grew_window#vpaned_doc#set_position 30;
          grew_window#btn_show_doc#set_active false);

    if (grew_window#vpaned_left#position < 30)
    then (grew_window#vpaned_left#set_position 30;
          grew_window#btn_show_grs#set_active false);

    if (grew_window#vpane_right#position < 30)
    then (grew_window#vpane_right#set_position 30;
          grew_window#btn_show_module#set_active false) in

  let _ = grew_window#btn_show_doc#connect#clicked
    ~callback:
    (fun () ->
      if grew_window#btn_show_doc#active
      then grew_window#vpaned_doc#set_position 250
      else (grew_window#vpaned_doc#set_position 30; check_positions ())
    ) in

  let _ = grew_window#vpaned_doc#event#connect#button_release
    ~callback:
    (fun b ->
      if (grew_window#vpaned_doc#position > 30)
      then (grew_window#btn_show_doc#set_active true)
      else (grew_window#vpaned_doc#set_position 30;
            grew_window#btn_show_doc#set_active false);
      check_positions ();
      false
    ) in

  let _ = grew_window#btn_show_grs#connect#clicked
    ~callback:
    (fun () ->
      if grew_window#btn_show_grs#active
      then grew_window#vpaned_left#set_position (!Grew_config.current_config.Grew_config.last_grs_position)
      else (grew_window#vpaned_left#set_position 30; check_positions ())
    ) in

  let _ = grew_window#vpaned_left#event#connect#button_release
    ~callback:
    (fun _ ->
      if (grew_window#vpaned_left#position > 30)
      then (grew_window#btn_show_grs#set_active true;
            !Grew_config.current_config.Grew_config.last_grs_position <- grew_window#vpaned_left#position)
      else (grew_window#vpaned_left#set_position 30;
            grew_window#btn_show_grs#set_active false);
      check_positions ();
      false
    ) in

  let _ = grew_window#btn_show_module#connect#clicked
    ~callback:
    (fun () ->
      if grew_window#btn_show_module#active
      then (grew_window#vpane_right#set_position (!Grew_config.current_config.Grew_config.last_module_position))
      else (grew_window#vpane_right#set_position 30; check_positions ())
    ) in

  let _ = grew_window#vpane_right#event#connect#button_release
    ~callback:
    (fun b ->
      if (grew_window#vpane_right#position > 30)
      then (grew_window#btn_show_module#set_active true;
            !Grew_config.current_config.Grew_config.last_module_position <- grew_window#vpane_right#position)
      else (grew_window#vpane_right#set_position 30;
            grew_window#btn_show_module#set_active false);
      check_positions ();
      false
    ) in

  let _ =  grew_window#toplevel#connect#destroy ~callback:(GMain.quit) in

  let _ =
    grew_window#btn_run#connect#clicked
      ~callback:
      (fun () ->
        try
          let rew_display = Resources.rewrite (List.nth !seq_list (!seq_combo#active)) in
          let fl = ref "G0" in
          grew_window#vpane_right#set_position 30;
          grew_window#btn_show_module#set_active false;
          grew_window#vpaned_doc#set_position 30;
          grew_window#btn_show_doc#set_active false;
          grew_window#btn_show_grs#set_active true;
          graph_top_webkit#load_html_string empty_html "";
          graph_bottom_webkit#load_html_string empty_html "";
          refresh_doc_webkit ();
          module_webkit#load_html_string empty_html "";

          if (grew_window#vpaned_left#position <= 30)
          then (grew_window#vpaned_left#set_position 250);

          let (fleaf,file_svg) = Grew_rew_display.rew_display_to_svg rew_display in
          fl := fleaf;
          Grew_rew_display.transform ~show_bottom:true file_svg (file_svg^".trans.svg") !fl;
          grs_webkit#load_uri ("file://"^file_svg^".trans.svg");
          Log.debug ("[Grew_gtk] file://"^file_svg^".trans.svg");
          grew_window#vpaned_doc#misc#show ();
          grew_window#err_view_scroll#misc#hide ();
          grs_webkit#execute_script("alert('showOnTop2::G0')");
          grs_webkit#execute_script("alert('showOnBottom2::"^(!fl)^"')");
        with
          | Resources.Cannot_rewrite msg -> show_error msg
          | Libgrew.Parsing_err msg -> show_error msg
          | Libgrew.File_dont_exists file -> show_error (Printf.sprintf "The file %s doesn't exist!" file)
          | Libgrew.Build (msg,loc) ->
            begin match loc with
              | None -> show_error msg
              | Some (file,line) -> show_error (Printf.sprintf "%s\nfile: %s\nline: %d" msg file line)
            end
          | Libgrew.Run (msg,loc) ->
            begin match loc with
              | None -> show_error msg
              | Some (file,line) -> show_error (Printf.sprintf "%s\nfile: %s\nline: %d" msg file line)
            end
          | Libgrew.Bug (msg,_) -> show_error msg
      ) in

  (** CLICK ON SVG GRAPHS *)
  let _ = grs_webkit#connect#script_alert
    ~callback:
    (fun _ msg ->
      match Str.split (Str.regexp "::") msg with
       | ["showOnBottom"; graph] ->
          let svg_file =
            if grew_window#btn_gr_bottom_dot#active
            then (Grew_rew_display.get_dot_graph_with_background
                   ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) graph)
            else (Grew_rew_display.get_dep_graph_with_background ~filter:(get_current_filter ())
                   ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) graph) in
          graph_bottom_webkit#load_uri ("file://"^svg_file);
          Grew_rew_display.current_bottom_graph := graph;
          module_webkit#load_html_string empty_html "";
          grs_webkit#execute_script
           "if (get_edge_flag()) { remove_back_from_current_top(); hide_current_edge(); current_edge_two='qsd';current_top_graph='';current_bottom_graph=''; alert('removeTop'); }";
          grs_webkit#execute_script "set_edge_flag(false)";
          grew_window#btn_show_module#set_active false;
          grew_window#vpane_right#set_position 30;
          true
        | ["showOnTop"; graph] ->
          let svg_file =
            if grew_window#btn_gr_top_dot#active
            then (Grew_rew_display.get_dot_graph_with_background
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) graph)
            else (Grew_rew_display.get_dep_graph_with_background ~filter:(get_current_filter ())
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) graph) in
          graph_top_webkit#load_uri ("file://"^svg_file);
          Grew_rew_display.current_top_graph := graph;
          module_webkit#load_html_string empty_html "";
          grs_webkit#execute_script "if (get_edge_flag()) { remove_back_from_current_bottom(); hide_current_edge(); current_edge_two='qsd';current_top_graph='';current_bottom_graph=''; alert('removeBottom'); }";
          grs_webkit#execute_script "set_edge_flag(false)";
          grew_window#btn_show_module#set_active false;
          grew_window#vpane_right#set_position 30;
          true
        | ["showOnBottom2"; graph] ->
          let svg_file =
            if grew_window#btn_gr_bottom_dot#active
            then (Grew_rew_display.get_dot_graph_with_background
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) graph)
            else (Grew_rew_display.get_dep_graph_with_background ~filter:(get_current_filter ())
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) graph) in
          graph_bottom_webkit#load_uri ("file://"^svg_file);
          Grew_rew_display.current_bottom_graph := graph;
          true
        | ["showOnTop2"; graph] ->
          let svg_file = if grew_window#btn_gr_top_dot#active
            then (Grew_rew_display.get_dot_graph_with_background
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) graph)
            else (Grew_rew_display.get_dep_graph_with_background ~filter:(get_current_filter ())
                    ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) graph) in
          graph_top_webkit#load_uri ("file://"^svg_file);
          Grew_rew_display.current_top_graph := graph;
          true
        | ["showModuleFromGraph"; graph] ->
          if (grew_window#vpane_right#position <= 30)
          then (grew_window#vpane_right#set_position 250);
          let svg_file = Grew_rew_display.get_big_step_for graph in
          module_webkit#load_uri ("file://"^svg_file);
          grs_webkit#execute_script "set_edge_flag(true)";
          grew_window#btn_show_module#set_active true;
          true
        | ["removeTop"] ->
          graph_top_webkit#load_html_string empty_html "";
          true
        | ["removeBottom"] ->
          graph_bottom_webkit#load_html_string empty_html "";
          true
        | _ -> false
    ) in

  let click_marker1 = ref false
  and click_marker2 = ref false in

  let _ = module_webkit#connect#script_alert
    ~callback:(fun _ msg ->
      match Str.split (Str.regexp "::") msg with
        | ["showOnBottom"; graph]
        | ["showOnBottom2"; graph] ->
          if !click_marker1
          then click_marker1 := false
          else
            begin
              let svg_file =
                if grew_window#btn_gr_bottom_dot#active
                then (Grew_rew_display.get_dot_graph_with_background2
                        ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) (graph^".2"))
                else (Grew_rew_display.get_dep_graph_with_background2 ~filter:(get_current_filter ())
                        ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(true,false) (graph^".2")) in
              graph_bottom_webkit#load_uri ("file://"^svg_file);
              Grew_rew_display.current_bottom_graph := (graph^".2");
              grs_webkit#execute_script "remove_back_from_current_bottom()";
            end;
          true
        | ["showOnTop"; graph]
        | ["showOnTop2"; graph] ->
          if !click_marker2
          then click_marker2 := false
          else
            begin
              let svg_file =
                if grew_window#btn_gr_top_dot#active
                then (Grew_rew_display.get_dot_graph_with_background2
                        ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) (graph^".2"))
                else (Grew_rew_display.get_dep_graph_with_background2  ~filter:(get_current_filter ())
                        ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) ~botop:(false,true) (graph^".2")) in
              graph_top_webkit#load_uri ("file://"^svg_file);
              Grew_rew_display.current_top_graph := (graph^".2");
              grs_webkit#execute_script "remove_back_from_current_top()";
            end;
          true
        | ["showModuleFromGraph"; graph] ->
          let top_dot = grew_window#btn_gr_top_dot#active
          and bottom_dot = grew_window#btn_gr_bottom_dot#active in
          let (svg_file_top,svg_file_bottom,graph_top,doc_file) =
            Grew_rew_display.get_rule_for
              ~main_feat:(!Grew_config.current_config.Grew_config.main_feat) top_dot bottom_dot (graph^".2") in

          graph_top_webkit#load_uri ("file://"^svg_file_top);
          graph_bottom_webkit#load_uri ("file://"^svg_file_bottom);

          (match !doc_dir with
            | Some dir -> doc_webkit#load_uri ("file://"^(Filename.concat dir doc_file))
            | None -> doc_webkit#load_html_string empty_html "");

          Grew_rew_display.current_top_graph := (graph_top);
          Grew_rew_display.current_bottom_graph := (graph^".2");
          click_marker1 := true;
          click_marker2 := true;
          true
        | _ -> false
    ) in

  (* -------------------------------------------------------------------------------- *)
  (** SWAP BETWEEN DEP AND DOT *)
  let _ =  grew_window#btn_gr_bottom_dep#connect#clicked
    ~callback:
    (fun () ->
      grew_window#btn_gr_bottom_dot#set_active (not grew_window#btn_gr_bottom_dep#active);
      if (grew_window#btn_gr_bottom_dep#active) && (!Grew_rew_display.current_bottom_graph <> "")
      then
        begin
          Log.fdebug "[Grew_gtk] Try to display dep for '%s'" !Grew_rew_display.current_bottom_graph;
          !Grew_config.current_config.Grew_config.last_is_dep_bottom_graph <- true;
          let svg_file =
            try Grew_rew_display.get_dep_graph_with_background
                  ~filter:(get_current_filter ())
                  ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
                  ~botop:(true,false)
                  !Grew_rew_display.current_bottom_graph
            with Not_found ->
              Grew_rew_display.get_dep_graph_with_background2
                ~filter:(get_current_filter ())
                ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
                ~botop:(true,false) !Grew_rew_display.current_bottom_graph in
          graph_bottom_webkit#load_uri ("file://"^svg_file);
        end
    ) in

  let _ = grew_window#btn_gr_top_dep#connect#clicked
    ~callback:
    (fun () ->
      grew_window#btn_gr_top_dot#set_active (not grew_window#btn_gr_top_dep#active);
      if (grew_window#btn_gr_top_dep#active) && (!Grew_rew_display.current_top_graph <> "")
      then
        begin
          Log.fdebug "[Grew_gtk] Try to display dep for '%s'" !Grew_rew_display.current_top_graph;
          !Grew_config.current_config.Grew_config.last_is_dep_top_graph <- true;
          let svg_file =
            try Grew_rew_display.get_dep_graph_with_background
                  ~filter:(get_current_filter ())
                  ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
                  ~botop:(false,true) !Grew_rew_display.current_top_graph
            with Not_found -> Grew_rew_display.get_dep_graph_with_background2
              ~filter:(get_current_filter ())
              ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
              ~botop:(false,true) !Grew_rew_display.current_top_graph in
          graph_top_webkit#load_uri ("file://"^svg_file);
        end
    ) in

  let _ = grew_window#btn_gr_bottom_dot#connect#clicked
    ~callback:
    (fun () ->
      grew_window#btn_gr_bottom_dep#set_active (not grew_window#btn_gr_bottom_dot#active);
      if (grew_window#btn_gr_bottom_dot#active) && (!Grew_rew_display.current_bottom_graph <> "")
      then
        begin
          Log.fdebug "[Grew_gtk] Try to display dot for '%s'" !Grew_rew_display.current_bottom_graph;
          !Grew_config.current_config.Grew_config.last_is_dep_bottom_graph <- false;
          let svg_file =
            try Grew_rew_display.get_dot_graph_with_background
                  ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
                  ~botop:(true,false) !Grew_rew_display.current_bottom_graph
            with Not_found -> Grew_rew_display.get_dot_graph_with_background2
              ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
              ~botop:(true,false) !Grew_rew_display.current_bottom_graph in
          graph_bottom_webkit#load_uri ("file://"^svg_file);
        end
    ) in

  (* XXX *)
  let _ = grew_window#btn_gr_top_dot#connect#clicked
    ~callback:
    (fun () ->
      grew_window#btn_gr_top_dep#set_active (not grew_window#btn_gr_top_dot#active);
      if (grew_window#btn_gr_top_dot#active) && (!Grew_rew_display.current_top_graph <> "")
      then
        begin
          Log.fdebug "[Grew_gtk] Try to display dot for '%s'" !Grew_rew_display.current_top_graph;
          !Grew_config.current_config.Grew_config.last_is_dep_top_graph <- false;
          let svg_file =
            try Grew_rew_display.get_dot_graph_with_background
                  ?deco:!Grew_rew_display.current_top_deco
                  ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
                  ~botop:(false,true) !Grew_rew_display.current_top_graph
            with Not_found -> Grew_rew_display.get_dot_graph_with_background2
              ~main_feat:(!Grew_config.current_config.Grew_config.main_feat)
              ~botop:(false,true) !Grew_rew_display.current_top_graph in
          graph_top_webkit#load_uri ("file://"^svg_file);
        end
    ) in


  (* -------------------------------------------------------------------------------- *)
  (** ZOOMS *)
  let _ = grew_window#grs_zoom#connect#value_changed
    ~callback:
    (fun () ->
      grs_webkit#set_zoom_level (grew_window#grs_zoom#adjustment#value /. 100.);
      !Grew_config.current_config.Grew_config.last_rewriting_history_zoom <- int_of_float grew_window#grs_zoom#adjustment#value;
    ) in

  let _ = grew_window#doc_zoom#connect#value_changed
    ~callback:
    (fun () ->
      doc_webkit#set_zoom_level (grew_window#doc_zoom#adjustment#value /. 100.);
      !Grew_config.current_config.Grew_config.last_doc_zoom <- int_of_float grew_window#doc_zoom#adjustment#value;
    ) in

  let _ = grew_window#graph_top_zoom#connect#value_changed
    ~callback:
    (fun () ->
      graph_top_webkit#set_zoom_level (grew_window#graph_top_zoom#adjustment#value /. 100.);
      !Grew_config.current_config.Grew_config.last_top_graph_zoom <- int_of_float grew_window#graph_top_zoom#adjustment#value;
    ) in

  let _ = grew_window#graph_bottom_zoom#connect#value_changed
    ~callback:
    (fun () ->
      graph_bottom_webkit#set_zoom_level (grew_window#graph_bottom_zoom#adjustment#value /. 100.);
      !Grew_config.current_config.Grew_config.last_bottom_graph_zoom <- int_of_float grew_window#graph_bottom_zoom#adjustment#value;
    ) in

  let _ = grew_window#module_zoom#connect#value_changed
    ~callback:
    (fun () ->
      module_webkit#set_zoom_level (grew_window#module_zoom#adjustment#value /. 100.);
      !Grew_config.current_config.Grew_config.last_rule_zoom <- int_of_float grew_window#module_zoom#adjustment#value;
    ) in

  (* -------------------------------------------------------------------------------- *)
  (** FULLSCREEN *)
  let fullscreen = ref false in
  let _ = grew_window#btn_enter_fullscreen#connect#clicked
    ~callback: (fun () -> grew_window#toplevel#fullscreen (); fullscreen := true) in

  let _ = grew_window#btn_leave_fullscreen#connect#clicked
    ~callback: (fun () -> grew_window#toplevel#unfullscreen (); fullscreen := false) in

(*  *)
  (* ==================== Grs file choosing ==================== *)
  (* 1: the file given with the "-grs" option, if it is a valid path *)
  (* 2: the last grs file used with grew, if it is a valid path *)
  (* 3: else, message and no file chosen *)
  if !Grew_args.grs <> "" && Sys.file_exists !Grew_args.grs
  then Resources.current_grs_file := Some !Grew_args.grs
  else
    if !Grew_config.current_config.Grew_config.last_grs_file <> "" && Sys.file_exists !Grew_config.current_config.Grew_config.last_grs_file
    then Resources.current_grs_file := Some !Grew_config.current_config.Grew_config.last_grs_file
    else ignore (warning_dialog ~message: (sprintf "==> No grs loaded:\nno grs file on command line, no grs file in config") ());

  (* ==================== Gr file choosing ==================== *)
  (* 1: the file given with the "-gr" option, if it is a valid path *)
  (* 2: the last gr file used with grew, if it is a valid path *)
  (* 3: else, message and no file chosen *)
  if !Grew_args.gr <> "" && Sys.file_exists !Grew_args.gr
  then Resources.current_gr_file := Some !Grew_args.gr
  else
    if !Grew_config.current_config.Grew_config.last_gr_file <> "" && Sys.file_exists !Grew_config.current_config.Grew_config.last_gr_file
    then Resources.current_gr_file := Some !Grew_config.current_config.Grew_config.last_gr_file
    else ignore (warning_dialog ~message: (sprintf "==> No gr loaded:\nno gr file on command line, no gr file in config") ());

  (* ==================== (un)set dot/dep buttons ==================== *)
  grew_window#btn_gr_bottom_dep#set_active !Grew_config.current_config.Grew_config.last_is_dep_bottom_graph;
  grew_window#btn_gr_bottom_dot#set_active (not !Grew_config.current_config.Grew_config.last_is_dep_bottom_graph);
  grew_window#btn_gr_top_dep#set_active !Grew_config.current_config.Grew_config.last_is_dep_top_graph;
  grew_window#btn_gr_top_dot#set_active (not !Grew_config.current_config.Grew_config.last_is_dep_top_graph);

  (* cpt is used to avoid a loop between the two propagations of value_changed *)
  let cpt = ref 0 in
  let _ = GMain.Timeout.add ~ms:50 ~callback:(fun () -> cpt := 0; true) in

  let _ = grew_window#graph_view_bottom#hadjustment#connect#value_changed
    ~callback:
    (fun () ->
      if (!cpt<1 && grew_window#synchronize#active)
      then
        begin
          let percent = ((grew_window#graph_view_bottom#hadjustment#value +. (grew_window#graph_view_bottom#hadjustment#page_size /. 2.)) /. grew_window#graph_view_bottom#hadjustment#upper) in
          let value = percent *. grew_window#graph_view_top#hadjustment#upper -. (grew_window#graph_view_top#hadjustment#page_size /. 2.) in
          incr cpt;
          grew_window#graph_view_top#hadjustment#set_value value;
        end
    ) in

  let _ = grew_window#graph_view_top#hadjustment#connect#value_changed
    ~callback:
    (fun () ->
      if (!cpt<1 && grew_window#synchronize#active)
      then
        (let percent = ((grew_window#graph_view_top#hadjustment#value +. (grew_window#graph_view_top#hadjustment#page_size /. 2.)) /. grew_window#graph_view_top#hadjustment#upper) in
         let value = percent *. grew_window#graph_view_bottom#hadjustment#upper -. (grew_window#graph_view_bottom#hadjustment#page_size /. 2.) in
         incr cpt;
         grew_window#graph_view_bottom#hadjustment#set_value value;
        )
    ) in


  (* ==================== Contextual menu to export ==================== *)

  let save extension graph save_function () =
    let filter = GFile.filter ~name:("*."^extension) ~patterns:["*."^extension] () in
    match ask_for_file_to_save filter grew_window#toplevel with
      | None -> ()
      | Some filename -> save_function graph filename in

  (* -------------------------------------------------------------------------------- *)
  let view fct graph () =
    let text = fct graph in
    let sv = new src_viewer () in
    sv#source#buffer#set_text text;
    ignore(sv#toplevel#connect#destroy ~callback:sv#toplevel#destroy);
    ignore(sv#close#connect#clicked ~callback:sv#toplevel#destroy);
    sv#check_widgets ();
    sv#toplevel#show ();
    () in

  let contextual_menu side ev =
    if GdkEvent.Button.button ev <> 3
    then false (* we did not handle this *)
    else
      let graph = match side with
        | Top -> !Grew_rew_display.current_top_graph
        | Bottom -> !Grew_rew_display.current_bottom_graph in
      if graph = ""
      then true
      else begin
        let dot =
          match side with
          | Top -> grew_window#btn_gr_top_dot#active
          | Bottom -> grew_window#btn_gr_bottom_dot#active in
        let deco =
          match side with
          | Top -> !Grew_rew_display.current_top_deco
          | Bottom -> !Grew_rew_display.current_bottom_deco in
        let main_feat = !Grew_config.current_config.Grew_config.main_feat in

        (* create the contextual menu *)
        let menu = GMenu.menu () in
        let add_item (label,callback) =
          let menuitem = GMenu.menu_item ~label ~packing:menu#append () in
          ignore (menuitem#connect#activate ~callback) in
        (* build save items and put them in the menu *)
        let save_items =
          ("Save as gr", save "gr" graph Grew_rew_display.to_grfile_graph) ::
          ("Save as conll", save "conll" graph Grew_rew_display.save_conll_graph) ::
          ("Save as pdf",
            if dot
            then save "pdf" graph (Grew_rew_display.to_pdf_dotfile_graph ?deco ~main_feat)
            else save "pdf" graph (Grew_rew_display.to_pdf_depfile_graph ?deco ~main_feat)
          ) ::
          ("Save as svg",
            if dot
            then save "svg" graph (Grew_rew_display.to_svg_dotfile_graph ?deco ~main_feat)
            else save "svg" graph (Grew_rew_display.to_svg_depfile_graph ?deco ~main_feat)
          ) ::
          ("Save as png", save "png" graph (Grew_rew_display.to_pngfile_graph ?deco ~main_feat)) ::
          (if dot
           then ["Save as dot", save "dot" graph (Grew_rew_display.to_dotfile_graph ?deco ~main_feat)]
           else ["Save as dep", save "dep" graph (Grew_rew_display.to_depfile_graph ?deco ~main_feat)]
          ) in
        List.iter add_item save_items;
        let _ = GMenu.separator_item ~packing:menu#append () in

        (* build view items and put them in the menu *)
        let view_items =
          ("View gr", view Grew_rew_display.to_grstring_graph graph) ::
          ("View conll", view Grew_rew_display.to_conll_graph graph) ::
          (if dot
           then ["View dot", view (Grew_rew_display.to_dotstring_graph ?deco ~main_feat) graph]
           else ["View dep", view (Grew_rew_display.to_depstring_graph ?deco ~main_feat) graph]
          ) in
        List.iter add_item view_items;

        menu#popup ~button:(GdkEvent.Button.button ev) ~time:(GdkEvent.Button.time ev);
      true (* we handled this *)
    end in

  let _ = top_event_box#event#connect#button_press ~callback: (contextual_menu Top) in
  let _ = bottom_event_box#event#connect#button_press ~callback: (contextual_menu Bottom) in




  (* At exit, save the last grs/gr files used for next grew usage *)
  at_exit
    (fun () ->
      (match !Resources.current_grs_file with
        | Some file -> !Grew_config.current_config.Grew_config.last_grs_file <- file
        | None -> ()
      );
      (match  !Resources.current_gr_file with
        | Some file -> !Grew_config.current_config.Grew_config.last_gr_file <- file
        | None -> ()
      );
      Grew_config.save_config ()
    );

  (* startup load of grs files (which implies loading of the gr file) *)
  error_handling load_grs ();

  (* force doc building in required on the commande line *)
  if !Grew_args.gui_doc then build_doc ();

  (* Really start the gui *)
  grew_window#check_widgets ();
  grew_window#toplevel#show ();
  GMain.Main.main ()
