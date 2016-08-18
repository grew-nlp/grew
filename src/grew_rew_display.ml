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
open Libgrew

open Grew_utils


let fos str =
  try float_of_string str
  with Failure _ ->
    Pervasives.float_of_string (Str.global_replace (Str.regexp "\\.") "," str)

let sof fl = Str.global_replace (Str.regexp ",") "." (Pervasives.string_of_float fl)

let save output_file string =
  let out_ch = open_out output_file in
  Printf.fprintf out_ch "%s" string;
  close_out out_ch

module Grew_rew_display = struct

  let graph_map = ref []
  let graph_map2 = ref []

  let current_bottom_graph = ref ""
  let current_top_graph = ref ""

  let current_bottom_deco = ref None
  let current_top_deco = ref None

  let top_color = "#35FF5C"
  let bottom_color = "#FF3E47"
  let middle_color = "#FFFF33"

  let top_color_light = "#EAFFEE"
  let bottom_color_light = "#FFEAEB"
  let middle_color_light = "#FFFFD2"

  let get_graph graph_id =
    try fst (List.assoc graph_id !graph_map)
    with Not_found -> fst (List.assoc graph_id !graph_map2)

  let to_dotstring_graph ?domain ?deco ?main_feat graph_id =
    Graph.to_dot ?domain ?deco ?main_feat (get_graph graph_id)

  let to_grstring_graph ?domain graph_id =
    Graph.to_gr ?domain (get_graph graph_id)

  let to_grfile_graph ?domain graph_id output_file =
    save output_file (to_grstring_graph ?domain graph_id)

  let to_conll_graph ?domain graph_id =
    Graph.to_conll_string ?domain (get_graph graph_id)

  let save_conll_graph ?domain graph_id output_file =
    save output_file (to_conll_graph ?domain graph_id)

  let to_depstring_graph ?domain ?deco ?main_feat graph_id =
    Graph.to_dep ?domain ?deco ?main_feat (get_graph graph_id)

  let to_dotfile_graph ?domain ?deco ?main_feat graph_id output_file =
    save output_file (to_dotstring_graph ?domain ?deco ?main_feat graph_id)

  let to_depfile_graph ?domain ?deco ?main_feat graph_id output_file =
    save output_file (to_depstring_graph ?domain ?deco ?main_feat graph_id)

  let to_pdf_dotfile_graph ?domain ?deco ?main_feat graph_id output_file =
    let dot = to_dotstring_graph ?domain ?deco ?main_feat graph_id in
    Pdf.dot_to_file dot output_file


  let to_pngfile_graph ?domain ?deco ?main_feat graph_id output_file =
    let dep = to_depstring_graph ?domain ?deco ?main_feat graph_id in
    let d2p = Dep2pict.Dep2pict.from_dep ~dep in
    Dep2pict.Dep2pict.save_png ~filename:output_file d2p

  let to_pdf_depfile_graph ?domain ?deco ?main_feat graph_id output_file =
    let dep = to_depstring_graph ?domain ?deco ?main_feat graph_id in
    let d2p = Dep2pict.Dep2pict.from_dep ~dep in
    Dep2pict.Dep2pict.save_pdf ~filename:output_file d2p

  let to_svg_depfile_graph ?domain ?deco ?main_feat graph_id output_file =
    let dep = to_depstring_graph ?domain ?deco ?main_feat graph_id in
    let d2p = Dep2pict.Dep2pict.from_dep ~dep in
    Dep2pict.Dep2pict.save_svg ~filename:output_file d2p

  let to_svg_dotfile_graph ?domain ?deco ?main_feat graph_id output_file =
    let dot = to_dotstring_graph ?domain ?deco ?main_feat graph_id in
    Svg.dot_to_file dot output_file

  (* create 2 temp file: 1 svg (via Svg.dot_to_tmp) and 1 html *)
  let svg_dot_temp_file ?domain ?main_feat ?deco ?(botop=(false,false)) graph =
    let dot = Graph.to_dot ?domain ?deco ?main_feat graph in
    let dot = Str.replace_first (Str.regexp "digraph G {") ("digraph G {\n    bgcolor=\"transparent\";\n") dot in
    let svg_file = Svg.dot_to_tmp dot in
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".html" in
    let color = if (fst botop && snd botop) then middle_color_light else if (snd botop) then top_color_light else if (fst botop) then bottom_color_light else "white" in
    Printf.fprintf out_ch "<body style=\"background:%s;\"><img src=\"%s\"/></body>" color svg_file;
    close_out out_ch;
    temp_file_name


  let get_dot_graph_with_background ?domain ?main_feat ?deco ?(botop=(false,false)) graph_id =
    svg_dot_temp_file ?domain ?main_feat ?deco ~botop (fst (List.assoc graph_id !graph_map))


  let get_dot_graph_with_background2 ?domain ?main_feat ?deco ?(botop=(false,false)) graph_id =
    svg_dot_temp_file ?domain ?main_feat ?deco ~botop (fst (List.assoc graph_id !graph_map2))

  (* create 2 temp file: 1 svg (via Svg.dep_to_tmp) and 1 html *)
  let svg_dep_temp_file ?domain ?filter ?main_feat ?deco ?(botop=(false,false)) graph =
    let dep = Graph.to_dep ?domain ?filter ?deco ?main_feat graph in
    let svg_file = Svg.dep_to_tmp dep in
    let temp_file_name,out_ch = Filename.open_temp_file ~mode:[Open_rdonly;Open_wronly;Open_text] "grew_" ".html" in
    let color =
      if (fst botop && snd botop)
      then middle_color_light
      else
        if (snd botop)
        then top_color_light
        else
          if (fst botop)
          then bottom_color_light
          else "white" in
    Printf.fprintf out_ch "<body style=\"background:%s;\"><img src=\"%s\"/></body>" color svg_file;
    close_out out_ch;
    temp_file_name

  let get_dep_graph_with_background ?domain ?(filter=None) ?main_feat ?(botop=(false,false)) graph_id =
    svg_dep_temp_file ?domain ?filter ?main_feat ~botop (fst (List.assoc graph_id !graph_map))

  let get_dep_graph_with_background2 ?domain ?(filter=None) ?main_feat ?deco ?(botop=(false,false)) graph_id =
    svg_dep_temp_file ?domain ?filter ?main_feat ?deco ~botop (fst (List.assoc graph_id !graph_map2))

  type leaf = No | Leaf of string | Dead_lock of string

  let rew_display_to_svg rew_display =
    graph_map := [];
    let buff = Buffer.create 32 in
    let add str = Printf.bprintf buff "%s\n" str in
    let kadd arg = Printf.ksprintf add arg in
    add "digraph G {";
    add "    node [fontname=Arial];";
    add "    ranksep=0.01;";
    add "    nodesep=0.04";
    (*		add "    edge [color=transparent]";*)
    let module_counter = ref 0 in
    let graph_counter = ref 0 in
    let previous_mod = ref "" in
    let modules = ref [] in
    let first_leaf = ref No in
    let rec transform parent pt_is_nf ?(bs=None) module_name rew_display level =
      let tmp_id = !graph_counter in
      let gid = Printf.sprintf "G%d" !graph_counter in
      let pid = Printf.sprintf "G%s" parent in

      let common_part n g =
        begin
          try
            let modu = List.assoc (n^"_"^(string_of_int level)) !modules in
	    kadd "    {rank=same; %s; node_mod_%d_0}" gid (modu-1);
	    kadd "    node_mod_%d_0 -> %s [color=transparent]" (modu-1) gid;
	  with Not_found ->
	    kadd "    node_mod_%d_0 [label=\"\", fontcolor=transparent, color=transparent]" !module_counter;
	    kadd "    node_mod_%d [label=\"%s\"%s]" !module_counter n (if n = "##end##" then ", fontcolor=transparent, color=transparent" else "");
	    kadd "    {rank=same; %s; node_mod_%d_0}" gid !module_counter;
	    kadd "    node_mod_%d_0 -> %s [color=transparent]" !module_counter gid;
	    kadd "    node_mod_%d_0 -> node_mod_%d [color=transparent]" !module_counter !module_counter;
	    if !previous_mod <> ""
            then kadd "    %s -> node_mod_%d_0 [color=transparent]" !previous_mod !module_counter;
	    previous_mod := Printf.sprintf "node_mod_%d" !module_counter;
	    incr module_counter;
	    modules := ((Printf.sprintf "%s_%d" n level), !module_counter) :: !modules;
	end;
      	graph_map := (gid, (g, (pid, module_name, bs))) :: !graph_map;
      in

      let connect_up () =
        if parent <> "" then kadd "    %s -> %s%s" pid gid (if pt_is_nf then " [style=dotted]" else "") in

      let step () =
	if !graph_counter = 0
        then kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"%s\"]" gid gid top_color
        else kadd "    %s [shape=box, label=\"%s\"]" gid gid in

      begin
	match rew_display with
	  | Libgrew_types.Empty -> ()
	  | Libgrew_types.Leaf g ->
	    common_part "##end##" g;
            incr graph_counter;
            (match !first_leaf with
              | Leaf _ ->
                kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"white\", peripheries=2]" gid gid
              | Dead_lock gid' ->
                first_leaf := Leaf gid;
                kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"%s\", peripheries=2]" gid gid bottom_color;
                (* next line is a hack: hide the previous def of node gid' in the dot file *)
                kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"white\"]" gid' gid'
              | No ->
                first_leaf := Leaf gid;
                kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"%s\", peripheries=2]" gid gid bottom_color
	    );
            connect_up ();

	  | Libgrew_types.Local_normal_form (g,n,rd) ->
            common_part n g;
	    let previous_node = !graph_counter in
            step ();

	    incr graph_counter;
            connect_up ();
	    ignore(transform (string_of_int previous_node) true n rd (level+1));
	    ()
	  | Libgrew_types.Node (g,n,[]) ->
            common_part n g;
            connect_up ();

            (match (!graph_counter, !first_leaf) with
              | (0,_) -> kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"%s\"]" gid gid top_color
              | (_,No) -> first_leaf := Dead_lock gid; kadd "    %s [shape=box, label=\"%s\", style=filled, fillcolor=\"%s\"]" gid gid bottom_color
              | _ -> kadd "    %s [shape=box, label=\"%s\"  ]" gid gid);
	    incr graph_counter


	  | Libgrew_types.Node (g,n,children) ->
            common_part n g;

	    let previous_node = !graph_counter in
            step ();

	    incr graph_counter;
            connect_up ();

	    let tmp_list = ref [] in
	    List.iter (fun (bs,rd) ->
	      let id = transform (string_of_int previous_node) false ~bs:(Some bs) n rd (level+1) in
	      tmp_list := id::(!tmp_list)
	    ) children;

	    kadd "{rank=same; %s; %s}"
              (String.concat "; " (List.map (fun id -> Printf.sprintf "G%d" id) !tmp_list))
	      (try
                 let mod_id = List.assoc (n^"_"^(string_of_int level)) !modules in
	         (Printf.sprintf "node_mod_%d_0" mod_id)
	       with Not_found -> ""
              )
      end;
      tmp_id
    in
    ignore(transform "" false "" rew_display 0);
    add "}";
    (* Printf.printf "==================================\n%s\n==============================\n" (Buffer.contents buff); *)
    (
      (match !first_leaf with No -> "" | Leaf x -> x | Dead_lock x -> x),
      Svg.dot_to_tmp (Buffer.contents buff)
    )

  let init = Str.regexp "<g id=\"\\(.*\\)\" class=\"\\(.*\\)\" transform=\"\\(.*\\)\">"

  let graph_comment_match = Str.regexp "<!-- \\(G[0-9]+\\([0-9]+\\)?\\) -->"
  let g_match = Str.regexp "<g id=\"\\(.*\\)\" class=\"node\"><title>.*</title>"
  let polygon_matchA = Str.regexp "<polygon fill=\"\\(.*\\)\" stroke=\"black\" stroke-dasharray=\".*\" points=\"\\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\)\"/>"
  let polygon_matchB = Str.regexp "<polygon fill=\"\\(.*\\)\" stroke=\"black\" points=\"\\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\) \\(.*\\),\\(.*\\)\"/>"

  let edge_comment_match = Str.regexp "<!-- \\(G[0-9]+\\(_[0-9]+\\)?\\)&#45;&gt;\\(G[0-9]+\\(_[0-9]+\\)?\\) -->"
  let edge_path_match = Str.regexp "<path fill=\"none\" stroke=\"\\(.*\\)\" d=\"M\\(.*\\),\\(.*\\)C.*,.* .*,.* .*,.*\"/>"
  let edge_polygon_match = Str.regexp "<polygon fill=\".*\" stroke=\".*\" points=\".*,.* \\(.*\\),\\(.*\\) .*,.* .*,.*\"/>"

  let svg_match = Str.regexp "<svg .*"

  let module_match = Str.regexp "<g id=\"\\(.*\\)\" class=\"node\"><title>node_mod_\\([^_]*\\)</title>"
  let module_text_match = Str.regexp "<text text-anchor=\"middle\" x=\"\\(.*\\)\" y=\"\\(.*\\)\" font-family=\"\\(.*\\)\" font-size=\"\\(.*\\)\">\\(.*\\)</text>"

  let background_match = Str.regexp "<polygon fill=\"white\" stroke=\"white\" points=\"\\(.*\\),.* .*,.* \\(.*\\),.* .*,.* .*,.*\"/>"

  let transform ?(show_bottom=false) file file_out fl =
    let graph_id_internal = ref "" in
    let edge_counter = ref 0 in
    let graph_id = ref "" in
    let polygon_tmp = ref "" in
    let polygon_back_tmp = ref "" in
    let graph_line_counter = ref 0 in
    let edge_color = ref "" in
    let x1 = ref "" in
    let x2 = ref "" in
    let y1 = ref "" in
    let y2 = ref "" in

    let x_left = ref "" and x_right = ref "" in

    let edge_from = ref "" in
    let edge_to = ref "" in
    let in_ch = open_in file in
    let out_ch = open_out file_out in
    try
      while true do
	let line = input_line in_ch in

	match !graph_line_counter with
	  | 0 -> (* matching de la premiere ligne pour un graphe (<!-- GXX -->) *)
	    if (Str.string_match init line 0)
            then
              begin
	        let id = Str.matched_group 1 line in
	        let classe = Str.matched_group 2 line
	        and transform = Str.matched_group 3 line in
	        if (show_bottom) then (
		  output_string out_ch ("<g id=\""^id^"\" onload=\"window.scrollBy(0,100000);\" class=\""^classe^"\" transform=\""^transform^"\">");
	        ) else (
		  output_string out_ch line
	        )
	      end
            else if (Str.string_match graph_comment_match line 0)
            then
              begin
	        graph_id_internal := Str.matched_group 1 line;
	        incr graph_line_counter;
	        output_string out_ch (line^"\n");
	      (* Log.fdebug "[Grew_rew_display]--> Graph found : %s" !graph_id_internal *)
	      end
            else if (Str.string_match edge_comment_match line 0)
            then
              begin
		(* Log.fdebug "[Grew_rew_display]--> edge found : %s -> %s" (Str.matched_group 1 line) (Str.matched_group 3 line); *)
		edge_from := (Str.matched_group 1 line);
	        edge_to := (Str.matched_group 3 line);
	        graph_line_counter := 4;
	        output_string out_ch (line^"\n");
	      end
            else if (Str.string_match svg_match line 0)
            then
              begin
	        graph_line_counter := 99;
	        output_string out_ch (line^"\n");
	      end
            else if (Str.string_match module_match line 0)
            then
              begin
	        graph_line_counter := 59;
	        output_string out_ch (line^"\n");
	      end
            else if (Str.string_match background_match line 0)
            then
              begin
	        let _1 = Str.matched_group 1 line
	        and _2 = Str.matched_group 2 line in
	        x_left := _1;
	        x_right := _2;
	        output_string out_ch (line^"\n");
	      end
            else output_string out_ch (line^"\n")
	  | 1 -> (* deuxieme ligne du graphe (g) *)
	    if (Str.string_match g_match line 0)
            then (
	      graph_id := Str.matched_group 1 line;
	      incr graph_line_counter;
	      output_string out_ch (Printf.sprintf "<g id=\"%s\" class=\"node\" >\n" !graph_id);
	    (* Log.fdebug "[Grew_rew_display]    Graph id : %s" !graph_id *)
	    ) else (
	      output_string out_ch (line^"\n");
	    )
	  | 2 ->
	    if (Str.string_match polygon_matchA line 0) || (Str.string_match polygon_matchB line 0) then (
	      incr graph_line_counter;
	      let fill = Str.matched_group 1 line in
	      let _1 = Str.matched_group 2 line
	      and _2 = Str.matched_group 3 line
	      and _3 = Str.matched_group 4 line
	      and _4 = Str.matched_group 5 line
	      and _5 = Str.matched_group 6 line
	      and _6 = Str.matched_group 7 line
	      and _7 = Str.matched_group 8 line
	      and _8 = Str.matched_group 9 line
	      and _9 = Str.matched_group 10 line
	      and _10 = Str.matched_group 11 line in
	      polygon_back_tmp := Printf.sprintf "<polygon id=\"polygon_%s_back\"
		  fill=\"%s\" fill-opacity=\"%d\" stroke=\"transparent\" points=\"%s,%s %s,%s %s,%s %s,%s %s,%s\"/>\n"
		!graph_id_internal
		fill
		(if (fill<>"none") then 1 else 0)
		_1 _2 _3 _4 _5 _6 _7 _8 _9 _10;
	      polygon_tmp := Printf.sprintf "<polygon id=\"polygon_%s\"
		  onmouseover=\"this.setAttribute('stroke','orange');document.getElementById('polygon_%s_top').style.display='block';document.getElementById('polygon_%s_bottom').style.display='block';\"
		  onmouseout=\"this.setAttribute('stroke','black');document.getElementById('polygon_%s_top').style.display='none';document.getElementById('polygon_%s_bottom').style.display='none';show_current_edge()\"
		  fill=\"white\" fill-opacity=\"0\" stroke=\"black\" stroke-width=\"2\" points=\"%s,%s %s,%s %s,%s %s,%s %s,%s\"/>\n"
		!graph_id_internal !graph_id_internal !graph_id_internal !graph_id_internal !graph_id_internal
		_1 _2 _3 _4 _5 _6 _7 _8 _9 _10;
	      polygon_tmp := Printf.sprintf "%s<polygon
		  onmouseover=\"this.style.display='block';document.getElementById('polygon_%s').setAttribute('stroke','orange');\"
		  onmouseout=\"this.style.display='none';document.getElementById('polygon_%s').setAttribute('stroke','black');show_current_edge()\"
		  onclick=\"alert('showOnTop::%s');remove_back_from_current_top();current_top='%s';add_back_from_current_top();\"
		  id=\"polygon_%s_top\" style=\"cursor:ne-resize;display:none\" fill=\"grey\" stroke=\"black\" points=\"%s,%s %s,%s %s,%s\"/>\n"
		!polygon_tmp
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		(sof ((fos _1) -. 2.) ^ "0")
		(sof ((fos _2) +. 7.) ^ "0")
		(sof ((fos _1) -. 2.) ^ "0")
		(sof ((fos _2) +. 2.) ^ "0")
		(sof ((fos _1) -. 7.) ^ "0")
		(sof ((fos _2) +. 2.) ^ "0");
	      polygon_tmp := Printf.sprintf "%s<polygon
		  onmouseover=\"this.style.display='block';document.getElementById('polygon_%s').setAttribute('stroke','orange');\"
		  onmouseout=\"this.style.display='none';document.getElementById('polygon_%s').setAttribute('stroke','black');show_current_edge()\"
		  onclick=\"alert('showOnBottom::%s');remove_back_from_current_bottom();current_bottom='%s';add_back_from_current_bottom();\"
		  id=\"polygon_%s_bottom\" style=\"cursor:se-resize;display:none\" fill=\"grey\" stroke=\"black\" points=\"%s,%s %s,%s %s,%s\"/>\n"
		!polygon_tmp
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		!graph_id_internal
		(sof ((fos _7) -. 2.) ^ "0")
		(sof ((fos _8) -. 7.) ^ "0")
		(sof ((fos _7) -. 2.) ^ "0")
		(sof ((fos _8) -. 2.) ^ "0")
		(sof ((fos _7) -. 7.) ^ "0")
		(sof ((fos _8) -. 2.) ^ "0");
	    (* Log.fdebug "[Grew_rew_display]    Polygon found : %s"  _1 *)
	    ) else (
	      output_string out_ch (line^"\n");
	    )
	  | 3 ->
	    output_string out_ch !polygon_back_tmp;
	    output_string out_ch (line^"\n");
	    output_string out_ch !polygon_tmp;
	    graph_line_counter := 0;
	  | 4 ->  (* recuperation des coordonnées utiles pour les aretes *)
	    if (Str.string_match edge_path_match line 0) then (
	      if (Str.matched_group 1 line) = "black" then (
		incr graph_line_counter;
		(* Log.fdebug "[Grew_rew_display]    color: %s, x1:%s, y1:%s" (Str.matched_group 1 line) (Str.matched_group 2 line) (Str.matched_group 3 line); *)
		edge_color := (Str.matched_group 1 line);
		x1 := (Str.matched_group 2 line);
		y1 := (Str.matched_group 3 line);
	      ) else (
		graph_line_counter := 0;
	      )
	    );
	    output_string out_ch (line^"\n");
	  | 5 ->
	    if (Str.string_match edge_polygon_match line 0) then (
	      x2 := (Str.matched_group 1 line);
	      y2 := (Str.matched_group 2 line);

	      output_string out_ch (line^"\n");
	      output_string out_ch (Printf.sprintf "<polygon id=\"edge_%d\" style=\"cursor:pointer;\"
				      onmouseover=\"this.setAttribute('stroke-opacity','1');this.setAttribute('stroke-width','2');document.getElementById('polygon_%s').setAttribute('stroke','orange');document.getElementById('polygon_%s').setAttribute('stroke','orange');\"
				      onmouseout=\"this.setAttribute('stroke-opacity','0');document.getElementById('polygon_%s').setAttribute('stroke','black');document.getElementById('polygon_%s').setAttribute('stroke','black');show_current_edge()\"
				      onclick=\"alert('showModuleFromGraph::%s');hide_current_edge();current_edge_two='edge_%d';current_top_graph='polygon_%s';current_bottom_graph='polygon_%s';show_current_edge();
				      alert('showOnTop2::%s');remove_back_from_current_top();current_top='%s';add_back_from_current_top();
				    alert('showOnBottom2::%s');remove_back_from_current_bottom();current_bottom='%s';add_back_from_current_bottom();\"
				      stroke=\"orange\" stroke-dasharray=\"2\" stroke-opacity=\"0\" fill=\"orange\" fill-opacity=\"0\"
				      points=\"%s,%s %s,%s %s,%s %s,%s %s,%s %s,%s\"/>%!\n"
				      !edge_counter
				      !edge_from !edge_to
				      !edge_from !edge_to
				      !edge_to
				      !edge_counter
				      !edge_from !edge_to
				      !edge_from !edge_from
				      !edge_to !edge_to
				      !x1 !y1
				      (sof ((fos !x1) +. 5.) ^ "0") !y1
				      (sof ((fos !x2) +. 5.) ^ "0") !y2
				      (sof ((fos !x2) -. 5.) ^ "0") !y2
				      (sof ((fos !x1) -. 5.) ^ "0") !y1
				      !x1 !y1);
	      incr edge_counter;
	    ) else (
	      output_string out_ch (line^"\n");
	    );
	    graph_line_counter := 0;
	  | 99 ->
	    let ecmascript = "<script type=\"text/ecmascript\">
		<![CDATA[
		   var current_top = \"G0\";
		     var current_bottom = \""^fl^"\";
						    var current_bottom_graph = \""^fl^"\";
						                                         var current_top_graph = \"G0\";
						                                         var current_edge_two = \"\";
		   function remove_back_from_current_top() {
		       if (document.getElementById('polygon_'+current_top+'_back')) {
			 document.getElementById('polygon_'+current_top+'_back').setAttribute('fill-opacity','0');
			 if (current_top==current_bottom) {
			   document.getElementById('polygon_'+current_top+'_back').setAttribute('fill-opacity','1');
			   document.getElementById('polygon_'+current_top+'_back').setAttribute('fill','"^bottom_color^"');
			 }
		       }
		     }

		       function remove_back_from_current_bottom() {
			   if (document.getElementById('polygon_'+current_bottom+'_back')) {
			     document.getElementById('polygon_'+current_bottom+'_back').setAttribute('fill-opacity','0');
			     if (current_bottom==current_top) {
			       document.getElementById('polygon_'+current_bottom+'_back').setAttribute('fill-opacity','1');
			       document.getElementById('polygon_'+current_bottom+'_back').setAttribute('fill','"^top_color^"');
			     }
			   }
			 }

			   function add_back_from_current_top() {
			       if (document.getElementById('polygon_'+current_top+'_back')) {
				 document.getElementById('polygon_'+current_top+'_back').setAttribute('fill-opacity','1');
				 document.getElementById('polygon_'+current_top+'_back').setAttribute('fill','"^top_color^"');
				 if (current_bottom==current_top) {
				   document.getElementById('polygon_'+current_top+'_back').setAttribute('fill','"^middle_color^"');
				 }
			       }
			     }

			       function add_back_from_current_bottom() {
				   if (document.getElementById('polygon_'+current_bottom+'_back')) {
				     document.getElementById('polygon_'+current_bottom+'_back').setAttribute('fill-opacity','1');
				     document.getElementById('polygon_'+current_bottom+'_back').setAttribute('fill','"^bottom_color^"');
				     if (current_bottom==current_top) {
				       document.getElementById('polygon_'+current_top+'_back').setAttribute('fill','"^middle_color^"');
				     }
				   }
				 }

				   function hide_current_edge() {
				       if (document.getElementById(current_edge_two)) {
					 document.getElementById(current_edge_two).setAttribute('stroke','orange')
					   document.getElementById(current_top_graph).setAttribute('stroke','orange')
					   document.getElementById(current_bottom_graph).setAttribute('stroke','orange')

					   document.getElementById(current_edge_two).setAttribute('stroke-opacity','0');
					 document.getElementById(current_top_graph).setAttribute('stroke','black')
					   document.getElementById(current_bottom_graph).setAttribute('stroke','black')
				       }
				     }
				       function show_current_edge() {
					   if (document.getElementById(current_edge_two)) {
					     document.getElementById(current_edge_two).setAttribute('stroke','red')
					       document.getElementById(current_top_graph).setAttribute('stroke','red')
					       document.getElementById(current_bottom_graph).setAttribute('stroke','red')

					       document.getElementById(current_edge_two).setAttribute('stroke-opacity','1');
					     document.getElementById(current_top_graph).setAttribute('stroke-opacity','1');
					     document.getElementById(current_bottom_graph).setAttribute('stroke-opacity','1');
					   }
					 }
					     var edge_flag = false;
					   function set_edge_flag(b) { edge_flag = b; }
					       function get_edge_flag() { return edge_flag; }

 		 ]]>
	      </script>" in
	    output_string out_ch (line^"\n");
	    output_string out_ch (ecmascript^"\n");
	    graph_line_counter := 0;
	  | 59 ->
	    if (Str.string_match module_text_match line 0) then (
	      Printf.fprintf out_ch "<text text-anchor=\"middle\" x=\"%s\" y=\"%s\" font-family=\"%s\"
		font-size=\"%s\"
		font-variant=\"small-caps\">%s</text>\n"
		(Str.matched_group 1 line)
		(Str.matched_group 2 line)
		(Str.matched_group 3 line)
		(Str.matched_group 4 line)
		(Str.matched_group 5 line);
	      graph_line_counter := 0;
	    )
	  | _ -> ()
      done;
    with End_of_file ->
      close_in in_ch;
      close_out out_ch

  let current_selected_mod = ref ""

  let get_big_step_for gr =
    graph_map2 := [];
    (* Log.debug "[Grew_rew_display] get_big_step_for"; *)
    let infos = List.assoc gr !graph_map in
    (* graph,(parent_name,module_name,big_step) *)
    let final_graph = fst infos
    and (first_graph_name,module_name,bs) = snd infos in
    (* Log.debug "[Grew_rew_display] %s" module_name; *)
    current_selected_mod := module_name;
    let first_graph = fst (List.assoc first_graph_name !graph_map) in

    let fix_rule_name n = Str.global_replace (Str.regexp "-") "_" n in

    match bs with
      | None -> ""
      | Some bs ->
	let graph_counter = ref 0 in
        (*				let graph_counter_prefix = Str.global_replace (Str.regexp "G") "" first_graph_name in*)

	(* Log.fdebug "[Grew_rew_display] Father : %s | Son : %s" first_graph_name gr; *)
	let first_rule = bs.Libgrew_types.first in
	let steps = bs.Libgrew_types.small_step in
	(* Log.fdebug "[Grew_rew_display] First rule name : %s" first_rule.Libgrew.rule_name; *)

	let dot = ref "digraph G {" in
	let add str = dot := !dot^"\n"^str in

        add "    node [shape=box, fontname=Arial];";
	add "    ranksep=0.1;";
	add "    nodesep=0.1";

	graph_map2 := [("G0.2",(first_graph,("",None)))];

	add (Printf.sprintf "    G%d [style=filled, fillcolor=\"%s\", label=G%d]" !graph_counter top_color !graph_counter);
	add (Printf.sprintf "    node_mod_%d0 [fontcolor=transparent, color=transparent]" !graph_counter);
	add (Printf.sprintf "    node_mod_%d1 [label=\"%s\", color=transparent]" !graph_counter (fix_rule_name first_rule.Libgrew_types.rule_name));
	add (Printf.sprintf "    node_mod_%d0 -> node_mod_%d1 [fontcolor=transparent, color=transparent]" !graph_counter !graph_counter);
	add (Printf.sprintf "    {rank=same;node_mod_%d0; G%d}" !graph_counter !graph_counter);
	incr graph_counter;

	let rec compute rule graphs = match graphs with
	  | [] ->
	    add (Printf.sprintf "    G%d [style=filled, fillcolor=\"%s\", label=G%d]" !graph_counter bottom_color !graph_counter);
	    add (Printf.sprintf "    G%d -> G%d" (!graph_counter-1) !graph_counter);

	    add (Printf.sprintf "    node_mod_%d0 [fontcolor=transparent, color=transparent]" !graph_counter);
	    add (Printf.sprintf "    node_mod_%d1 [fontcolor=transparent, color=transparent]" !graph_counter);
	    add (Printf.sprintf "    node_mod_%d1 -> node_mod_%d0 [fontcolor=transparent, color=transparent]" (!graph_counter-1) !graph_counter);
	    add (Printf.sprintf "    node_mod_%d0 -> node_mod_%d1 [fontcolor=transparent, color=transparent]" !graph_counter !graph_counter);
	    add (Printf.sprintf "    {rank=same;node_mod_%d0; G%d}" !graph_counter !graph_counter);

	    graph_map2 := ("G"^(string_of_int !graph_counter)^".2",(final_graph,("G"^(string_of_int (!graph_counter-1))^".2",Some rule)))::(!graph_map2);

	  | (g,r)::t ->
	    add (Printf.sprintf "    G%d [label=\"G%d\"]" !graph_counter!graph_counter);
	    add (Printf.sprintf "    G%d -> G%d" (!graph_counter-1) !graph_counter);

	    add (Printf.sprintf "    node_mod_%d0 [fontcolor=transparent, color=transparent]" !graph_counter);
	    add (Printf.sprintf "    node_mod_%d1 [label=\"%s\", color=transparent]" !graph_counter (fix_rule_name r.Libgrew_types.rule_name));
	    add (Printf.sprintf "    node_mod_%d1 -> node_mod_%d0 [fontcolor=transparent, color=transparent]" (!graph_counter-1) !graph_counter);
	    add (Printf.sprintf "    node_mod_%d0 -> node_mod_%d1 [fontcolor=transparent, color=transparent]" !graph_counter !graph_counter);
	    add (Printf.sprintf "    {rank=same;node_mod_%d0; G%d}" !graph_counter !graph_counter);

	    graph_map2 := ("G"^(string_of_int !graph_counter)^".2",(g,("G"^(string_of_int (!graph_counter-1))^".2",Some rule)))::(!graph_map2);

	    incr graph_counter;
	    compute r t
	in
	compute first_rule steps;

	add "}";

	let svg_file = (Svg.dot_to_tmp !dot) in
	transform svg_file (svg_file^".trans.svg") ~show_bottom:false ("G"^(string_of_int !graph_counter)) ;
	let in_ch = open_in (svg_file^".trans.svg") in
	let out_ch = open_out svg_file in
	try while true do
	    let line = input_line in_ch in
	    (* let line = Str.global_replace (Str.regexp "var current_top = \"\";") ("var current_top = \"G0\";") line in *)
	    (* let line = Str.global_replace (Str.regexp "var current_bottom = \"\";") ("var current_bottom = \"G"^(string_of_int !graph_counter)^"\";") line in *)
	    output_string out_ch (line^"\n");
	  done; assert false;
	with End_of_file ->
	  close_in in_ch;
	  close_out out_ch;
	  svg_file

  let get_rule_for ?domain ?main_feat top_dot bottom_dot graph =
    let (gr,(gr_parent_name,rule)) = List.assoc graph !graph_map2 in
    match rule with
      | Some rule ->
      	let up = rule.Libgrew_types.up and down = rule.Libgrew_types.down in
      
      	current_bottom_deco := Some down;
      	current_top_deco := Some up;
      
      	let svg_file_top =
      	  if top_dot
          then get_dot_graph_with_background2 ?domain ?main_feat ~deco:up ~botop:(false,true) gr_parent_name
      	  else get_dep_graph_with_background2 ?domain ?main_feat ~deco:up ~botop:(false,true) gr_parent_name in
      
      	let svg_file_bottom =
      	  if bottom_dot
          then get_dot_graph_with_background2 ?domain ?main_feat ~deco:down ~botop:(true,false) graph
      	  else get_dep_graph_with_background2 ?domain ?main_feat ~deco:down ~botop:(true,false) graph in
      
      	let doc = Printf.sprintf "%s_%s.html" !current_selected_mod rule.Libgrew_types.rule_name in
      	(svg_file_top,svg_file_bottom,gr_parent_name,doc)
      | None -> failwith "get_rule_for"
end
      