(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2013 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: http://grew.loria.fr                                    *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Log

open Grew_args
IFDEF BUILD_GUI THEN
open Grew_gtk
ENDIF

let _ =
  Printexc.record_backtrace true;
  Log.set_active_levels [(*`DEBUG;*)`MESSAGE;`WARNING];
  Log.set_write_to_log_file false;

  (* ignore(open_in "/tmp/tmp.svg"); *)

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Gui ->
      IFDEF BUILD_GUI THEN
	Grew_gtk.init ()
      ELSE
	Log.critical "You can't use gui without lablwebkit installed!!"
      ENDIF
  | Grew_args.Corpus -> Grew_corpus.init ()
  | Grew_args.Det -> Grew_corpus.det ()
  | Grew_args.Index -> Grew_corpus.make_index ()
  | Grew_args.Filter -> Grew_corpus.multi_conll ()
  | Grew_args.Annot -> Grew_corpus.annot ()
  | Grew_args.Grep -> Grew_corpus.grep ()
  | Grew_args.Cluster ->
      IFDEF BUILD_CLUSTER THEN
	Grew_cluster.init ()
      ELSE
	Log.critical "You can't use cluster mode without cluster library installed!!"
      ENDIF
