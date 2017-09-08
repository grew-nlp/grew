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
open Grew_gtk

let _ =
  Printexc.record_backtrace true;
  Log.set_active_levels [`INFO; `MESSAGE; `WARNING];
  Log.set_write_to_log_file false;

  (* ignore(open_in "/tmp/tmp.svg"); *)

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Gui -> Grew_gtk.init ()
  | Grew_args.Corpus -> Grew_corpus.init ()
  | Grew_args.Transform -> Grew_corpus.transform ()
  | Grew_args.Full -> Grew_corpus.full ()
  | Grew_args.Filter -> Grew_corpus.multi_conll ()
  | Grew_args.Grep -> Grew_corpus.grep ()

  | Grew_args.Test -> failwith "No test available"
