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

let _ =
  Printexc.record_backtrace true;
  Log.set_active_levels [`INFO; `MESSAGE; `WARNING];
  Log.set_write_to_log_file false;

  (* parsing command line args *)
  Grew_args.parse ();

  match !Grew_args.mode with
  | Grew_args.Undefined -> ()

  | Grew_args.Transform -> Grew_corpus.transform ()
  | Grew_args.Grep -> Grew_corpus.grep ()

  | Grew_args.Test -> failwith "No test available"

  | Grew_args.Gui args ->
    match Unix.system ("grew_gui " ^args) with
    | Unix.WEXITED i when i <> 0 ->
      Log.warning "It seems that grew_gui is not installed on your system. Try to run \"opam install grew_gui\""
    | _ -> ()



