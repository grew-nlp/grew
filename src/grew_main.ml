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
  | Grew_args.Det -> Grew_corpus.det ()
  | Grew_args.Full -> Grew_corpus.full ()
  | Grew_args.Filter -> Grew_corpus.multi_conll ()
  | Grew_args.Grep -> Grew_corpus.grep ()

  | Grew_args.Test ->
    let fail msg =
      let rule = String.make (String.length msg) '=' in
      Log.fwarning "\n%s\n%s\n%s" rule msg rule; exit 2 in

    try
    match (!Grew_args.grs, !Grew_args.gr, !Grew_args.seq) with
      | (Some grs_file, Some gr_file, strat) ->
        let grs = Libgrew.New_grs.load grs_file in
        let gr = Libgrew.Graph.load gr_file in
        let res = Libgrew.Rewrite.new_simple_rewrite gr grs strat in
        Printf.printf ">>>> %d <<<<\n" (List.length res);
        List.iter (fun gr -> Printf.printf "===========\n%s\n///////////\n" (Libgrew.Graph.to_conll_string gr)) res
        (* Libgrew.New_grs.dump grs *)
    | _ -> Printf.printf "FAIL\n%!"
    with
      | Libgrew.Error msg ->           fail msg
      | exc ->                         fail (Printf.sprintf "Uncaught exception, please report: %s" (Printexc.to_string exc))
