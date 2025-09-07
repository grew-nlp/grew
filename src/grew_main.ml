(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Grew_cli_utils
open Grew_args
open Grew_corpusbank

let main () =
  match !Grew_cli_global.subcommand with
  | Some "transform" -> Grew_transform.transform ()
  | Some "grep" -> Grew_grep.grep ()
  | Some "count" -> Grew_count.count ()

  | Some "validate" -> validate ()
  | Some "compile" -> compile ()
  | Some "clean" -> clean ()
  | Some "status" -> status ()
  | Some "build" -> build ()
  | Some "search" -> search ()
  | Some "show" -> show ()

  | Some cmd -> Log.echo_help := true; error "Unknown command: \"%s\"" cmd
  | None -> Log.echo_help := true; error "No subcommand provided"

(* -------------------------------------------------------------------------------- *)
let _ =
  Printexc.record_backtrace true;
  handle Grew_args.parse ();
  handle main ()
