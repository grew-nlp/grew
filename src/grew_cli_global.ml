(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2023 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Conll

let quiet = ref false
let verbose = ref false

type mode = Undefined | Transform | Grep | Count | Valid | Valid_ud | Stat | Compile | Clean | Status | Build | Test
let mode = ref Undefined

let (grs: string option ref) = ref None

let (dep_dir: string option ref) = ref None

type output = Conll of  Conll_columns.t | Dot | Json | Multi_json | Tsv
let output = ref (Conll Conll_columns.default)


let (input_data : string list ref) = ref []
let (output_data : string option ref) = ref None
let strat = ref "main"
let (timeout : float option ref) = ref None
let (requests : string list ref) = ref []
let html = ref false

let corpusbank = ref (Sys.getenv_opt "CORPUSBANK")
let udtools = ref (Sys.getenv_opt "UDTOOLS")

let (clustering : string list ref) = ref []

let config = ref (Conll_config.build "ud")  (* "ud" is used as default value. *)

let force = ref false
