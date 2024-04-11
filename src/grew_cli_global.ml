(***********************************************************************)
(*    Grew - a Graph Rewriting tool dedicated to NLP applications      *)
(*                                                                     *)
(*    Copyright 2011-2024 Inria, Université de Lorraine                *)
(*                                                                     *)
(*    Webpage: https://grew.fr                                         *)
(*    License: CeCILL (see LICENSE folder or "http://www.cecill.info") *)
(*    Authors: see AUTHORS file                                        *)
(***********************************************************************)

open Conll

let quiet = ref false
let verbose = ref false

let (subcommand: string option ref) = ref None


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

let (clustering : string list ref) = ref []

let config = ref (Conll_config.build "ud")  (* "ud" is used as default value. *)

let force = ref false
let text_from_tokens = ref false

let (anonymous_args : string list ref) = ref []


let vars = ["CORPUSBANK"; "UDTOOLS"; "SUDTOOLS"; "SUDVALIDATION"] 
let (env: (string * string) list ref) =
  let init_env =
    List.fold_left
      (fun acc k ->
        match Sys.getenv_opt k with
        | None -> acc
        | Some v -> (k,v)::acc
      ) [] vars in
  ref (init_env)
let setenv  k v = env := (k,v) :: List.remove_assoc k !env
let getenv_opt k = List.assoc_opt k !env