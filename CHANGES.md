# 1.8.1 (2022/01/19)
  * add grep mode with "-key" arg

# 1.8.0 (2021/12/15)
  * follow libcaml-grew update

# 1.7.0 (2021/09/20)
  * follow libcaml-grew update

## 1.6.3 (2021/07/26)
  * Bug Fix (counting error with -key option)

## 1.6.2 (2021/07/26)
  * add option `-key` for `grew count`

## 1.6.1 (2021/05/22)
  * add option `-multi_json`

# 1.6.0 (2021/05/05)
  * add -columns argument
  * fix invalid json output if there is more than one output graph
  * follow libcaml-conll and libcaml-grew updates

# 1.5.0 (2021/03/16)
  * New JSON encoding of graphs
  * add tests

## 1.4.1 (2021/02/24)
  * stat mode

# 1.4.0 (2020/10/02)
  * new subcommands `compile`, `clean`, `valid` and `count`
  * add options `-config`, `-json`

## 1.3.2 (2020/05/07)
  * fix multiple graph output with "-gr" option

## 1.3.1 (2020/01/18)
  * /!\ change output of grep command (see http://grew.fr/run#grep-mode)
  * remove Pervasives deprecated ocaml lib

# 1.3.0 (2019/06/24)
  * Usage of stdin/stdout for data handling
  * Empty GRS by default
  * Fix #7: https://gitlab.inria.fr/grew/grew/issues/7

# 1.2.0 (2019/03/26)
  * Follow libcaml-grew versioning
  * Add -track_rules argument

## 1.1.1 (2019/03/26)
  * move to opam2

## 1.1.0 (2018/11/23)
  * Follow libcaml-grew versioning

### 1.0.1 (2018/09/16)
  * Fix invalid JSON output in “grew grep”

# 1.0.0 (2018/09/10)
  * Add parseme handling of column 11 (MWE/NE)
  * Fix https://gitlab.inria.fr/grew/grew/issues/5

## 0.49.0 (2018/07/05)
  * Follow libcaml-grew versioning

## 0.48.0 (2018/06/05)
  * Follow libcaml-grew versioning

## 0.47.0 (2018/03/18)
  * Adapt to libcaml-grew 0.47
  * new json output for grep mode
  * -safe_commands option

### 0.46.2 (2017/12/17)
  * Fix doc and dev call to gui

### 0.46.1 (2017/12/14)
  * remove all code refering to Dep2pict

## 0.46.0 (2017/12/14)
  * Remove GUI (now available in a different opam package `grew_gui`)
  * New command line parsing of arguments, see `grew help`

### 0.45.1 (2017/12/07)
Bugfix: [#3](https://gitlab.inria.fr/grew/grew/issues/3) (Bug in GUI with graphviz 2.40)

## 0.45.0 (2017/10/10)
The version number is aligned of the version number of `libcaml-grew` (hence, the jump from 33 to 45!)
  * `-old_grs` option
  * `-fullscreen` option
  * GUI can now load a corpus: the option `-gr` has disapear, `-i` is used instead

## 0.33.0 (2017/09/05)
  * adapt to new GRS definition and restrict usage to deterministic strategies fir a given GRS.

## 0.32.0 (2017/04/18)
  * adapt to libcaml-grew 0.42.0 (dependence on yojson)
