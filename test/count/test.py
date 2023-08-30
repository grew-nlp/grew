#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from grew_test_utils import *

print ("--------------- count 01 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -tsv -request ADJ_NOUN.req -request NOUN_ADJ.req -i en_fr_zh.json", "01_expected.tsv")

print ("--------------- count 02 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -request ADJ_NOUN.req -request NOUN_ADJ.req -i en_fr_zh.json", "02_expected.json")

print ("--------------- count 03 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -tsv -request ADJ_NOUN.req -key N.Number -i en_fr_zh.json", "03_expected.tsv")

print ("--------------- count 04 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -request ADJ_NOUN.req -key N.Number -i en_fr_zh.json", "04_expected.json")

