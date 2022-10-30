#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from utils import *

print ("--------------- count 01 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -tsv -patterns \"ADJ_NOUN.pat NOUN_ADJ.pat\" -i en_fr_zh.json", "01_expected.tsv")

print ("--------------- count 02 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -patterns \"ADJ_NOUN.pat NOUN_ADJ.pat\" -i en_fr_zh.json", "02_expected.json")

print ("--------------- count 03 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -tsv -patterns ADJ_NOUN.pat -key N.Number -i en_fr_zh.json", "03_expected.tsv")

print ("--------------- count 04 ---------------")
success("grew_dev compile -i en_fr_zh.json && grew_dev count -patterns ADJ_NOUN.pat -key N.Number -i en_fr_zh.json", "04_expected.json")

