#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from utils import *

print ("--------------- grep ---------------")
success("grew_dev grep -request subcat.req -quiet -i input.conll", "01_expected.json")

print ("--------------- grep with key ---------------")
success("grew_dev grep -request subcat.req -key V.lemma -quiet -i input.conll", "02_expected.json")

print ("--------------- grep with whether ---------------")
success('grew_dev grep -request subcat.req -whether "V.lemma=faire" -quiet -i input.conll', "03_expected.json")

print ("--------------- grep with two keys ---------------")
success('grew_dev grep -request subcat.req -key V.m -key V.t -quiet -i input.conll', "04_expected.json")

