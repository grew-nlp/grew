#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from utils import *

print ("--------------- grep ---------------")
success("grew_dev grep -pattern subcat.pat -quiet -i 01_input.conll", "01_expected.grep")

