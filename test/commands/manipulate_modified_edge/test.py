#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from utils import *

print ("--------------- manipulate_modified_edge ---------------")
success('grew_dev transform -grs code.grs -config sud -quiet -i 01_input.conll', '01_expected.conll')
