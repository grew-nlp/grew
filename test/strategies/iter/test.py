#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from utils import *

print ("--------------- iter ---------------")
success('grew_dev transform -grs code.grs -config basic -strat s01 -quiet -i input.json','01_expected.conll')
success('grew_dev transform -grs code.grs -config basic -quiet -strat s02 -i input.json','02_expected.conll')
