#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from utils import *

print ("--------------- edge_feat_update ---------------")
success('grew_dev transform -grs code.grs -config sud -quiet -i 01_input.conll', '01_expected.conll')
success('grew_dev transform -grs code.grs -config sud -quiet -i 02_input.conll', '02_expected.conll')
success('grew_dev transform -grs code.grs -config sud -quiet -i 03_input.conll', '03_expected.conll')
