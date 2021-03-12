#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from utils import *

print ("--------------- pattern ---------------")
success('grew_dev transform -grs ExtPos.grs -strat "Onf (local)" -quiet -i input.conll', 'local_expected.conll')
success('grew_dev transform -grs ExtPos.grs -strat "Onf (external)" -quiet -i input.conll', 'external_expected.conll')
success('grew_dev transform -grs ExtPos.grs -strat "Onf (nap)" -quiet -i input.conll', 'nap_expected.conll')
success('grew_dev transform -grs POS.grs -strat "Iter (r)" -quiet -i POS_input.conll', 'pos_expected.conll')
