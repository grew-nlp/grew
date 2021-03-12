#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from utils import *

print ("--------------- matching_complex_edges ---------------")
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_01)" -quiet -i input.conll','01_expected.conll')
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_02)" -quiet -i input.conll','02_expected.conll')
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_03)" -quiet -i input.conll','03_expected.conll')
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_04)" -quiet -i input.conll','04_expected.conll')
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_05)" -quiet -i input.conll','05_expected.conll')
success('grew_dev transform -grs code.grs -config sud -strat "Onf(r_06)" -quiet -i input.conll','06_expected.conll')
