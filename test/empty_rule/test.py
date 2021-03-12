#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from utils import *

print ("--------------- empty rule ---------------")
success("grew_dev transform -grs code.grs -strat r -quiet -json -i input.json", "expected.json")
fail ("grew_dev transform -grs code.grs -strat onf -quiet -json -i input.json", "More than")
success("grew_dev transform -grs code.grs -strat iter -quiet -json -i input.json", "expected.json")

