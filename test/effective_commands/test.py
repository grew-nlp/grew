#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/..')
from utils import *

print ("--------------- effective commands ---------------")
print (" -- r1 --")
success('grew_dev transform -grs code.grs -strat r1 -quiet -json -i input.json', 'expected.json')
fail('grew_dev transform -grs code.grs -strat r1 -safe_commands -quiet -json -i input.json', 'already exists')
fail('grew_dev transform -grs code.grs -strat "Onf(r1)" -quiet -json -i input.json', 'More than')
fail('grew_dev transform -grs code.grs -strat "Onf(r1)" -safe_commands -quiet -json -i input.json', 'already exists')
success('grew_dev transform -grs code.grs -strat "Iter(r1)" -quiet -json -i input.json', 'expected.json')
fail('grew_dev transform -grs code.grs -strat "Iter(r1)" -safe_commands -quiet -json -i input.json', 'already exists')

print (" -- r2 --")
success('grew_dev transform -grs code.grs -strat r2 -quiet -json -i input.json', 'expected2.json')
fail('grew_dev transform -grs code.grs -strat r2 -safe_commands -quiet -json -i input.json', 'already exists')
success('grew_dev transform -grs code.grs -strat "Onf(r2)" -quiet -json -i input.json', 'expected2.json')
fail('grew_dev transform -grs code.grs -strat "Onf(r2)" -safe_commands -quiet -json -i input.json', 'already exists')
success('grew_dev transform -grs code.grs -strat "Iter(r2)" -quiet -json -i input.json', 'expected2.json')
fail('grew_dev transform -grs code.grs -strat "Iter(r2)" -safe_commands -quiet -json -i input.json', 'already exists')

