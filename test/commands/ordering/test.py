#! /usr/bin/env python3
import sys
import os
sys.path.append(os.getcwd() + '/../..')
from grew_test_utils import *

print ("--------------- ordering ---------------")
success('grew_dev transform -grs code.grs -config sud -quiet -i input.json -json', 'expected.json')
