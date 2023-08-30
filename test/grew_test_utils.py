import os.path
import subprocess
from termcolor import colored

def fail (cmd, in_msg):
    exe = subprocess.run (cmd, shell=True, capture_output=True)
    if exe.returncode == 0:
        print (colored('FAILED ', 'red'), cmd)
        print (" --> the command return 0 code (supposed to fail)")
    else:
        if in_msg in str(exe.stderr):
            print(colored('SUCCESS', 'green'))
        else:
            print (colored('FAILED ', 'red'), cmd)
            print (' --> the error shoud contain "%s" but is "%s"' % (in_msg, exe.stderr))

def success (cmd, expected):
    exe = subprocess.run (cmd, shell=True, capture_output=True)
    if exe.returncode != 0:
        print (colored('FAILED ', 'red'), cmd)
        print (' --> the command failed with error ' % exe.stderr)
    else:
        obt = exe.stdout.decode()
        if os.path.isfile(expected):
            with open(expected) as f:
                exp = f.read()
        else:
            exp = expected
        if obt == exp:
            print(colored('SUCCESS', 'green'))
        else:
            print (colored('FAILED ', 'red'), cmd)
            print ("============ Expected ============")
            print (exp)
            print ("============ Obtained ============")
            print (obt)

