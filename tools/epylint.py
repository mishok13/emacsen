#!/usr/bin/env python

import re
import argparse
from os import environ

from subprocess import Popen, PIPE

def main(source, working_dir):
    if working_dir:
        command = 'PYTHONPATH={} pylint -f parseable -r n {}'.format(working_dir.strip(), source)
    else:
        command = "pylint -f parseable -r n {}".format(source)
    with open('/tmp/pylint.out', 'a') as f:
        f.write('command is: {}\n'.format(command))
    p = Popen(command, shell=True, stdout=PIPE)
    stdout = p.communicate()[0]
    fullpattern = re.compile("(.*?):" # filename
                             "([0-9]):" # line number
                             "\[([EFIRWC])" # error type
                             "([0-9]+).*?\]" # the error number and context (unused)
                             "(.*)" # description
                             )
    pattern = re.compile("\[([EFIRWC])" # error type
                         "([0-9]+).*?\]" # the error number and context (unused)
                         "(.*)" # description
        )

    for line in stdout.splitlines():
        line = line.strip()
        try:
            filename, linenum, line = line.split(':', 2)
        except Exception:
            pass
        match = pattern.match(line.strip())
        if match:
            errtype, errnum, description = match.groups()
            description = description.strip()
            if errtype in 'EF':
                msg = 'Error'
            else:
                msg = 'Warning'
            print "%s %s%s %s at %s line %s." % (msg, errtype, errnum,
                                                 description, filename, linenum)

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Emacs Pylint integration')
    parser.add_argument('-w', '--working-dir')
    parser.add_argument('source')
    args = parser.parse_args()
    main(args.source, args.working_dir)
