#!/usr/bin/env python

"""Pylint runner for Emacs"""

from __future__ import print_function

import re
import argparse

from subprocess import Popen, PIPE

def main(source, working_dir):
    """Main entry point"""
    if working_dir:
        command = 'PYTHONPATH={} pylint -f parseable -r n {}'.format(
            working_dir.strip(), source)
    else:
        command = "pylint -f parseable -r n {}".format(source)
    process = Popen(command, shell=True, stdout=PIPE)
    stdout = process.communicate()[0]
    pattern = re.compile("\[([EFIRWC])" # error type
                         "([0-9]+).*?\]" # the error number and context (unused)
                         "(.*)" # description
        )

    for line in stdout.splitlines():
        try:
            filename, linenum, line = line.strip().split(':', 2)
        except Exception:
            pass
        match = pattern.match(line.strip())
        if match:
            errtype, errnum, description = match.groups()
            description = description.strip()
            level = {'E': 'Error', 'F': 'Error'}.get(errtype, 'Warning')
            print(('{level} {error_type}{error_id} '
                   '{description} {path} {line}').format(
                       level=level, error_type=errtype,
                       error_id=errnum, description=description,
                       path=filename, line=linenum))

if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Emacs Pylint integration')
    parser.add_argument('-w', '--working-dir')
    parser.add_argument('source')
    args = parser.parse_args()
    main(args.source, args.working_dir)
