#!/usr/bin/env python

import sys
import beancount_parser_lima as lima

def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: parse.py <beancount-file>\n")
        sys.exit(1)

    lima.parse(sys.argv[1])

if __name__ == '__main__':
    main()
