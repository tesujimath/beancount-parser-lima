#!/usr/bin/env python

import sys
import beancount_parser_lima as lima

def main():
    if len(sys.argv) != 2:
        sys.stderr.write("usage: parse.py <beancount-file>\n")
        sys.exit(1)

    sources = lima.BeancountSources(sys.argv[1])
    result = sources.parse()

    if isinstance(result, lima.ParseSuccess):
        for directive in result.directives:
            print(directive)
            print()
        print(result.options)

        print("Assets account is called '%s'" % result.options.account_name_by_type["Assets"])
        
        sources.write(result.warnings)
    else:
        sources.write(result.errors)
        sources.write(result.warnings)

if __name__ == '__main__':
    main()
