#!/usr/bin/env python3

import sys
import nltk

import model

def parse(fn):
    with open(fn) as infile:
        text = '(' + infile.read() + ')'
        return nltk.Tree.fromstring(text)

def main():
    assert len(sys.argv) > 1, "need a filename to compile"
    syntaxtree = parse(sys.argv[1])

    print(syntaxtree.pprint())

if __name__ == "__main__": main()
