#!/usr/bin/env python3

import argparse
import re
import sys
import nltk

from model import Conditional
from model import Constant
from model import Function
from model import FunctionCall
from model import FunctionMention
from model import LogicOperation
from model import Program
from model import VariableMention

def parse_code_string(code):
    pat = re.compile(r";.*[\n]")
    code = '(program' + code + '\n)'
    code = re.sub(pat, "", code)
    return nltk.Tree.fromstring(code)

def flatten_argument_tree(tree):
    if not tree.label():
        return []
    return [tree.label()] + [leaf for leaf in tree]

def syntax_to_function(syntax):
    assert(syntax.label() == 'defun')

    ## TODO: if this is a defun, just remember the name.
    ## if it's a lambda, just make the function object and gensym it.
    # TODO: Check for duplicates.
    function_name = syntax[0]
    argument_names = flatten_argument_tree(syntax[1])
    child_expressions = [syntax_to_expression(child_syntax, argument_names) for child_syntax in syntax[2:]]
    return Function(function_name, argument_names, child_expressions)

def syntax_to_program(syntax):
    assert(syntax.label() == 'program')
    ## instantiate children
    functions = [syntax_to_function(item) for item in syntax[:-1]]
    mainexp = syntax_to_expression(syntax[-1], set())
    return Program(functions, mainexp)

def valid_variable_name(s):
    """True if the string is an OK variable name."""
    return re.match(r"^[A-Za-z0-9-]+", s)

def syntax_to_expression(syntax, variable_names):
    """Recursive function."""

    if type(syntax) is str:
        ## if it's a constant...
        if syntax.isnumeric():
            return Constant(int(syntax))

        ## either a variable or a function name
        assert(valid_variable_name(syntax)), \
            "bad variable name: {0}".format(syntax)
        if syntax in Function.functions:
            return FunctionMention(syntax)
        if syntax in variable_names:
            return VariableMention(syntax, variable_names.index(syntax))
        assert False, "don't know what to do with token '{0}'".format(syntax)

    assert type(syntax) is nltk.Tree
    label = syntax.label()

    child_expressions = [syntax_to_expression(child, variable_names)
                         for child in syntax]
    if label == 'if':
        assert len(child_expressions) == 3, "wrong number of arguments to if"
        return Conditional(*child_expressions)

    if label in LogicOperation.operations:
        return LogicOperation.generate_operation(label, child_expressions)

    ## otherwise it's a function call probably
    return FunctionCall(label, child_expressions)

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description='Compile kinda-LISP to LambdaMan.')
    parser.add_argument('-d', '--debug', action='store_true',
                        help='Print debugging information.')
    parser.add_argument('source_files',
                        metavar='FILE',
                        type=argparse.FileType('r'),
                        nargs="+",
                        help='Files to compile.')
    args = parser.parse_args()

    source_code = "\n".join([file.read() for file in args.source_files])
    parsed_input = parse_code_string(source_code)
    if args.debug:
        print("Tree'd input:")
        print(parsed_input.pprint())

    program = syntax_to_program(parsed_input)
    if args.debug:
        print("\nAST:")
        print(nltk.Tree.fromstring(str(program)).pprint())

    program.write_instructions()
    if args.debug:
        print("function offsets:", program.function_offsets)
        print("blanche offsets:", program.branch_offsets)
        print("\nMachine Code with labels:")
        for line in program.instructions:
            print(line)

    program.labels_to_offsets()
    if args.debug:
        print("\nActual machine code:")
    for line in program.instructions:
        print(line)
