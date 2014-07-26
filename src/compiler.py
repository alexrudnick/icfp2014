#!/usr/bin/env python3

import sys
import nltk

from model import Conditional
from model import Constant
from model import Function
from model import FunctionCall
from model import FunctionMention
from model import Program
from model import VariableMention

def parse(fn):
    with open(fn) as infile:
        text = '(program' + infile.read() + ')'
        return nltk.Tree.fromstring(text)

def flatten_argument_tree(tree):
    if not tree.label():
        return []
    return [tree.label()] + [leaf for leaf in tree]

def syntax_to_function(syntax):
    assert(syntax.label() == 'defun')

    ## TODO: if this is a defun, just remember the name.
    ## if it's a lambda, just make the function object and gensym it.
    # TODO: Check for duplicates.
    argument_names = flatten_argument_tree(syntax[1])

    return Function(syntax[0], # function name
                    argument_names, # arguments
                    syntax_to_expression(syntax[2], argument_names)) # body

def syntax_to_program(syntax):
    assert(syntax.label() == 'program')
    ## instantiate children
    functions = [syntax_to_function(item) for item in syntax[:-1]]
    mainexp = syntax_to_expression(syntax[-1], set())
    return Program(functions, mainexp)

def syntax_to_expression(syntax, variable_names):
    """Recursive function."""

    if type(syntax) is str:
        ## if it's a constant...
        if syntax.isnumeric():
            return Constant(int(syntax))

        ## either a variable or a function name
        assert(syntax.isalnum())
        if syntax in Program.function_names:
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
    ## otherwise it's a function call probably
    return FunctionCall(label, child_expressions)

def main():
    assert len(sys.argv) > 1, "need a filename to compile"
    syntaxtree = parse(sys.argv[1])

    print("Tree'd input:")
    print(syntaxtree.pprint())
    program = syntax_to_program(syntaxtree)

    print("\nAST:")
    print(nltk.Tree.fromstring(str(program)).pprint())

if __name__ == "__main__": main()
