#!/usr/bin/env python3

## this is like our IR. So we turn the nltk Tree into a tree of this.

class Program:
    function_names = []
    def __init__(self, functions, mainexp):
        self.functions = functions
        self.mainexp = mainexp

class FunctionCall:
    conditionals = ['>', '<', '>=', '<=', '==']
    def __init__(self, funcname, child_expressions):
        self.funcname = funcname
        self.child_expressions = funcname

class Function:
    def __init__(self, funcname, argument_names, body):
        self.funcname = funcname
        self.argument_names = argument_names
        self.body = body
        Program.function_names.append(funcname)

class Constant:
    def __init__(self, value):
        self.value = value

class Conditional:
    def __init__(self, expression, true_case, false_case):
        self.expression = expression
        self.true_case = true_case
        self.false_case = false_case

class VariableMention:
    def __init__(self, name):
        self.name = name

class FunctionMention:
    def __init__(self, name):
        self.name = name
