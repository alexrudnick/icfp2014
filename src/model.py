#!/usr/bin/env python3

## this is like our IR. So we turn the nltk Tree into a tree of this.

class Program:
    function_names = []
    def __init__(self, functions, mainexp):
        self.functions = functions
        self.mainexp = mainexp

    def __str__(self):
        out = "(PROGRAM "
        for func in self.functions:
            out += str(func)
        out += str(self.mainexp)
        out += ")"
        return out

class FunctionCall:
    conditionals = ['>', '<', '>=', '<=', '==']
    def __init__(self, funcname, child_expressions):
        self.funcname = funcname
        self.child_expressions = child_expressions

    def __str__(self):
        out = "(CALL {0} ".format(self.funcname)
        out += " ".join(str(exp) for exp in self.child_expressions)
        out += ")"
        return out

class Function:
    def __init__(self, funcname, argument_names, body):
        self.funcname = funcname
        self.argument_names = argument_names
        self.body = body
        Program.function_names.append(funcname)

    def __str__(self):
        out = "(DEFUN {0} ".format(self.funcname)
        out += "( " + " ".join(self.argument_names) + " )"
        out += str(self.body)
        out += ") "
        return out

class Constant:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "(CONSTANT {0})".format(self.value)

class Conditional:
    def __init__(self, expression, true_case, false_case):
        self.expression = expression
        self.true_case = true_case
        self.false_case = false_case

    def __str__(self):
        return "(CONDITIONAL {0} {1} {2})".format(
            self.expression, self.true_case, self.false_case)

class VariableMention:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "(VARIABLEMENTION {0})".format(self.name)

class FunctionMention:
    def __init__(self, name):
        self.name = name

    def __str__(self):
        return "(FUNCTIONMENTION {0})".format(self.name)
