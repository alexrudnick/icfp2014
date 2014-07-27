#!/usr/bin/env python3

## this is like our IR. So we turn the nltk Tree into a tree of this.

import re

class Program:
    def __init__(self, functions, mainexp):
        ## XXX: nb, we're keeping all of the functions both on the Program
        ## object and as a class variable on Function; may need to clean up
        ## later.
        self.functions = functions
        self.mainexp = mainexp
        self.function_offsets = {} # name -> offset in instructions
        self.branch_offsets = {}  # branchid -> offset in instructions
        self.instructions = []  # list of instructions

    def __str__(self):
        out = "(PROGRAM "
        for func in self.functions:
            out += str(func)
        out += str(self.mainexp)
        out += ")"
        return out

    def write_instructions(self):
        ## first part: main expression
        self.mainexp.write_instructions(self.function_offsets,
                                        self.branch_offsets,
                                        self.instructions)
        self.instructions.append("RTN")

        ## then we put all the functions
        for func in self.functions:
            func.write_instructions(self.function_offsets,
                                    self.branch_offsets,
                                    self.instructions)

        ## but we left out all the conditional branch_offsets. now add those.
        for conditional in Conditional.conditionals:
            tag_true = "%{0}-true".format(conditional.conditional_id)
            tag_false = "%{0}-false".format(conditional.conditional_id)
            self.branch_offsets[tag_true] = len(self.instructions)
            conditional.true_case.write_instructions(self.function_offsets,
                                                     self.branch_offsets,
                                                     self.instructions)
            self.instructions.append("JOIN")
            self.branch_offsets[tag_false] = len(self.instructions)
            conditional.false_case.write_instructions(self.function_offsets,
                                                      self.branch_offsets,
                                                      self.instructions)
            self.instructions.append("JOIN")

    def labels_to_offsets(self):
        """Make all of our labels be numeric offsets."""
        for i,line in enumerate(self.instructions):
            # change all the branch labels
            words = line.split()
            for label, offset in self.branch_offsets.items():
                words = [str(offset) if word == label else word
                         for word in words]
            for label, offset in self.function_offsets.items():
                words = [str(offset) if word == label else word
                         for word in words]
            line = " ".join(words)
            self.instructions[i] = line

class LogicOperation:
    operations = ['and', 'or', 'not']

    @classmethod
    def generate_operation(cls, operator, child_expressions):
        assert operator in cls.operations
        if operator == 'and': # a AND b
            return Conditional(
                FunctionCall('==', [Constant(0), child_expressions[0]]),
                Constant(0), # a == 0
                Conditional( # a != 0
                    FunctionCall('==', [Constant(0), child_expressions[1]]),
                    Constant(0), # b == 0
                    Constant(1)))
        if operator == 'or': # a OR b
            return Conditional(
                FunctionCall('==', [Constant(1), child_expressions[0]]),
                Constant(1), # a == 1
                Conditional( # a != 1
                    FunctionCall('==', [Constant(1), child_expressions[1]]),
                    Constant(1), # b == 1
                    Constant(0))) # b != 1
        if operator == 'not': # NOT a
                return Conditional(
                    FunctionCall('==', [Constant(1), child_expressions[0]]),
                    Constant(0), # a == 1
                    Constant(1)) # a != 0

class FunctionCall:
    def __init__(self, funcname, child_expressions):
        self.funcname = funcname
        self.child_expressions = child_expressions
        self.reverse_child_expressions = self.funcname in ['<', '<=']

    def instructions_for_func_call(self):
        simple = {
            'cons': 'CONS',
            'cdr': 'CDR',
            'car': 'CAR',
            'debug': 'DBUG',
            '+': 'ADD',
            '-': 'SUB',
            '*': 'MUL',
            '/': 'DIV',
            '==': 'CEQ',
            '>': 'CGT',
            '>=': 'CGTE',

            '<=': 'CGT', # Arguments are reversed.
            '<': 'CGTE', # Arguments are reversed.
        }
        if self.funcname in simple:
            return [simple[self.funcname]]
        return["LDF %{0}".format(self.funcname), "AP {0}".format(len(self.child_expressions))]

    def __str__(self):
        out = "(CALL {0} ".format(self.funcname)
        out += " ".join(str(exp) for exp in self.child_expressions)
        out += ")"
        return out

    def write_list_instructions(self, function_offsets, branch_offsets, instructions):
        instructions.append(self.funcname.upper())

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        expected_number_of_args = Function.number_of_args_for_function_name(self.funcname)
        number_of_args = len(self.child_expressions)
        assert number_of_args == expected_number_of_args, \
            "Wrong number of arguments to {0}. " \
            "Got {1}, expected {2}".format(self.funcname, number_of_args, expected_number_of_args)

        expressions_copy = list(self.child_expressions)
        if self.reverse_child_expressions:
            expressions_copy.reverse()
        for expression in expressions_copy:
            expression.write_instructions(function_offsets, branch_offsets, instructions)

        instructions += self.instructions_for_func_call()


class Function:
    functions = {}
    maths = ['+', '-', '*', '/', '<', '>', '==', '>=', '<=']
    list_builtins = ['cons', 'cdr', 'car']

    def __init__(self, funcname, argument_names, child_expressions):
        self.funcname = funcname
        self.argument_names = argument_names
        self.child_expressions = child_expressions
        Function.functions[funcname] = self

    @classmethod
    def number_of_args_for_function_name(self, funcname):
        if funcname in Function.maths or funcname == 'cons':
            return 2
        if funcname in ['cdr', 'car', 'debug']:
            return 1
        return len(Function.functions[funcname].argument_names)

    def __str__(self):
        out = "(DEFUN {0} ".format(self.funcname)
        out += "( " + " ".join(self.argument_names) + " )"
        out += str(self.body)
        out += ") "
        return out

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        function_offsets["%" + self.funcname] = len(instructions)
        for expression in self.child_expressions:
            expression.write_instructions(function_offsets, branch_offsets, instructions)
        instructions.append("RTN")

class Constant:
    def __init__(self, value):
        self.value = value

    def __str__(self):
        return "(CONSTANT {0})".format(self.value)

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        instructions.append("LDC {0}".format(self.value))

class Conditional:
    gensym_num = 0

    conditionals = []

    @classmethod
    def next_gensym(cls):
        out = cls.gensym_num
        cls.gensym_num += 1
        return out

    def __init__(self, expression, true_case, false_case):
        self.expression = expression
        self.true_case = true_case
        self.false_case = false_case
        self.conditionals.append(self)
        self.conditional_id = Conditional.next_gensym()

    def __str__(self):
        return "(CONDITIONAL {0} {1} {2})".format(
            self.expression, self.true_case, self.false_case)

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        self.expression.write_instructions(function_offsets, branch_offsets, instructions)
        instructions.append("SEL %{0}-true %{0}-false".format(self.conditional_id))
        ## both branches get written at the end, see Program.write_instructions

class VariableMention:
    def __init__(self, name, index):
        self.name = name
        self.index = index

    def __str__(self):
        return "(VARIABLEMENTION {0}[{1}])".format(self.name, self.index)

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        instructions.append("LD 0 {0}".format(self.index))

class FunctionMention:
    def __init__(self, funcname):
        self.funcname = funcname

    def __str__(self):
        return "(FUNCTIONMENTION {0})".format(self.funcname)

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        instructions.append("LDF %{0}".format(self.funcname))
