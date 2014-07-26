#!/usr/bin/env python3

## this is like our IR. So we turn the nltk Tree into a tree of this.

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
            for label, offset in self.branch_offsets.items():
                line = line.replace(label, str(offset))
            # change all the function labels
            for label, offset in self.function_offsets.items():
                line = line.replace(label, str(offset))
            self.instructions[i] = line

class FunctionCall:
    def __init__(self, funcname, child_expressions):
        self.funcname = funcname
        self.child_expressions = child_expressions

    def __str__(self):
        out = "(CALL {0} ".format(self.funcname)
        out += " ".join(str(exp) for exp in self.child_expressions)
        out += ")"
        return out

    def write_maths_instructions(self, function_offsets, branch_offsets, instructions):
        easy = {
            '+': 'ADD',
            '-': 'SUB',
            '*': 'MUL',
            '/': 'DIV',
            '==': 'CEQ',
            '>': 'CGT',
            '>=': 'CGTE',
            '>=': 'CGTE',
        }
        if self.funcname in easy:
            instructions.append(easy[self.funcname])
            return
        assert False, "OOOPS"

    def write_list_instructions(self, function_offsets, branch_offsets, instructions):
        instructions.append(self.funcname.upper())

    def write_instructions(self, function_offsets, branch_offsets, instructions):
        expected_number_of_args = Function.number_of_args_for_function_name(self.funcname)
        number_of_args = len(self.child_expressions)
        assert number_of_args == expected_number_of_args, \
            "Wrong number of arguments to {0}. " \
            "Got {0}, expected {1}".format(number_of_args, expected_number_of_args)

        for expression in self.child_expressions:
            expression.write_instructions(function_offsets, branch_offsets, instructions)

        if self.funcname in Function.maths:
            self.write_maths_instructions(function_offsets, branch_offsets, instructions)
            return

        if self.funcname in Function.list_builtins:
            self.write_list_instructions(function_offsets, branch_offsets, instructions)
            return

        instructions.append("LDF %{0}".format(self.funcname))
        instructions.append("AP {0}".format(number_of_args))

class Function:
    functions = {}
    maths = ['+', '-', '*', '/', '<', '>', '==', '>=', '<=']
    list_builtins = ['cons', 'cdr', 'car']

    def __init__(self, funcname, argument_names, body):
        self.funcname = funcname
        self.argument_names = argument_names
        self.body = body
        Function.functions[funcname] = self

    @classmethod
    def number_of_args_for_function_name(self, funcname):
        if funcname in Function.maths or funcname == 'cons':
            return 2
        if funcname in ['cdr', 'car']:
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
        self.body.write_instructions(function_offsets, branch_offsets, instructions)
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
