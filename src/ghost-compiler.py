#!/usr/bin/env python3

import sys

from pyparsing import *

variables = {}
free_registers = ['c', 'd', 'e', 'f', 'g', 'h']
current_label = 0
conditional_stack = []
while_stack = []
code = []

def make_label():
    global current_label
    current_label += 1
    return 'label' + str(current_label)

def find_or_allocate_register(variable_name, create=False):
    global variables
    global free_registers

    if variable_name in variables:
        return variables[variable_name]

    if not len(free_registers):
        print("Ran out of registers when allocating for {0}. :(".format(variable_name))
        sys.exit()

    if not create:
        print("Could not find variable {0}".format(variable_name))
        sys.exit()

    register = free_registers.pop()
    variables[variable_name] = register
    return register

def get_atom_production(token):
    if token.isnumeric():
        token = int(token)
        assert token >= 0 and token < 256, "Constant " + str(token) + "is not in allowed range."
        return token
    return find_or_allocate_register(token)

def produce_operation(operator, operand):
    """Produce an operation, assuming the left-hand operand is already in register a."""
    global code

    if (operator ==  '+'):
        code.append('ADD a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '-'):
        code.append('SUB a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '/'):
        code.append('DIV a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '*'):
        code.append('MUL a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '&'):
        code.append('AND a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '|'):
        code.append('OR a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '^'):
       code.append( 'XOR a,{0}'.format(get_atom_production(operand)))
    elif (operator ==  '=='):
        code.append('JEQ +3,a,{0}'.format(get_atom_production(operand)))
        code.append('MOV a,0')
        code.append('JEQ +2,0,0')
        code.append('MOV a,1')
    elif (operator ==  '!='): # a-b == 0 (false)
        code.append('JEQ +3,a,{0}'.format(get_atom_production(operand)))
        code.append('MOV a,1')
        code.append('JEQ +2,0,0')
        code.append('MOV a,0')
    else:
        assert False, "Unknown operator " + operator

def produce_expression(tokens, target=None):
    global code
    assert len(tokens) > 0, "Cannot process empty expression."

    tokens = tokens.asList()
    first = tokens.pop(0)

    if target and not len(tokens):
        code.append("MOV {0},{1}".format(target, get_atom_production(first)))
        return

    # Prepare A which is our tabulation register.
    code.append("MOV a,{0}".format(get_atom_production(first)))

    while len(tokens) > 0:
        assert len(tokens) >= 2, "Not enough terms in expression."
        produce_operation(tokens.pop(0), tokens.pop(0))

    if target:
        code.append("MOV {0},a".format(target))

def produce_set(tokens):
    tokens = tokens[0]
    target_register = find_or_allocate_register(tokens.destination, create=True)
    produce_expression(tokens.expression, target_register)

def produce_set_ghost_direction(tokens):
    global code
    tokens = tokens[0]
    produce_expression(tokens.expression)
    code.append("INT 0")

def push_conditional(tokens):
    global code
    global conditional_stack
    label = make_label()
    conditional_stack.append(label)
    produce_expression(tokens[0])
    code.append("JEQ {0}, a, 0".format(label))

def pop_conditional(tokens):
    global code
    global conditional_stack
    label = conditional_stack.pop(0)
    code.append("{0}-end:".format(label))

def push_while(tokens):
    global code
    global while_stack
    label = make_label()
    while_stack.append(label)
    code.append("{0}-start:".format(label))
    produce_expression(tokens[0])
    code.append("JEQ {0}, a, 0".format(label + "-end"))

def pop_while(tokens):
    global code
    global while_stack
    label = while_stack.pop(0)
    code.append("JEQ {0},0,0".format(label + "-start"))
    code.append("{0}-end:".format(label))

def produce_exit(tokens=None):
    global code
    code.append("HLT")

def produce_debug(tokens=None):
    global code
    code.append("INT 8")

def replace_labels(code):
    named_labels = {}

    i = 0
    new_code = []
    for code_line in code:
        if code_line.endswith(':'):
            named_labels[code_line.replace(':', '')] = i
            continue

        code_line = code_line.replace("+2", str(i+2))
        code_line = code_line.replace("+3", str(i+3))
        new_code.append(code_line)
        i += 1

    code = new_code
    new_code = []
    for code_line in code:
        for label, address in named_labels.items():
            code_line = code_line.replace(label, str(address))
        new_code.append(code_line)

    return new_code

def setup_environment():
    global code
    global variables
    code.append("INT 3") # Get our ghost index into A.
    code.append("INT 5") # Get our ghost coordinates.
    code.append("MOV [0], a") # Move our x-coordinate to 0 in memory.
    code.append("MOV [1], b") # Move our x-coordinate to 1 in memory.
    variables["xcoord"] = '[0]'
    variables["ycoord"] = '[1]'

def print_final_machine_code():
    global code
    code = replace_labels(code)
    for (i, line) in enumerate(code):
        print("{0:15} ; line {1}".format(line, i))


def convert(source_lines):
    constant = Word(nums)
    variable = Word(alphas).setResultsName("variable")

    op = Literal("+") | Literal("-") | \
         Literal("*") | Literal("/") | \
         Literal("&") | Literal("|" ) | \
         Literal("^") | Literal("==") | Literal("!=")
    atom = constant | variable.setResultsName('variable')
    expression = (atom + ZeroOrMore(op + atom))
    set_statement = \
        Group(variable.setResultsName('destination') + \
              Literal(":=") + \
              expression.setResultsName('expression')).setParseAction(produce_set)
    set_ghost_direction_statement = \
        Group(Literal("set_ghost_direction(") + \
              expression.setResultsName('expression') + \
              Literal(")")).setParseAction(produce_set_ghost_direction)

    exit_statement = Literal("exit()").setParseAction(produce_exit)

    debug_statement = Literal("debug()").setParseAction(produce_debug)

    statement = (set_statement | \
                 set_ghost_direction_statement | \
                 debug_statement | \
                 exit_statement) + \
                 Suppress(";")

    if_statement = \
        Group(Suppress("if") + Suppress("(") + expression + Suppress(")") + \
            Suppress("then")).setParseAction(push_conditional) + \
        ZeroOrMore(statement) + \
            Suppress("endif").setParseAction(pop_conditional)

    while_statement = \
        Group(Suppress("while") + Suppress("(") + expression + Suppress(")") + \
            Suppress("do")).setParseAction(push_while) + \
        ZeroOrMore(statement) + \
            Suppress("endwhile").setParseAction(pop_while)

    program = OneOrMore(statement | if_statement | while_statement)

    setup_environment()
    print(program.parseString("""
        n := 3;
        while (n) do
            debug();
        endwhile
        set_ghost_direction(3);
    """))
    produce_exit()

    print_final_machine_code()

if __name__ == "__main__":
    #parser = argparse.ArgumentParser(description='Compile kinda-LISP to LambdaMan.')
    #parser.add_argument('-d', '--debug', action='store_true',
    #                    help='Print debugging information.')
    #parser.add_argument('source_files',
    #                    metavar='FILE',
    #                    type=argparse.FileType('r'),
    #                    nargs="+",
    #                    help='Files to compile.')
    #args = parser.parse_args()
    convert("blah")
