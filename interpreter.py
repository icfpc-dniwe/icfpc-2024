#!/usr/bin/env python3

import sys


class ICFPInterpreter:
    def __init__(self, program):
        self.tokens = program.split()
        self.current_token = 0
        self.beta_reduction_count = 0  # To track beta reductions
        self.translation_order = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
        self.char2int = {ch: idx for idx, ch in enumerate(self.translation_order)}
        self.variables = {}

    def parse(self):
        if self.current_token >= len(self.tokens):
            return None
        token = self.tokens[self.current_token]
        self.current_token += 1

        if token[0] == "T":
            return True
        elif token[0] == "F":
            return False
        elif token[0] == "I":
            return self.parse_integer(token[1:])
        elif token[0] == "S":
            return self.parse_string(token[1:])
        elif token[0] == "U":
            return self.parse_unary_op(token[1:])
        elif token[0] == "B":
            return self.parse_binary_op(token[1:])
        elif token[0] == "?":
            return self.parse_if_else()
        elif token[0] == "L":
            return self.parse_lambda(token[1:])
        elif token[0] == "v":
            return self.var(self.parse_integer(token[1:]))  # Variable identifier

    def var(self, identifier):
        self.variables[identifier] = None
        return None, identifier

    def parse_integer(self, body, other_order=False):
        base94_digits = body
        num = 0
        if other_order:
            translate = lambda x: self.char2int[x]
        else:
            translate = lambda x: ord(x) - 33
        for char in base94_digits:
            num = num * 94 + translate(char)
        return num

    def to_base94(self, num):
        num94 = []
        while num > 0:
            num94 = [num % 94] + num94
            num = num // 94
        if len(num94) < 1:
            num94 = [0]
        return num94

    def num2str(self, num):
        num94 = self.to_base94(num)
        return "".join(self.translation_order[idx] for idx in num94)

    def parse_string(self, body):
        def self_translate(ch):
            idx = ord(ch) - 33
            if idx < 0 or idx >= len(self.translation_order):
                return ch
            else:
                return self.translation_order[idx]

        return "".join(self_translate(char) for char in body)

    def parse_unary_op(self, body):
        op = body[0]
        arg = self.parse()
        if op == "-":
            return -arg
        elif op == "!":
            return not arg
        elif op == "#":
            return self.parse_integer(arg, True)
        elif op == "$":
            return self.num2str(arg)

    def parse_binary_op(self, body):
        op = body[0]
        x = self.parse()
        y = self.parse()
        if op == "+":
            return x + y
        elif op == "-":
            return x - y
        elif op == "*":
            return x * y
        elif op == "/":
            return x // y
        elif op == "%":
            return x % y
        elif op == "<":
            return x < y
        elif op == ">":
            return x > y
        elif op == "=":
            return x == y
        elif op == "|":
            return x or y
        elif op == "&":
            return x and y
        elif op == ".":
            return str(x) + str(y)
        elif op == "T":
            return str(y)[:x]
        elif op == "D":
            return str(y)[x:]
        elif op == "$":
            return self.apply_term(x, y)

    def parse_if_else(self):
        condition = self.parse()
        true_case = self.parse()
        false_case = self.parse()
        if condition:
            return true_case
        else:
            return false_case

    def parse_lambda(self, body):
        var_num = self.parse_integer(body)
        print(var_num)
        body_expr = self.parse()
        # In this simple interpreter, we ignore the lambda abstraction
        return body_expr

    def apply_term(self, x, y):
        # Apply x (which is a lambda abstraction) to y
        return y  # For simplicity, return y directly without substitution

    def evaluate(self):
        result = self.parse()
        if self.current_token < len(self.tokens):
            remaining_tokens = len(self.tokens) - self.current_token
            raise ValueError(f"Syntax error: {remaining_tokens} tokens remaining")
        return result


if __name__ == "__main__":
    program = sys.stdin.read().strip()
    interpreter = ICFPInterpreter(program)
    result = interpreter.evaluate()
    print(result)
