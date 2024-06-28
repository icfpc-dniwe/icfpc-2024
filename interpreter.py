class ICFPInterpreter:
    def __init__(self, program):
        self.tokens = program.split()
        self.current_token = 0
        self.beta_reduction_count = 0  # To track beta reductions
    
    def parse(self):
        if self.current_token >= len(self.tokens):
            return None
        token = self.tokens[self.current_token]
        self.current_token += 1
        
        if token[0] == 'T':
            return True
        elif token[0] == 'F':
            return False
        elif token[0] == 'I':
            return self.parse_integer(token[1:])
        elif token[0] == 'S':
            return self.parse_string(token[1:])
        elif token[0] == 'U':
            return self.parse_unary_op(token[1:])
        elif token[0] == 'B':
            return self.parse_binary_op(token[1:])
        elif token[0] == '?':
            return self.parse_if_else()
        elif token[0] == 'L':
            return self.parse_lambda(token[1:])
        elif token[0] == 'v':
            return int(token[1:])  # Variable identifier
    
    def parse_integer(self, body):
        base94_digits = body
        num = 0
        for char in base94_digits:
            num = num * 94 + (ord(char) - 33)
        return num
    
    def parse_string(self, body):
        translation_order = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
        return ''.join(translation_order[ord(char) - 33] for char in body)
    
    def parse_unary_op(self, body):
        op = body[0]
        arg = self.parse()
        if op == '-':
            return -arg
        elif op == '!':
            return not arg
        elif op == '#':
            return self.parse_integer(str(arg))
        elif op == '$':
            return self.parse_string(str(arg))
    
    def parse_binary_op(self, body):
        op = body[0]
        x = self.parse()
        y = self.parse()
        if op == '+':
            return x + y
        elif op == '-':
            return x - y
        elif op == '*':
            return x * y
        elif op == '/':
            return x // y
        elif op == '%':
            return x % y
        elif op == '<':
            return x < y
        elif op == '>':
            return x > y
        elif op == '=':
            return x == y
        elif op == '|':
            return x or y
        elif op == '&':
            return x and y
        elif op == '.':
            return str(x) + str(y)
        elif op == 'T':
            return str(y)[:x]
        elif op == 'D':
            return str(y)[x:]
        elif op == '$':
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


# Example usage:
if __name__ == "__main__":
    program = "SJ!23%}%22/2n}O.%80%#4%$}#(!2!#4%2}ee}!4}).$%8}U"
    interpreter = ICFPInterpreter(program)
    try:
        result = interpreter.evaluate()
        print("Result:", result)
    except ValueError as e:
        print(e)
