#!/usr/bin/env python3

from typing import Any, Callable
import sys
from dataclasses import dataclass, field
from abc import abstractmethod


_TRANSLATION_ORDER = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!\"#$%&'()*+,-./:;<=>?@[\\]^_`|~ \n"
_CHARACTER_START = 33
_CHARACTER_END = 126
_CHARACTER_COUNT = _CHARACTER_END - _CHARACTER_START + 1
assert len(_TRANSLATION_ORDER) == _CHARACTER_COUNT


def decode_string(body: str) -> str:
    def self_translate(ch: str) -> str:
        idx = ord(ch) - _CHARACTER_START
        try:
            return _TRANSLATION_ORDER[idx]
        except IndexError as e:
            raise ValueError(f"Invalid string character: {ch}") from e

    return "".join(self_translate(char) for char in body)


def decode_integer(encoded: str) -> int:
    num = 0
    for char in encoded:
        num = num * _CHARACTER_COUNT + (ord(char) - _CHARACTER_START)
    return num


def encode_integer(num: int) -> str:
    if num == 0:
        return "!"
    else:
        ret = ""
        while num > 0:
            digit = chr(_CHARACTER_START + num % _CHARACTER_COUNT)
            ret = digit + ret
            num = num // _CHARACTER_COUNT
        return ret


# Variables are actually expressions which are recomputed each time they are used.
Variables = dict[int, "Thunk | EvaledVariable"]


@dataclass
class Thunk:
    capture: Variables
    body: "Expression"


@dataclass
class EvaledVariable:
    value: Any
    beta_reduction_count: int


@dataclass(kw_only=True)
class Context:
    variables: Variables = field(default_factory=dict)
    beta_reduction_count: int = 0
    trace: bool = False


@dataclass
class LambdaValue:
    variable: int
    capture: Variables
    body: "Expression"


class Expression:
    @abstractmethod
    def __call__(self, ctx: Context) -> Any:
        raise NotImplementedError()

    @abstractmethod
    def __repr__(self) -> str:
        raise NotImplementedError()


class Constant(Expression):
    value: Any

    def __init__(self, value: Any) -> None:
        self.value = value

    def __call__(self, ctx: Context) -> Any:
        return self.value

    def __repr__(self) -> str:
        return f"Constant({self.value})"


class Lambda(Expression):
    variable: int
    body: Expression

    def __init__(self, variable: int, body: Expression) -> None:
        self.variable = variable
        self.body = body

    def __call__(self, ctx: Context) -> Any:
        return LambdaValue(self.variable, ctx.variables, self.body)

    def __repr__(self) -> str:
        return f"Lambda({self.variable}, {self.body})"


class UnaryOp(Expression):
    NAME: str
    x: Expression

    def __init__(self, x: Expression) -> None:
        self.x = x

    def __repr__(self) -> str:
        return f"Unary{self.NAME}({self.x})"


class BinaryOp(Expression):
    NAME: str
    x: Expression
    y: Expression

    def __init__(self, x: Expression, y: Expression) -> None:
        self.x = x
        self.y = y

    def __repr__(self) -> str:
        return f"Binary{self.NAME}({self.x}, {self.y})"


class IfThenElse(Expression):
    cond: Expression
    if_expr: Expression
    else_expr: Expression

    def __init__(self, cond: Expression, if_expr: Expression, else_expr: Expression) -> None:
        self.cond = cond
        self.if_expr = if_expr
        self.else_expr = else_expr

    def __call__(self, ctx: Context) -> Any:
        if ctx.trace:
            print(f"Entering if-else with condition {self.cond}")
        cond = self.cond(ctx)
        if not isinstance(cond, bool):
            raise ValueError(f"Invalid condition for if-else: {cond}")
        if cond:
            expr = self.if_expr
        else:
            expr = self.else_expr
        if ctx.trace:
            print(f"Condition evaluated to {cond}, evaluating {expr}")
        ret = expr(ctx)
        if ctx.trace:
            print(f"Exiting if-else with condition {self.cond}, result: {ret}")
        return ret

    def __repr__(self) -> str:
        return f"IfThenElse({self.cond}, {self.if_expr}, {self.else_expr})"


class VariableRef(Expression):
    variable: int

    def __init__(self, variable: int) -> None:
        self.variable = variable

    def __call__(self, ctx: Context) -> Any:
        if ctx.trace:
            print(f"Entering variable reference {self.variable}")
        try:
            var = ctx.variables[self.variable]
        except KeyError as e:
            raise ValueError(f"Variable with identifier {self.variable} not defined") from e
        match var:
            case Thunk():
                old_variables = ctx.variables
                old_beta_reduction_count = ctx.beta_reduction_count
                try:
                    ctx.variables = var.capture
                    ret = var.body(ctx)
                    old_variables[self.variable] = EvaledVariable(ret, ctx.beta_reduction_count - old_beta_reduction_count)
                    if ctx.trace:
                        print(f"Exiting variable reference {self.variable}, result: {ret}")
                    return ret
                finally:
                    ctx.variables = old_variables
            case EvaledVariable():
                ctx.beta_reduction_count += var.beta_reduction_count
                return var.value

    def __repr__(self) -> str:
        return f"VariableRef({self.variable})"


class UnaryNegate(UnaryOp):
    NAME = "-"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        if not isinstance(x, int):
            raise ValueError(f"Invalid argument for the unary (-): {x}")
        return -x


class UnaryNot(UnaryOp):
    NAME = "!"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        if not isinstance(x, bool):
            raise ValueError(f"Invalid argument for the unary (!): {x}")
        return not x


class UnaryStringToInt(UnaryOp):
    NAME = "#"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        if not isinstance(x, str):
            raise ValueError(f"Invalid argument for the unary (#): {x}")
        return decode_integer(x)


class UnaryIntToString(UnaryOp):
    NAME = "$"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        if not isinstance(x, int):
            raise ValueError(f"Invalid argument for the unary ($): {x}")
        return encode_integer(x)


class BinaryAdd(BinaryOp):
    NAME = "+"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (+): {x}, {y}")
        return x + y


class BinarySubtract(BinaryOp):
    NAME = "-"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (-): {x}, {y}")
        return x - y


class BinaryMultiply(BinaryOp):
    NAME = "*"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (*): {x}, {y}")
        return x * y


class BinaryDivide(BinaryOp):
    NAME = "/"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (/): {x}, {y}")
        return x // y


class BinaryModulo(BinaryOp):
    NAME = "%"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (%): {x}, {y}")
        return x % y


class BinaryLessThan(BinaryOp):
    NAME = "<"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (<): {x}, {y}")
        return x < y


class BinaryGreaterThan(BinaryOp):
    NAME = ">"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, int):
            raise ValueError(f"Invalid arguments for the binary (>): {x}, {y}")
        return x > y


class BinaryEqual(BinaryOp):
    NAME = "="

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        return x == y


class BinaryOr(BinaryOp):
    NAME = "|"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, bool) or not isinstance(y, bool):
            raise ValueError(f"Invalid arguments for the binary (|): {x}, {y}")
        return x or y


class BinaryAnd(BinaryOp):
    NAME = "&"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, bool) or not isinstance(y, bool):
            raise ValueError(f"Invalid arguments for the binary (&): {x}, {y}")
        return x and y


class BinaryConcat(BinaryOp):
    NAME = "."

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, str) or not isinstance(y, str):
            raise ValueError(f"Invalid arguments for the binary (.): {x}, {y}")
        return x + y


class BinaryTake(BinaryOp):
    NAME = "T"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, str):
            raise ValueError(f"Invalid arguments for the binary (T): {x}, {y}")
        return y[:x]


class BinaryDrop(BinaryOp):
    NAME = "D"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        y = self.y(ctx)
        if not isinstance(x, int) or not isinstance(y, str):
            raise ValueError(f"Invalid arguments for the binary (D): {x}, {y}")
        return y[x:]


class BinaryApply(BinaryOp):
    NAME = "$"

    def __call__(self, ctx: Context) -> Any:
        x = self.x(ctx)
        if not isinstance(x, LambdaValue):
            raise ValueError(f"Invalid arguments for the binary ($): {x}")

        if ctx.trace:
            print(f"Entering binary apply with lambda {x} and argument {self.y}")

        # We restore the variables for evaluation of the argument.
        variable = Variable(ctx.variables, self.y)
        ctx.beta_reduction_count += 1
        variables = x.capture.copy()
        variables[x.variable] = variable
        old_variables = ctx.variables
        try:
            ctx.variables = variables
            ret = x.body(ctx)
            if ctx.trace:
                print(f"Exiting binary apply, result: {ret}")
            return ret
        finally:
            ctx.variables = old_variables


_UNARY_OPS_LIST: list[type[UnaryOp]] = [
    UnaryNegate,
    UnaryNot,
    UnaryStringToInt,
    UnaryIntToString,
]


UNARY_OPS = {op.NAME: op for op in _UNARY_OPS_LIST}


_BINARY_OPS_LIST: list[type[BinaryOp]] = [
    BinaryAdd,
    BinarySubtract,
    BinaryMultiply,
    BinaryDivide,
    BinaryModulo,
    BinaryLessThan,
    BinaryGreaterThan,
    BinaryEqual,
    BinaryOr,
    BinaryAnd,
    BinaryConcat,
    BinaryTake,
    BinaryDrop,
    BinaryApply,
]


BINARY_OPS = {op.NAME: op for op in _BINARY_OPS_LIST}


class _ICFPParser:
    tokens: list[str]
    current_token = 0

    def __init__(self, program: str) -> None:
        self.tokens = program.split()

    def parse(self) -> Expression:
        token = self.tokens[self.current_token]
        self.current_token += 1

        # Rewrite as a case expr.
        match token[0]:
            case "T":
                return Constant(True)
            case "F":
                return Constant(False)
            case "I":
                val = decode_integer(token[1:])
                return Constant(val)
            case "S":
                val = decode_string(token[1:])
                return Constant(val)
            case "U":
                try:
                    unary_op = UNARY_OPS[token[1:]]
                except KeyError as e:
                    raise ValueError(f"Unknown unary operator: {token}") from e
                arg_expr = self.parse()
                return unary_op(arg_expr)
            case "B":
                try:
                    binary_op = BINARY_OPS[token[1:]]
                except KeyError as e:
                    raise ValueError(f"Unknown binary operator: {token}") from e
                x_expr = self.parse()
                y_expr = self.parse()
                return binary_op(x_expr, y_expr)
            case "?":
                if len(token) != 1:
                    raise ValueError(f"Invalid token: {token}")
                cond = self.parse()
                if_expr = self.parse()
                else_expr = self.parse()
                return IfThenElse(cond, if_expr, else_expr)
            case "L":
                variable = decode_integer(token[1:])
                body_expr = self.parse()
                return Lambda(variable, body_expr)
            case "v":
                var_num = decode_integer(token[1:])
                return VariableRef(var_num)
            case _:
                raise ValueError(f"Invalid token: {token}")


def parse(program: str) -> Expression:
    parser = _ICFPParser(program)
    return parser.parse()


if __name__ == "__main__":
    program = sys.stdin.read().strip()
    expression = parse(program)
    result = expression(Context())
    print(result)
