from z3 import *

def get_number_from_model(m, radix=9):
    raw_digits = {}
    for d in m.decls():
        b = int(d.name()[1:])
        v = m[d].as_long()
        raw_digits[b] = v
    min_digit = max(raw_digits.keys())
    max_digit = max(raw_digits.keys())

    num = 0
    for d, v in raw_digits.items():
        real_d = max_digit - d
        real_v = v - 1
        assert real_v >= 0 and real_v < radix
        num += real_v * (radix ** real_d)

    return num

def all_smt(s, initial_terms):
    def block_term(s, m, t):
        s.add(t != m.eval(t, model_completion=True))
    def fix_term(s, m, t):
        s.add(t == m.eval(t, model_completion=True))
    def all_smt_rec(terms):
        if sat == s.check():
           m = s.model()
           yield m
           for i in range(len(terms)):
               s.push()
               try:
                   block_term(s, m, terms[i])
                   for j in range(i):
                       fix_term(s, m, terms[j])
                   yield from all_smt_rec(terms[i:])
               finally:
                   s.pop()
    yield from all_smt_rec(list(initial_terms))

s = Solver()

terms = [Int(f"x{name}") for name in range(11, 100)]
for v in terms:
    s.add(v >= 1, v <= 9)

s.from_file("constr.z3")

min_number = None
for m in all_smt(s, terms):
    new_number = get_number_from_model(m)
    print(f"New number: {new_number}")
    if min_number is None:
        min_number = new_number
    else:
        min_number = min(min_number, new_number)
    print(f"New min number: {min_number}")

print("No more solutions")
