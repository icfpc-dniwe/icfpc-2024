import sys
from lambdaman import solve_file
from encoder import encode_integer, encode_string


if __name__ == '__main__':
    task_idx = int(sys.stdin.read().strip())
    filename = f'lambdaman/{task_idx}.txt'
    solution = solve_file(filename)
    program = encode_string(solution)
    prefix = 'B. B. ' + encode_string('solve ') + ' ' + encode_string(f'lambdaman{task_idx} ')
    message = prefix + ' ' + program
    print(message)
