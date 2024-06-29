import sys


def spaceship_moves(target_squares):
    # Dictionary to map moves to (vx, vy) changes
    move_map = {
        '1': (-1, -1),
        '2': (0, -1),
        '3': (1, -1),
        '4': (-1, 0),
        '5': (0, 0),
        '6': (1, 0),
        '7': (-1, 1),
        '8': (0, 1),
        '9': (1, 1),
    }

    # Initial position and velocity
    x, y = 0, 0
    vx, vy = 0, 0
    moves = []

    for target_x, target_y in target_squares:
        while (x, y) != (target_x, target_y):
            # Calculate the required velocity change
            dx = target_x - x
            dy = target_y - y

            # Determine the next move to make
            if dx > vx:
                if dy > vy:
                    move = '9'
                elif dy < vy:
                    move = '3'
                else:
                    move = '6'
            elif dx < vx:
                if dy > vy:
                    move = '7'
                elif dy < vy:
                    move = '1'
                else:
                    move = '4'
            else:
                if dy > vy:
                    move = '8'
                elif dy < vy:
                    move = '2'
                else:
                    move = '5'

            # Apply the move
            dvx, dvy = move_map[move]
            vx += dvx
            vy += dvy
            x += vx
            y += vy
            moves.append(move)

    return ''.join(moves)


def parse_point(line):
    if len(line) < 1:
        return None
    return tuple(int(p) for p in line.split())


if __name__ == '__main__':
    filename = sys.stdin.read().strip()
    with open(filename) as f:
        target_squares = [parse_point(line.strip()) for line in f if len(line) > 2]
    result = spaceship_moves(target_squares)
    print(result)
