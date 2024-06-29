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

    target_squares.sort(key=lambda p: abs(p[0]) + abs(p[1]))

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


def spaceship_moves2(target_squares):
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
    reverse_move_map = {v: k for k, v in move_map.items()}

    # Sort target squares based on Manhattan distance from the origin (0, 0)
    target_squares.sort(key=lambda p: abs(p[0]) + abs(p[1]))

    # Initial position and velocity
    x, y = 0, 0
    vx, vy = 0, 0
    moves = []

    for target_x, target_y in target_squares:
        # Decelerate to stop at the target cell
        while vx != 0 or vy != 0:
            if vx > 0:
                vx -= 1
            elif vx < 0:
                vx += 1
            if vy > 0:
                vy -= 1
            elif vy < 0:
                vy += 1
            x += vx
            y += vy
            move = None
            for k, (mvx, mvy) in move_map.items():
                if mvx == -1 if vx < 0 else 1 if vx > 0 else 0 and mvy == -1 if vy < 0 else 1 if vy > 0 else 0:
                    move = k
                    break
            moves.append(move)

        # Adjust velocities to reach the next target cell
        while (x, y) != (target_x, target_y):
            # Calculate the required change in position to reach the target
            dx = target_x - x
            dy = target_y - y

            # Adjust velocities more carefully
            dvx = 1 if vx < dx else -1 if vx > dx else 0
            dvy = 1 if vy < dy else -1 if vy > dy else 0

            move = reverse_move_map[(dvx, dvy)]

            # Apply the move
            vx += dvx
            vy += dvy
            x += vx
            y += vy
            moves.append(move)

    return ''.join(moves)


import heapq


def manhattan_distance(x1, y1, x2, y2):
    return abs(x1 - x2) + abs(y1 - y2)


def get_neighbors(vx, vy):
    return [(vx + dvx, vy + dvy) for dvx in [-1, 0, 1] for dvy in [-1, 0, 1]]


def a_star(start, target_squares):
    move_map = {
        (-1, -1): '1',
        (0, -1): '2',
        (1, -1): '3',
        (-1, 0): '4',
        (0, 0): '5',
        (1, 0): '6',
        (-1, 1): '7',
        (0, 1): '8',
        (1, 1): '9',
    }

    open_set = []
    heapq.heappush(open_set, (0, start, 0, 0, []))  # (priority, (x, y), vx, vy, path)
    came_from = {}
    g_score = {start: 0}
    f_score = {start: manhattan_distance(start[0], start[1], *target_squares[0])}

    while open_set:
        _, current, vx, vy, path = heapq.heappop(open_set)

        if current == target_squares[0]:
            target_squares.pop(0)
            if not target_squares:
                return path

            open_set = []
            g_score = {current: 0}
            f_score = {current: manhattan_distance(current[0], current[1], *target_squares[0])}

        for neighbor_vx, neighbor_vy in get_neighbors(vx, vy):
            neighbor = (current[0] + neighbor_vx, current[1] + neighbor_vy)
            tentative_g_score = g_score[current] + 1

            if neighbor not in g_score or tentative_g_score < g_score[neighbor]:
                g_score[neighbor] = tentative_g_score
                f_score[neighbor] = tentative_g_score + manhattan_distance(neighbor[0], neighbor[1], *target_squares[0])
                heapq.heappush(open_set, (f_score[neighbor], neighbor, neighbor_vx, neighbor_vy,
                                          path + [move_map[(neighbor_vx - vx, neighbor_vy - vy)]]))

    return []


def spaceship_moves3(target_squares):
    target_squares.sort(key=lambda p: abs(p[0]) + abs(p[1]))
    start = (0, 0)
    moves = a_star(start, target_squares)
    return ''.join(moves)


def parse_point(line):
    if len(line) < 1:
        return None
    return tuple(int(p) for p in line.split())


def solve_file(filename):
    with open(filename) as f:
        target_squares = [parse_point(line.strip()) for line in f if len(line) > 2]
    result = spaceship_moves(target_squares)
    return result


def solve_file3(filename):
    with open(filename) as f:
        target_squares = [parse_point(line.strip()) for line in f if len(line) > 2]
    result = spaceship_moves3(target_squares)
    return result


if __name__ == '__main__':
    filename = sys.stdin.read().strip()
    result = solve_file3(filename)
    print(result)
