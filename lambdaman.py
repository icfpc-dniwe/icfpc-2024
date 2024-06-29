import sys
from collections import deque


def lambda_man_path(grid):
    # Convert grid to list of lists for mutability
    grid = [list(row) for row in grid]

    # Find Lambda-Man's starting position
    start_x, start_y = -1, -1
    for i in range(len(grid)):
        for j in range(len(grid[0])):
            if grid[i][j] == 'L':
                start_x, start_y = i, j
                break
        if start_x != -1:
            break

    # Directions: Up, Right, Down, Left
    directions = [(-1, 0), (0, 1), (1, 0), (0, -1)]
    dir_char = ['U', 'R', 'D', 'L']

    # BFS initialization
    queue = deque([(start_x, start_y, '')])
    visited = set([(start_x, start_y)])
    pills = sum(row.count('.') for row in grid)

    while queue:
        x, y, path = queue.popleft()

        # Check if all pills are eaten
        if pills == 0:
            return path

        for i, (dx, dy) in enumerate(directions):
            nx, ny = x + dx, y + dy

            if (0 <= nx < len(grid) and 0 <= ny < len(grid[0]) and
                    (nx, ny) not in visited and grid[nx][ny] != '#'):

                if grid[nx][ny] == '.':
                    pills -= 1
                    grid[nx][ny] = ' '  # Mark pill as eaten

                queue.append((nx, ny, path + dir_char[i]))
                visited.add((nx, ny))

    return "No solution found"


def solve_file(filename):
    with open(filename) as f:
        grid = [line.strip() for line in f if len(line) > 2]
    return lambda_man_path(grid)


if __name__ == '__main__':
    filename = sys.stdin.read().strip()
    result = solve_file(filename)
    print(result)
