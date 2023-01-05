import re

def is_adjacent(head, tail):
    h = abs(head[0] - tail[0]) + abs(head[1] - tail[1])
    if head[0] == tail[0] or head[1] == tail[1]:
        if h <= 1:
            return True
    else:
        if h <= 2:
            return True
    return False

pattern_up = re.compile(r"U (\d*)")
pattern_down = re.compile(r"D (\d*)")
pattern_right = re.compile(r"R (\d*)")
pattern_left = re.compile(r"L (\d*)")

grid = [["o" for i in range(1000)] for j in range(1000)]
grid[500][500] = "H"

current_h = [500, 500]
current_t = [500, 500]

#for g in grid:
#    print(g)

set_t = set()
set_t.add((current_t[0], current_t[1]))

with open("input.txt", "r") as f:
    for line in f:
        up = re.match(pattern_up, line.strip())
        down = re.match(pattern_down, line.strip())
        right = re.match(pattern_right, line.strip())
        left = re.match(pattern_left, line.strip())

        if up:
            spaces = int(up.group(1))
            for i in range(spaces):
                grid[current_h[0]][current_h[1]] = "o"
                current_h = [current_h[0] - 1, current_h[1]]
                grid[current_h[0]][current_h[1]] = "H"

                if not is_adjacent(current_h, current_t):
                    grid[current_t[0]][current_t[1]] = "o"
                    current_t = [current_h[0] + 1, current_h[1]]
                    grid[current_t[0]][current_t[1]] = "T"
                    set_t.add((current_t[0], current_t[1]))

                #for g in grid:
                #    print(g)
                #print("\n\n\n")

        elif down:
            spaces = int(down.group(1))
            for i in range(spaces):
                grid[current_h[0]][current_h[1]] = "o"
                current_h = [current_h[0] + 1, current_h[1]]
                grid[current_h[0]][current_h[1]] = "H"

                if not is_adjacent(current_h, current_t):
                    grid[current_t[0]][current_t[1]] = "o"
                    current_t = [current_h[0] -1, current_h[1]]
                    grid[current_t[0]][current_t[1]] = "T"
                    set_t.add((current_t[0], current_t[1]))

                #for g in grid:
                #    print(g)
                #print("\n\n\n")

        elif right:
            spaces = int(right.group(1))
            for i in range(spaces):
                grid[current_h[0]][current_h[1]] = "o"
                current_h = [current_h[0], current_h[1] + 1]
                grid[current_h[0]][current_h[1]] = "H"

                if not is_adjacent(current_h, current_t):
                    grid[current_t[0]][current_t[1]] = "o"
                    current_t = [current_h[0], current_h[1] - 1]
                    grid[current_t[0]][current_t[1]] = "T"
                    set_t.add((current_t[0], current_t[1]))

                #for g in grid:
                #    print(g)
                #print("\n\n\n")

        elif left:
            spaces = int(left.group(1))
            for i in range(spaces):
                grid[current_h[0]][current_h[1]] = "o"
                current_h = [current_h[0], current_h[1] - 1]
                grid[current_h[0]][current_h[1]] = "H"

                if not is_adjacent(current_h, current_t):
                    grid[current_t[0]][current_t[1]] = "o"
                    current_t = [current_h[0], current_h[1] + 1]
                    grid[current_t[0]][current_t[1]] = "T"
                    set_t.add((current_t[0], current_t[1]))
                '''
                for g in grid:
                    print(g)
                print("\n\n\n")
                '''
print(set_t)
print(len(set_t))
