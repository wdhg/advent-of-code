import re

def get_claims():
    with open('input.txt', 'r') as file:
        lines = file.readlines()
    claims = []
    for line in lines:
        claims.append(map(int, re.findall(r'#(\d+) @ (\d+),(\d+): (\d+)x(\d+)', line)[0]))
    return claims

def get_cells():
    claims = get_claims()
    cells = [[0] * 1000 for i in range(1000)]
    for i in range(len(claims)):
        iden, x_start, y_start, width, height = claims[i]
        for x in range(width):
            for y in range(height):
                cells[x_start + x][y_start + y] += 1
    return cells

def part1():
    cells = get_cells()
    overlapping = 0
    for x in range(len(cells)):
        for y in range(len(cells[x])):
            if cells[x][y] > 1:
                overlapping += 1
    print(overlapping)

part1()

def part2():
    claims = get_claims()
    cells = get_cells()
    for i in range(len(claims)):
        iden, x_start, y_start, width, height = claims[i]
        area = 0
        for x in range(width):
            for y in range(height):
                area += cells[x_start + x][y_start + y]
        if area == width * height:
            print(iden)
            exit()

part2()
