def part1():
    lines = open("input.txt").readlines()
    twos = 0
    threes = 0
    for line in lines:
        has_two = False
        has_three = False
        for char in set(line):
            if line.count(char) == 2:
                has_two = True
            if line.count(char) == 3:
                has_three = True
        if has_two:
            twos += 1
        if has_three:
            threes += 1
    print(twos * threes)

part1()

def part2():
    lines = open("input.txt").readlines()
    for i in range(0, len(lines)):
        line_a = lines[i]
        for j in range(i, len(lines)):
            line_b = lines[j]
            difference = 0
            for k in range(0, 26):
                if line_a[k] != line_b[k]:
                    difference += 1
            if difference == 1:
                print(line_a, line_b)
                exit()

part2()
