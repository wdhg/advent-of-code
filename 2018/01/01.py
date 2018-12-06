def part1():
    file = open("input.txt")
    total = 0
    while True:
        line = file.readline()
        if line == "":
            break
        operation = line[0]
        value = line[1::]
        if operation == "+":
            total += int(value)
        else:
            total -= int(value)
    print(total)

part1()

# very slow
def part2():
    instructions = open("input.txt", "r").readlines()
    instruction = 0
    total = 0
    visited = []
    while True:
        line = instructions[instruction]
        operation = line[0]
        value = line[1::]
        if operation == "+":
            total += int(value)
        else:
            total -= int(value)

        if total in visited:
            break

        visited.append(total)
        instruction += 1
        if instruction >= len(instructions):
            instruction = 0
    print(total)

part2()
