FILENAME = 'input.txt'

with open(FILENAME) as file:
    input_data = file.read()

total = 0
half_len = len(input_data) // 2

for i in range(half_len):
    if input_data[i] == input_data[i + half_len]:
        total += 2 * int(input_data[i])

print(total)
