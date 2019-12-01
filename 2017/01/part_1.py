FILENAME = 'input.txt'

with open(FILENAME) as file:
    input_data = file.read()

# Add first character onto end for loop
input_data += input_data[-1]

total = 0

for i in range(len(input_data) - 1):
    if input_data[i] == input_data[i + 1]:
        total += int(input_data[i])

print(total)
