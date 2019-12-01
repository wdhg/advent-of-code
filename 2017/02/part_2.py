FILENAME = 'input.txt'

def format_input_data(input_data):
    input_data = input_data.split('\n')
    for i in range(len(input_data)):
        input_data[i] = list(map(int, input_data[i].split()))
    return input_data

def row_divided_result(row):
    result = 0
    for a in range(len(row)):
        for b in range(len(row)):
            if a == b:
                continue
            result += row[b] // row[a] if row[b] % row[a] == 0 else 0
    return result

with open(FILENAME) as file:
    input_data = format_input_data(file.read())
    print(sum(map(row_divided_result, input_data)))
