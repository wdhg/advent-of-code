FILENAME = 'input.txt'

with open(FILENAME) as file:
    input_data = file.read()

def checksum(nums):
    # Make sure they are integers
    nums = list(map(int, nums))
    return max(nums) - min(nums)

total = 0

for row in input_data.split('\n'):
    if row != '':
        total += checksum(row.split())

print(total)
