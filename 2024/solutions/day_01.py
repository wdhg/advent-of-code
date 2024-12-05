from collections import Counter

with open("inputs/01.txt", "r") as file:
    text = file.read()

left = []
right = []

for row in text.split("\n"):
    if row == "":
        continue
    nums = row.split('   ')
    x = int(nums[0])
    y = int(nums[1])
    left.append(x)
    right.append(y)

left.sort()
right.sort()

counts = Counter(right)

total_1 = 0
total_2 = 0

for (x, y) in zip(left, right):
    total_1 += abs(x - y)
    total_2 += x * counts[x]

print(total_1)
print(total_2)
