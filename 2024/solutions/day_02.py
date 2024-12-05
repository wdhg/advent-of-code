
with open("inputs/02.txt", "r") as file:
    text = file.read()


def is_safe(nums):
    diffs = [nums[i+1] - nums[i] for i in range(len(nums) - 1)]
    increasing = diffs[0] > 0
    safe = True
    
    for diff in diffs:
        if not increasing:
            diff = -diff
        if diff < 1 or diff > 3:
            safe = False
            break

    return safe



safe_count = 0

for row in text.split("\n"):
    if row == "":
        continue
    nums = [int(x) for x in row.split(" ")]
    if is_safe(nums):
        safe_count += 1

print(safe_count)



safe_count = 0

for row in text.split("\n"):
    if row == "":
        continue
    nums = [int(x) for x in row.split(" ")]
    
    safe = False
    
    for i in range(len(nums)):
        nums_2 = list(nums)
        del nums_2[i]
        safe = is_safe(nums_2)
        if safe:
            break
        
    if safe:
        safe_count += 1

print(safe_count)
