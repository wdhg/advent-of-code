with open("inputs/03.txt", "r") as file:
    text = file.read()


def match(i, s):
    if i + len(s) >= len(text):
        return False
    
    for j in range(len(s)):
        if text[i+j] != s[j]:
            return False
    
    return True


def parse_mul(i):
    j = i + 4 # start of the first arg
    k = j + 1 # end of the first arg
    # parse first number
    while text[j:k].isnumeric():
         k += 1
    # Last character isn't numeric
    k -= 1
    # Check if we actually found a number
    if j == k:
        return 0
    
    x = int(text[j:k])
    # Match the ,
    if not match(k, ","):
        return 0
    m = k+1 # Check this
    n = m+1
    # parse second number
    while text[m:n].isnumeric():
         n += 1
    n -= 1
    # Check if we actually found a number
    if m == n:
        return 0
    y = int(text[m:n])
    if not match(n, ")"):
        return 0
    
    return x * y
    

total_a = 0
total_b = 0
enabled = True


for i in range(len(text)):
    if match(i, "mul("):
        prod = parse_mul(i)
        total_a += prod
        if enabled:
            total_b += prod
    elif match(i, "do()"):
        print(i, 'do')
        enabled = True
    elif match(i, "don't()"):
        print(i, 'dont')
        enabled = False
        

print(total_a)
print(total_b)
