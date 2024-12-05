with open("inputs/04.txt", "r") as file:
    text = file.read()


rows = text.split('\n')
rows.remove('')

height = len(rows)
width = len(rows[0])

visual_xmas = [["."] * width for _ in range(height)]
visual_x_mas = [["."] * width for _ in range(height)]

print(width, height)

def d_search(s, x, y, dx, dy):
    for i in range(len(s)):
        if rows[y + i * dy][x + i * dx] != s[i]:
            return 0
    for i in range(len(s)):
        visual_xmas[y + i * dy][x + i * dx] = s[i]
    return 1


def search(x, y):
    total_xmas = 0
    total_x_mas = 0
    
    deltas_xmas = [
        [-1, -1],
        [-1, 0],
        [-1, 1],
        [0, -1],
        [0, 0],
        [0, 1],
        [1, -1],
        [1, 0],
        [1, 1],
    ]
    
    # XMAS
    for delta in deltas_xmas:
        dx = delta[0]
        dy = delta[1]
        ex = x + 3 * dx
        ey = y + 3 * dy
        if ex >= 0 and ey >= 0 and ex < width and ey < height:
            total_xmas += d_search('XMAS', x, y, dx, dy)
    
    # X-MAS
    if rows[y][x] == 'A' and x > 0 and y > 0 and x < width - 1 and y < height - 1:
        ms = "MS"
        
        a = rows[y-1][x-1]
        b = rows[y+1][x+1]
        if a in ms and b in ms and a != b:
            c = rows[y+1][x-1]
            d = rows[y-1][x+1]
            if c in ms and d in ms and c != d:
                visual_x_mas[y][x] = rows[y][x]
                visual_x_mas[y-1][x-1] = a
                visual_x_mas[y+1][x+1] = b
                visual_x_mas[y+1][x-1] = c
                visual_x_mas[y-1][x+1] = d
                total_x_mas = 1
    
    return [total_xmas, total_x_mas]


count_xmas = 0
count_x_mas = 0

for y in range(height):
    for x in range(width):
        [xmas, x_mas] = search(x, y)
        count_xmas += xmas
        count_x_mas += x_mas
        

visual_xmas_ = '\n'.join([''.join(row) for row in visual_xmas])
print(visual_xmas_)

print('\n')

visual_x_mas_ = '\n'.join([''.join(row) for row in visual_x_mas])
print(visual_x_mas_)

print(count_xmas)
print(count_x_mas)
