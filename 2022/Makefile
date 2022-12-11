CFLAGS = -Wall -Wextra -pedantic -Wlong-long -std=c89

aoc: main.c
	$(CC) -o main main.c $(CFLAGS)

debug: main.c
	$(CC) -o main main.c -g $(CFLAGS)

clean:
	rm -f main

.PHONY: clean
