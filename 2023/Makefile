C_FLAGS = -Wall -Wextra -pedantic

main: main.c
	$(CC) $(C_FLAGS) -o main main.c

debug: main.c
	$(CC) $(C_FLAGS) -g -o debug main.c

run: main
	./main

run-debug: debug
	lldb debug
