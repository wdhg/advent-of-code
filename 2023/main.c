#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FATAL(MSG)                                                             \
	{                                                                            \
		fprintf(stderr, MSG);                                                      \
		exit(1);                                                                   \
	}

#define MATCH(SRC, INDEX, PATTERN)                                             \
	(strncmp(&(SRC)[INDEX], (PATTERN), strlen(PATTERN)) == 0)

void DayHeader(int number) {
	printf("---------- DAY %2d ----------\n", number);
}

size_t ReadFile(char *filename, char **text) {
	FILE *fptr = fopen(filename, "r");
	if (fptr == NULL) {
		FATAL("Unable to open input");
	}

	fseek(fptr, 0, SEEK_END);
	size_t size = ftell(fptr);
	*text       = calloc(size, sizeof(char));
	fseek(fptr, 0, SEEK_SET);
	fread(*text, sizeof(char), size, fptr);
	fclose(fptr);

	return size;
}

void Day01(void) {
	char *text;
	size_t textSize = ReadFile("inputs/01.txt", &text);

	printf("%ld\n", textSize);

	int sum1 = 0;
	int sum2 = 0;

	int digits1[2] = {-1, -1};
	int digits2[2] = {-1, -1};

	for (size_t i = 0; i < textSize; i++) {
		char c = text[i];

		if (c == '\n') {
			sum1 += digits1[0] * 10 + (digits1[1] == -1 ? digits1[0] : digits1[1]);
			sum2 += digits2[0] * 10 + (digits2[1] == -1 ? digits2[0] : digits2[1]);
			digits1[0] = -1;
			digits1[1] = -1;
			digits2[0] = -1;
			digits2[1] = -1;
		}

		/* digits 0..9 */
		if (isdigit(c)) {
			size_t index   = digits1[0] == -1 ? 0 : 1;
			digits1[index] = (int)(c - '0');
			index          = digits2[0] == -1 ? 0 : 1;
			digits2[index] = (int)(c - '0');
			continue;
		}

		char *numbers[] = {
			"zero",
			"one",
			"two",
			"three",
			"four",
			"five",
			"six",
			"seven",
			"eight",
			"nine",
		};

		/* numbers zero..nine */
		for (int j = 0; j < 10; j++) {
			if (MATCH(text, i, numbers[j])) {
				size_t index   = digits2[0] == -1 ? 0 : 1;
				digits2[index] = j;
			}
		}
	}

	DayHeader(1);
	printf("Sum 1: %d\n", sum1);
	printf("Sum 2: %d\n", sum2);
}

int main(void) { Day01(); }
