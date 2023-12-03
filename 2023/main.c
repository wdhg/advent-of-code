#include <ctype.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define FATAL(...)                                                             \
	{                                                                            \
		__VA_OPT__(fprintf(stderr, __VA_ARGS__);)                                  \
		exit(1);                                                                   \
	}

#define MIN(a, b) (((a) < (b)) ? (a) : (b))
#define MAX(a, b) (((a) > (b)) ? (a) : (b))

typedef struct parser {
	char *text;
	size_t textSize;
	size_t i;
} Parser;

int ParserAtEnd(Parser *p) { return p->i >= p->textSize; }

int Match(Parser *p, char *pattern) {
	size_t patternLen = strlen(pattern);
	if (strncmp(&p->text[p->i], pattern, patternLen) == 0) {
		p->i += patternLen;
		return 1;
	}
	return 0;
}

void Consume(Parser *p, char *pattern) {
	size_t patternLen = strlen(pattern);
	if (strncmp(&p->text[p->i], pattern, patternLen) != 0) {
		char buf[32] = {0};
		strncpy(buf, &p->text[p->i], MIN(31, p->textSize - p->i));
		FATAL("Unable to parse pattern '%s' from '%s'\n", pattern, buf);
	}
	p->i += patternLen;
}

int ConsumeNum(Parser *p) {
	int num = 0;
	if (!sscanf(&p->text[p->i], "%d", &num)) {
		char buf[32] = {0};
		strncpy(buf, &p->text[p->i], MIN(31, p->textSize - p->i));
		FATAL("Unable to parse number '%s'\n", buf);
	}

	int digits = 0;
	for (int temp = num; temp > 0; temp /= 10) {
		digits++;
	}
	p->i += digits;

	return num;
}

void SkipUntil(Parser *p, char c) {
	for (; p->text[p->i] != c; p->i++) {
	}
}

void DayHeader(int number) {
	printf("---------- DAY %2d ----------\n", number);
}

size_t ReadFile(char *filename, char **text) {
	FILE *fptr = fopen(filename, "r");
	if (fptr == NULL) {
		FATAL("Unable to open input\n");
	}

	fseek(fptr, 0, SEEK_END);
	size_t size = ftell(fptr);
	*text       = calloc(size, sizeof(char));
	fseek(fptr, 0, SEEK_SET);
	fread(*text, sizeof(char), size, fptr);
	fclose(fptr);

	return size;
}

Parser ParseFile(char *filename) {
	Parser p   = {0};
	p.textSize = ReadFile(filename, &p.text);
	return p;
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
			char *number = numbers[j];
			if (strncmp(&text[i], number, strlen(number)) == 0) {
				size_t index   = digits2[0] == -1 ? 0 : 1;
				digits2[index] = j;
			}
		}
	}

	DayHeader(1);
	printf("Sum 1: %d\n", sum1);
	printf("Sum 2: %d\n", sum2);
}

void Day02(void) {
	typedef struct {
		int r, g, b;
	} Cubes;

	Parser p = ParseFile("inputs/02.txt");

	int sum1 = 0;
	int sum2 = 0;

	while (!ParserAtEnd(&p)) {
		Consume(&p, "Game ");
		int roundOK = 1;
		int gameID  = ConsumeNum(&p);
		Consume(&p, ": ");
		Cubes minCubes = {0};
		do {
			Cubes cubes = {0};
			do {
				int count = ConsumeNum(&p);
				Consume(&p, " ");
				if (Match(&p, "red")) {
					cubes.r    = count;
					minCubes.r = MAX(minCubes.r, count);
				} else if (Match(&p, "green")) {
					cubes.g    = count;
					minCubes.g = MAX(minCubes.g, count);
				} else if (Match(&p, "blue")) {
					cubes.b    = count;
					minCubes.b = MAX(minCubes.b, count);
				}
			} while (Match(&p, ", "));
			roundOK &= cubes.r <= 12 && cubes.g <= 13 && cubes.b <= 14;
		} while (Match(&p, "; "));
		Consume(&p, "\n");
		if (roundOK) {
			sum1 += gameID;
		}
		sum2 += minCubes.r * minCubes.g * minCubes.b;
	}

	free(p.text);

	DayHeader(2);
	printf("Sum 1: %d\n", sum1);
	printf("Sum 2: %d\n", sum2);
}

int main(void) {
	Day01();
	Day02();
}
