#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

/*** math ***/

#define _INT_MIN ((unsigned long int)1 << (sizeof(int) * 8 - 1))
#define INT_MIN ((int)_INT_MIN)
#define INT_MAX ((int)(_INT_MIN - 1))

int max(int a, int b) {
  if (a > b) {
    return a;
  }

  return b;
}

int sum(int *vals, size_t len) {
  size_t i;
  int total = 0;

  for (i = 0; i < len; i++) {
    total += vals[i];
  }

  return total;
}

/* returns a pointer to the minimum or maximum value in an array */
int *minimum_or_maximum(int *vals, size_t len, int is_min) {
  int value;
  int *ptr = &value;
  size_t i;

  if (is_min) {
    value = INT_MAX;
  } else {
    value = INT_MIN;
  }

  for (i = 0; i < len; i++) {
    if ((is_min && vals[i] < *ptr) || (!is_min && vals[i] > *ptr)) {
      ptr = &vals[i];
    }
  }

  assert(ptr != &value);

  return ptr;
}

int *maximum(int *vals, size_t len) { return minimum_or_maximum(vals, len, 0); }
int *minimum(int *vals, size_t len) { return minimum_or_maximum(vals, len, 1); }

/*** file ***/

/* generic line-by-line file parser */
size_t parse_file(char *filename,
                  size_t (*parse_to_buffer)(char *s, size_t len, void **buffer),
                  void *buffer) {
  FILE *fp = fopen(filename, "r");
  char *line;
  size_t line_size;
  size_t output_size = 0;

  while ((line = fgetln(fp, &line_size)) != NULL) {
    output_size += parse_to_buffer(line, line_size, &buffer);
  }

  return output_size;
}

/*** day 1 ***/

#define CALORIES_FILENAME "inputs/calories"

struct calories {
  size_t size;
  int values[16];
};

size_t parse_calories(char *s, size_t len, void **buffer) {
  struct calories *calories = (struct calories *)*buffer;

  if (*s == '\n') {
    *buffer = (void *)(calories + 1); /* increment the buffer */
    return 0;
  }

  calories->values[calories->size] = strtol(s, NULL, 10);
  calories->size++;

  (void)len; /* unused */

  return 1;
}

struct calories *get_calories(size_t *size) {
  struct calories *calories = calloc(1024, sizeof(struct calories));
  int i = 0;

  for (i = 0; i < 1024; i++) {
    calories[i].size = 0;
  }

  *size = parse_file(CALORIES_FILENAME, parse_calories, calories);

  return calories;
}

void day1() {
  struct calories *calories;
  size_t calories_size;
  size_t i = 0;
  int total_calories = 0;
  int max_calories[3] = {0};
  int *max_calories_minimum;

  printf("=== Day 1 ===\n");

  calories = get_calories(&calories_size);

  for (i = 0; i < calories_size; i++) {
    total_calories = sum(calories[i].values, calories[i].size);
    max_calories_minimum = minimum(max_calories, 3);
    *max_calories_minimum = max(*max_calories_minimum, total_calories);
  }

  printf("Highest calories: %d, %d, %d\n", max_calories[0], max_calories[1],
         max_calories[2]);
  printf("Top 3 sum is: %d\n", sum(max_calories, 3));
  printf("\n");
}

/*** main ***/

int main() {
  printf("Advent of Code 2022!\n\n");

  day1();

  return 0;
}
