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

/*** day 2 ***/

#define RPS_FILE "inputs/rock-paper-scissors"
#define RPS_PAIRS_LEN 2500

/* 0 = ROCK, 1 = PAPER, 2 = SCISSORS */
enum rps { ROCK, PAPER, SCISSORS };
/* 0 = WIN , 1 = DRAW, 2 = LOSS */
enum rps_outcome { WIN, DRAW, LOSS }; /* WIN meaning we win */

struct rps_pair {
  char them;
  char us;
};

/* first index is them, second index is us */
enum rps_outcome rps_move_move_outcome[3][3] = {
    {DRAW, WIN, LOSS}, {LOSS, DRAW, WIN}, {WIN, LOSS, DRAW}};

/* first index is their move, second index is the required outcome */
enum rps rps_move_outcome_move[3][3] = {
    {PAPER, ROCK, SCISSORS}, {SCISSORS, PAPER, ROCK}, {ROCK, SCISSORS, PAPER}};

int rps_scores_move[3] = {1, 2, 3};
int rps_scores_outcome[3] = {6, 3, 0};

enum rps rps_from_char(char c) {
  switch (c) {
  case 'A':
  case 'X':
    return ROCK;
  case 'B':
  case 'Y':
    return PAPER;
  case 'C':
  case 'Z':
    return SCISSORS;
  default:
    assert(0);
  }
}

enum rps_outcome rps_outcome_from_char(char c) {
  switch (c) {
  case 'X':
    return LOSS;
  case 'Y':
    return DRAW;
  case 'Z':
    return WIN;
  default:
    assert(0);
  }
}

size_t parse_rps_pair(char *s, size_t len, void **buffer) {
  struct rps_pair *pairs = (struct rps_pair *)*buffer;

  assert(len == 4); /* "A X\n" */

  pairs->them = s[0];
  pairs->us = s[2];
  *buffer = (void *)(pairs + 1);

  (void)len; /* unused */

  return 1;
}

void day2() {
  struct rps_pair *pairs = calloc(RPS_PAIRS_LEN, sizeof(struct rps_pair));
  size_t pairs_len = parse_file(RPS_FILE, parse_rps_pair, pairs);
  int total_score_1 = 0;
  int total_score_2 = 0;
  int i;

  assert(pairs_len == RPS_PAIRS_LEN);

  for (i = 0; i < RPS_PAIRS_LEN; i++) {
    struct rps_pair pair = pairs[i];
    enum rps them = rps_from_char(pair.them);
    enum rps us;
    enum rps_outcome outcome;

    /* strategy 1: XYZ means move */
    us = rps_from_char(pair.us);
    outcome = rps_move_move_outcome[them][us];
    total_score_1 += rps_scores_move[us] + rps_scores_outcome[outcome];

    /* strategy 2: XYZ means outcome */
    outcome = rps_outcome_from_char(pair.us);
    us = rps_move_outcome_move[them][us];
    total_score_2 += rps_scores_move[us] + rps_scores_outcome[outcome];
  }

  printf("=== Day 2 ===\n");
  printf("Total score: %d\n", total_score_1);
  printf("Total score: %d\n", total_score_2);
}

/*** main ***/

int main() {
  printf("Advent of Code 2022!\n\n");

  day1();
  day2();

  return 0;
}
