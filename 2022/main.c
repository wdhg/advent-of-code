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

/*** bits/binary ***/

int get_set_bit(uint64_t bits) {
  int i;

  for (i = 0; i < 64; i++) {
    if (bits >> i == 1) {
      return i;
    }
  }

  assert(0);
}

/*** file ***/

/* generic line-by-line file parser */
size_t parse_file(char *filename,
                  size_t (*parse_to_buffer)(char *s, size_t s_len,
                                            void **buffer, size_t i),
                  void *buffer) {
  FILE *fp = fopen(filename, "r");
  char *line;
  size_t line_size;
  size_t i = 0;

  while ((line = fgetln(fp, &line_size)) != NULL) {
    i += parse_to_buffer(line, line_size, &buffer, i);
  }

  return i;
}

/*** day 1 ***/

#define CALORIES_FILENAME "inputs/calories"

struct calories {
  size_t size;
  int values[16];
};

size_t parse_calories(char *s, size_t s_len, void **buffer, size_t i) {
  struct calories *calories = (struct calories *)*buffer;

  if (*s == '\n') {
    return 1;
  }

  calories[i].values[calories[i].size] = strtol(s, NULL, 10);
  calories[i].size++;

  (void)s_len; /* unused */

  return 0;
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

  calories = get_calories(&calories_size);

  for (i = 0; i < calories_size; i++) {
    total_calories = sum(calories[i].values, calories[i].size);
    max_calories_minimum = minimum(max_calories, 3);
    *max_calories_minimum = max(*max_calories_minimum, total_calories);
  }

  printf("\n=== Day 1 ===\n");
  printf("Highest calories: %d\n", *maximum(max_calories, 3));
  printf("Top 3 sum is: %d\n", sum(max_calories, 3));
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

size_t parse_rps_pair(char *s, size_t s_len, void **buffer, size_t i) {
  struct rps_pair *pairs = (struct rps_pair *)*buffer;

  assert(s_len == 4); /* "A X\n" */

  pairs[i].them = s[0];
  pairs[i].us = s[2];

  (void)s_len; /* unused */

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
    us = rps_move_outcome_move[them][outcome];
    total_score_2 += rps_scores_move[us] + rps_scores_outcome[outcome];
  }

  printf("\n=== Day 2 ===\n");
  printf("Total score: %d\n", total_score_1);
  printf("Total score: %d\n", total_score_2);
}

/*** day 3 ***/

#define RUCKSACKS_FILE "inputs/rucksacks"
#define ALPHABET_SIZE 26
#define NUM_RUCKSACKS 300

struct rucksack {
  uint64_t compartment_1;
  uint64_t compartment_2;
};

/* a..z => 0..25, A..Z => 26..51 */
int alpha_to_int(char c) {
  if (c >= 'a' && c <= 'z') {
    return (int)c - 'a';
  }
  if (c >= 'A' && c <= 'Z') {
    return (int)c - 'A' + ALPHABET_SIZE;
  }

  assert(0);
}

size_t parse_rucksack(char *s, size_t s_len, void **buffer, size_t i) {
  struct rucksack *rucksacks = (struct rucksack *)*buffer;
  size_t j;

  rucksacks[i].compartment_1 = 0;
  rucksacks[i].compartment_2 = 0;

  for (j = 0; j < s_len - 1; j++) {
    int char_index = alpha_to_int(s[j]);

    if (j < (s_len - 1) / 2) {
      rucksacks[i].compartment_1 |= (uint64_t)1 << char_index;
    } else {
      rucksacks[i].compartment_2 |= (uint64_t)1 << char_index;
    }
  }

  return 1;
}

void day3() {
  struct rucksack *rucksacks = calloc(NUM_RUCKSACKS, sizeof(struct rucksack));
  size_t rucksacks_len = parse_file(RUCKSACKS_FILE, parse_rucksack, rucksacks);
  int total_priorities_1 = 0;
  int total_priorities_2 = 0;
  int i;

  assert(rucksacks_len == NUM_RUCKSACKS);

  for (i = 0; i < NUM_RUCKSACKS; i++) {
    uint64_t compartments_and =
        rucksacks[i].compartment_1 & rucksacks[i].compartment_2;
    total_priorities_1 += get_set_bit(compartments_and) + 1;

    if (i % 3 == 0) {
      compartments_and =
          (rucksacks[i + 0].compartment_1 | rucksacks[i + 0].compartment_2) &
          (rucksacks[i + 1].compartment_1 | rucksacks[i + 1].compartment_2) &
          (rucksacks[i + 2].compartment_1 | rucksacks[i + 2].compartment_2);
      total_priorities_2 += get_set_bit(compartments_and) + 1;
    }
  }

  printf("\n=== Day 3 ===\n");
  printf("Mispacked items total priorities: %d\n", total_priorities_1);
  printf("Badge letter total priorities: %d\n", total_priorities_2);
}

/*** day 4 ***/

#define ASSIGNMENTS_FILE "inputs/section-assignments"
#define NUM_ASSIGNMENTS 1000

struct assignment {
  int from;
  int to;
};

struct assignment_pair {
  struct assignment a;
  struct assignment b;
};

size_t parse_assignment_pairs(char *s, size_t s_len, void **buffer, size_t i) {
  struct assignment_pair *pairs = (struct assignment_pair *)*buffer;
  char *next_char = s;

  /* "98-98,17-99" */
  pairs[i].a.from = strtol(next_char, &next_char, 10);
  pairs[i].a.to = strtol(next_char + 1, &next_char, 10);
  pairs[i].b.from = strtol(next_char + 1, &next_char, 10);
  pairs[i].b.to = strtol(next_char + 1, &next_char, 10);

  (void)s_len; /* unused */

  return 1;
}

int assignments_nest(struct assignment a, struct assignment b) {
  return (a.from <= b.from && b.to <= a.to) ||
         (b.from <= a.from && a.to <= b.to);
}

int assignments_overlap(struct assignment a, struct assignment b) {
  return (a.from <= b.from && b.from <= a.to) ||
         (b.from <= a.from && a.from <= b.to);
}

void day4() {
  struct assignment_pair *pairs =
      calloc(NUM_ASSIGNMENTS, sizeof(struct assignment_pair));
  size_t pairs_len =
      parse_file(ASSIGNMENTS_FILE, parse_assignment_pairs, pairs);
  int i;
  int num_nested_assignments = 0;
  int num_overlapping_assingments = 0;

  assert(pairs_len == NUM_ASSIGNMENTS);

  for (i = 0; i < NUM_ASSIGNMENTS; i++) {
    if (assignments_nest(pairs[i].a, pairs[i].b)) {
      num_nested_assignments++;
    }

    if (assignments_overlap(pairs[i].a, pairs[i].b)) {
      num_overlapping_assingments++;
    }
  }

  printf("\n=== Day 4 ===\n");
  printf("Total nested assignments: %d\n", num_nested_assignments);
  printf("Total overlapping assignments: %d\n", num_overlapping_assingments);
}

/*** main ***/

int main() {
  printf("Advent of Code 2022!\n");

  day1();
  day2();
  day3();
  day4();

  return 0;
}
