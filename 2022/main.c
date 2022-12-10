#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*** util ***/

void reverse_string(char *s, size_t len) {
  size_t i;
  char temp;

  for (i = 0; i < len / 2; i++) {
    temp = s[len - (i + 1)];
    s[len - (i + 1)] = s[i];
    s[i] = temp;
  }
}

int unique_chars(char *s, size_t len) {
  size_t i, j;

  for (i = 0; i < len - 1; i++) {
    for (j = i + 1; j < len; j++) {
      if (s[i] == s[j]) {
        return 0;
      }
    }
  }

  return 1;
}

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

int abs(int a) {
  if (a < 0) {
    return -a;
  }
  return a;
}

int sign(int a) {
  if (a > 0) {
    return 1;
  }

  if (a < 0) {
    return -1;
  }

  return 0;
}

typedef enum dir { N, E, S, W } dir_t;

typedef struct vec {
  int x;
  int y;
} vec_t;

#define vec_Z                                                                  \
  { 0, 0 }
#define vec_N                                                                  \
  { 0, 1 }
#define vec_E                                                                  \
  { 1, 0 }
#define vec_S                                                                  \
  { 0, -1 }
#define vec_W                                                                  \
  { -1, 0 }

vec_t vec_dirs[4] = {vec_N, vec_E, vec_S, vec_W};

int vec_eq(vec_t a, vec_t b) { return a.x == b.x && a.y == b.y; }

vec_t vec_add(vec_t a, vec_t b) {
  vec_t r;
  r.x = a.x + b.x;
  r.y = a.y + b.y;
  return r;
}

vec_t vec_sub(vec_t a, vec_t b) {
  vec_t r;
  r.x = a.x - b.x;
  r.y = a.y - b.y;
  return r;
}

vec_t vec_sign(vec_t a) {
  a.x = sign(a.x);
  a.y = sign(a.y);
  return a;
}

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

/*** arena allocator ***/

/* https://www.rfleury.com/i/70173682/untangling-lifetimes-with-the-arena */

#define ARENA_SIZE 8192 * 16

/* fits into one page */
typedef struct arena_t {
  uint8_t stack[ARENA_SIZE];
  size_t allocated;
} arena_t;

arena_t *a_alloc() { return calloc(1, sizeof(arena_t)); }

void *a_push(arena_t *arena, size_t size) {
  void *ptr = &arena->stack[arena->allocated];

  if (size > ARENA_SIZE - arena->allocated) {
    assert(0);
    return NULL;
  }

  arena->allocated += size;

  return ptr;
}

void *a_push_zero(arena_t *arena, size_t size) {
  uint8_t *ptr = a_push(arena, size);
  size_t i;

  if (ptr == NULL) {
    return NULL;
  }

  for (i = 0; i < size; i++) {
    ptr[i] = 0;
  }

  return ptr;
}

#define a_push_array(arena, count, type)                                       \
  (type *)a_push((arena), (count) * sizeof(type))
#define a_push_array_zero(arena, count, type)                                  \
  (type *)a_push_zero((arena), (count) * sizeof(type))

#define a_push_struct(arena, type) (type *)a_push((arena), sizeof(type))
#define a_push_struct_zero(arena, type)                                        \
  (type *)a_push_zero((arena), sizeof(type))

/*** file ***/

/* generic line-by-line file parser */
size_t parse_file(char *filename,
                  size_t (*parse_to_buffer)(char *s, size_t s_len,
                                            void **buffer, size_t ln),
                  void *buffer) {
  FILE *fp = fopen(filename, "r");
  char *line;
  size_t line_size;
  size_t ln = 0;

  while ((line = fgetln(fp, &line_size)) != NULL) {
    ln += parse_to_buffer(line, line_size, &buffer, ln);
  }

  fclose(fp);

  return ln;
}

/*** day 1 ***/

#define CALORIES_FILENAME "inputs/calories"

struct calories {
  size_t size;
  int values[16];
};

size_t parse_calories(char *s, size_t s_len, void **buffer, size_t ln) {
  struct calories *calories = (struct calories *)*buffer;

  if (*s == '\n') {
    return 1;
  }

  calories[ln].values[calories[ln].size] = strtol(s, NULL, 10);
  calories[ln].size++;

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

size_t parse_rps_pair(char *s, size_t s_len, void **buffer, size_t ln) {
  struct rps_pair *pairs = (struct rps_pair *)*buffer;

  assert(s_len == 4); /* "A X\n" */

  pairs[ln].them = s[0];
  pairs[ln].us = s[2];

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

size_t parse_rucksack(char *s, size_t s_len, void **buffer, size_t ln) {
  struct rucksack *rucksacks = (struct rucksack *)*buffer;
  size_t i;

  rucksacks[ln].compartment_1 = 0;
  rucksacks[ln].compartment_2 = 0;

  for (i = 0; i < s_len - 1; i++) {
    int char_index = alpha_to_int(s[i]);

    if (i < (s_len - 1) / 2) {
      rucksacks[ln].compartment_1 |= (uint64_t)1 << char_index;
    } else {
      rucksacks[ln].compartment_2 |= (uint64_t)1 << char_index;
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

size_t parse_assignment_pairs(char *s, size_t s_len, void **buffer, size_t ln) {
  struct assignment_pair *pairs = (struct assignment_pair *)*buffer;
  char *next_char = s;

  /* "98-98,17-99" */
  pairs[ln].a.from = strtol(next_char, &next_char, 10);
  pairs[ln].a.to = strtol(next_char + 1, &next_char, 10);
  pairs[ln].b.from = strtol(next_char + 1, &next_char, 10);
  pairs[ln].b.to = strtol(next_char + 1, &next_char, 10);

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

/*** day 5 ***/

#define CRATES_FILE "inputs/crates"
#define NUM_CRATE_MOVES 501
#define NUM_TOWERS 9
#define MAX_TOWER_SIZE 64
#define CHARS_BETWEEN_TOWERS 4

struct crate_towers {
  char stacks[NUM_TOWERS][64];
  size_t sizes[NUM_TOWERS];
};

struct crate_move {
  int count;
  int from;
  int to;
};

struct crate_moves {
  struct crate_move arr[NUM_CRATE_MOVES];
  size_t size;
};

size_t parse_towers(char *s, size_t s_len, void **buffer, size_t ln) {
  struct crate_towers *towers = (struct crate_towers *)*buffer;
  size_t i;

  if (s[0] == 'm' || s[0] == '\n' || s[1] == '1') {
    return 1;
  }

  for (i = 0; i < NUM_TOWERS; i++) {
    char c = s[1 + i * CHARS_BETWEEN_TOWERS];

    if (c == ' ') {
      continue;
    }

    towers->stacks[i][towers->sizes[i]] = c;
    towers->sizes[i]++;
  }

  (void)s_len;
  (void)ln;

  return 1;
}

size_t parse_moves(char *s, size_t s_len, void **buffer, size_t ln) {
  struct crate_moves *moves = (struct crate_moves *)*buffer;
  struct crate_move *move = &moves->arr[moves->size];

  if (s[0] != 'm') {
    return 1;
  }

  sscanf(s, "move %d from %d to %d\n", &move->count, &move->from, &move->to);

  /* convert to indices from 0 */
  move->from--;
  move->to--;

  moves->size++;

  (void)s_len;
  (void)ln;

  return 1;
}

void init_crate_towers(struct crate_towers *towers) {
  size_t i, j;

  for (i = 0; i < NUM_TOWERS; i++) {
    towers->sizes[i] = 0;
    for (j = 0; j < MAX_TOWER_SIZE; j++) {
      towers->stacks[i][j] = '\0';
    }
  }
}

void get_top_crates(struct crate_towers *towers, char *top_crates) {
  int i;

  for (i = 0; i < NUM_TOWERS; i++) {
    int tower_size = towers->sizes[i];

    if (tower_size == 0) {
      top_crates[i] = ' ';
    } else {
      top_crates[i] = towers->stacks[i][tower_size - 1];
    }
  }
}

void execute_moves(struct crate_towers *towers, struct crate_moves *moves,
                   int is_model_9000) {
  size_t i;

  for (i = 0; i < moves->size; i++) {
    struct crate_move move = moves->arr[i];
    char *tower_from = towers->stacks[move.from];
    char *tower_to = towers->stacks[move.to];
    size_t *tower_from_size = &towers->sizes[move.from];
    size_t *tower_to_size = &towers->sizes[move.to];
    int j;

    assert(*tower_from_size >= (size_t)move.count);
    assert(*tower_to_size >= 0);

    for (j = 0; j < move.count; j++) {
      if (is_model_9000) {
        tower_to[*tower_to_size + j] = tower_from[*tower_from_size - (j + 1)];
      } else {
        tower_to[*tower_to_size + j] =
            tower_from[*tower_from_size - move.count + j];
      }
    }

    (*tower_to_size) += move.count;
    (*tower_from_size) -= move.count;

    tower_to[*tower_to_size] = '\0';
    tower_from[*tower_from_size] = '\0';
  }
}

void day5() {
  struct crate_towers towers_1, towers_2;
  struct crate_moves moves;
  size_t i;
  char top_crates_1[NUM_TOWERS + 1];
  char top_crates_2[NUM_TOWERS + 1];

  init_crate_towers(&towers_1);
  moves.size = 0;

  parse_file(CRATES_FILE, parse_towers, &towers_1);
  parse_file(CRATES_FILE, parse_moves, &moves);

  /* reverse stacks from parsed order */
  for (i = 0; i < NUM_TOWERS; i++) {
    reverse_string(towers_1.stacks[i], towers_1.sizes[i]);
  }

  towers_2 = towers_1; /* create a copy for part 2 */

  execute_moves(&towers_1, &moves, 1);
  execute_moves(&towers_2, &moves, 0);

  get_top_crates(&towers_1, top_crates_1);
  get_top_crates(&towers_2, top_crates_2);

  printf("\n=== Day 5 ===\n");
  printf("Top crates 1: %s\n", top_crates_1);
  printf("Top crates 2: %s\n", top_crates_2);
}

/*** day 6 ***/

#define SIGNAL_FILE "inputs/signal"
#define SIGNAL_BUFFER_SIZE 4096
#define MARKER_PACKET_LEN 4
#define MARKER_MESSAGE_LEN 14

size_t get_signal(char *buffer) {
  FILE *fp = fopen(SIGNAL_FILE, "r");
  size_t len = fread(buffer, sizeof(char), SIGNAL_BUFFER_SIZE, fp);
  fclose(fp);
  return len;
}

void day6() {
  char signal[SIGNAL_BUFFER_SIZE];
  size_t signal_len = get_signal(signal);
  size_t i;
  int first_marker = -1;
  int first_message = -1;

  for (i = 0; i < signal_len - 3; i++) {
    if (first_marker == -1 && unique_chars(&signal[i], MARKER_PACKET_LEN)) {
      first_marker =
          i + MARKER_PACKET_LEN - 1; /* because we want the last character */
    }

    if (first_message == -1 && unique_chars(&signal[i], MARKER_MESSAGE_LEN)) {
      first_message =
          i + MARKER_MESSAGE_LEN - 1; /* because we want the last character */
    }

    if (first_marker != -1 && first_message != -1) {
      break;
    }
  }

  assert(first_marker != -1);
  assert(first_message != -1);

  printf("\n=== Day 6 ===\n");
  printf("First marker: %d\n", first_marker + 1);
  printf("First message: %d\n", first_message + 1);
}

/*** day 7 ***/

#define COMMANDS_FILE "inputs/commands"
#define MAX_CMDS 1024
#define MAX_DIR_CHILDREN 16
#define CD_LEN_BEFORE_ARG 5
#define DIR_LEN_BEFORE_NAME 4
#define MAX_FILE_SIZE_TO_COUNT 100000
#define DISK_SPACE_TOTAL 70000000
#define DISK_SPACE_NEEDED 30000000

enum cmd_type { CD, LS };
enum file_type { F, D }; /* file and dir (FILE and DIR already defined) */

struct cmd_output {
  char *name;
  enum file_type type;

  /* file stuff */
  size_t size;
};

struct cmd {
  enum cmd_type type;

  /* cd stuff */
  char *arg;

  /* ls stuff */
  struct cmd_output **outputs;
  size_t outputs_size;
};

struct file {
  char *name;
  enum file_type type;
  struct file *parent;
  size_t size;

  /* directory stuff*/
  struct file *children[MAX_DIR_CHILDREN];
  size_t child_count;
};

struct cmd *parse_cmd(char *s, size_t s_len, arena_t *arena) {
  struct cmd *cmd = a_push_struct_zero(arena, struct cmd);

  assert(s[0] == '$');

  if (s[2] == 'c') {
    /* e.g. s = "$ cd ..\n", s_len = 8 */
    size_t arg_len = s_len - CD_LEN_BEFORE_ARG;
    cmd->type = CD;
    cmd->arg = a_push_array_zero(arena, arg_len, char);
    memcpy(cmd->arg, &s[CD_LEN_BEFORE_ARG], arg_len - 1);
    cmd->arg[arg_len] = '\0';
  } else if (s[2] == 'l') {
    /* e.g. s = "$ ls\n", s_len = 5 */
    cmd->type = LS;
    cmd->outputs =
        a_push_array_zero(arena, MAX_DIR_CHILDREN, struct cmd_output *);
    cmd->outputs_size = 0;
  } else {
    assert(0);
  }

  return cmd;
}

struct cmd_output *parse_cmd_output(char *s, size_t s_len, arena_t *arena) {
  struct cmd_output *output = a_push_struct(arena, struct cmd_output);
  char *name;
  size_t name_len;

  if (s[0] == 'd') {
    /* directory, e.g. s = "dir qrpr\n", s_len = 9 */
    output->type = D;
    name = &s[DIR_LEN_BEFORE_NAME];
  } else {
    /* file, e.g. s = "112216 slzn.jls\n", s_len = 16 */
    output->type = F;
    output->size = strtol(s, &name, 10);
    name++; /* skip whitespace */
  }

  name_len = s_len - (name - s);
  output->name = a_push_array_zero(arena, name_len, char);
  memcpy(output->name, name, name_len - 1);
  output->name[name_len] = '\0';

  return output;
}

size_t get_cmds(arena_t *arena, struct cmd ***cmds_ptr) {
  FILE *fp = fopen(COMMANDS_FILE, "r");
  char *line;
  size_t line_size;
  struct cmd **cmds = a_push_array_zero(arena, MAX_CMDS, struct cmd *);
  size_t cmds_len = 0;

  *cmds_ptr = cmds;

  while ((line = fgetln(fp, &line_size)) != NULL) {
    if (line[0] == '$') {
      cmds[cmds_len] = parse_cmd(line, line_size, arena);
      cmds_len++;
    } else {
      assert(cmds[cmds_len - 1]->outputs_size < MAX_DIR_CHILDREN);
      cmds[cmds_len - 1]->outputs[cmds[cmds_len - 1]->outputs_size] =
          parse_cmd_output(line, line_size, arena);
      cmds[cmds_len - 1]->outputs_size++;
    }
  }

  return cmds_len;
}

struct file *add_child(struct file *dir, char *name, arena_t *arena) {
  struct file *child;
  size_t name_len = strlen(name);

  assert(dir->child_count < MAX_DIR_CHILDREN - 1);

  child = a_push_struct_zero(arena, struct file);
  child->name = a_push_array_zero(arena, name_len + 1, char);
  strcpy(child->name, name);
  child->parent = dir;

  dir->children[dir->child_count] = child;
  dir->child_count++;

  return child;
}

void add_file(struct file *dir, char *name, size_t size, arena_t *arena) {
  struct file *f = add_child(dir, name, arena);

  f->type = F;
  f->size = size;

  /* propagate size */
  while (f->parent != NULL) {
    f->parent->size += size;
    f = f->parent;
  }
}

void add_dir(struct file *dir, char *name, arena_t *arena) {
  struct file *f = add_child(dir, name, arena);

  f->type = D;
  f->child_count = 0;
  f->size = 0;
}

struct file *cmd_cd(struct file *cur_dir, char *name) {
  size_t i;

  if (strcmp(name, "..") == 0) {
    return cur_dir->parent;
  }

  for (i = 0; i < cur_dir->child_count; i++) {
    struct file *child = cur_dir->children[i];

    if (strcmp(child->name, name) == 0) {
      return child;
    }
  }

  assert(0);
}

struct file *get_files(struct cmd **cmds, size_t cmds_len, arena_t *arena) {
  struct file *root = a_push_struct_zero(arena, struct file);
  struct file *cur_dir = root;
  size_t i, j;

  root->name = a_push_array_zero(arena, 2, char);
  strcpy(root->name, "/");

  /* skip first command "$ cd /" */
  for (i = 1; i < cmds_len; i++) {
    struct cmd *cmd = cmds[i];

    if (cmd->type == CD) {
      cur_dir = cmd_cd(cur_dir, cmd->arg);
    } else {
      for (j = 0; j < cmd->outputs_size; j++) {
        struct cmd_output *output = cmd->outputs[j];

        if (output->type == F) {
          add_file(cur_dir, output->name, output->size, arena);
        } else {
          add_dir(cur_dir, output->name, arena);
        }
      }
    }
  }

  return root;
}

void print_cmds(struct cmd **cmds, size_t cmds_len) {
  size_t i, j;

  for (i = 0; i < cmds_len; i++) {
    if (cmds[i]->type == CD) {
      printf("$ cd %s\n", cmds[i]->arg);
    } else {
      printf("$ ls\n");
      for (j = 0; j < cmds[i]->outputs_size; j++) {
        if (cmds[i]->outputs[j]->type == F) {
          printf("%ld %s\n", cmds[i]->outputs[j]->size,
                 cmds[i]->outputs[j]->name);
        } else {
          printf("dir %s\n", cmds[i]->outputs[j]->name);
        }
      }
    }
  }
}

size_t get_dir_size_total_under_limit(struct file *dir) {
  size_t i;
  size_t size = 0;

  for (i = 0; i < dir->child_count; i++) {
    struct file *child = dir->children[i];

    if (child->type == D) {
      if (child->size < MAX_FILE_SIZE_TO_COUNT) {
        size += child->size;
      }
      size += get_dir_size_total_under_limit(child);
    }
  }

  return size;
}

struct file *get_smallest_dir_to_delete(struct file *dir, size_t space_needed) {
  struct file *smallest;
  size_t i;

  if (dir->size < space_needed) {
    return NULL;
  }

  smallest = dir;

  for (i = 0; i < dir->child_count; i++) {
    struct file *child = dir->children[i];
    if (child->type == D && child->size >= space_needed) {
      struct file *child_smallest =
          get_smallest_dir_to_delete(child, space_needed);

      if (child_smallest != NULL && child_smallest->size < smallest->size) {
        smallest = child_smallest;
      } else if (child->size < smallest->size) {
        smallest = child;
      }
    }
  }

  return smallest;
}

void day7() {
  arena_t *cmds_arena = a_alloc();
  struct cmd **cmds;
  size_t cmds_len = get_cmds(cmds_arena, &cmds);
  arena_t *files_arena = a_alloc();
  struct file *root = get_files(cmds, cmds_len, files_arena);
  size_t total_size_under_limit = get_dir_size_total_under_limit(root);
  size_t space_needed = DISK_SPACE_NEEDED - (DISK_SPACE_TOTAL - root->size);
  struct file *smallest_dir_to_delete =
      get_smallest_dir_to_delete(root, space_needed);

  printf("\n=== Day 7 ===\n");
  printf("Total size under limit: %ld\n", total_size_under_limit);
  printf("Smallest dir size to delete: %ld\n", smallest_dir_to_delete->size);
}

/*** day 8 ***/

#define TREES_FILE "inputs/trees"
#define GRID_SIZE 99

uint8_t *get_trees() {
  uint8_t *trees = calloc(GRID_SIZE * GRID_SIZE, sizeof(uint8_t));
  FILE *fp = fopen(TREES_FILE, "r");
  char *line;
  size_t line_size;
  size_t x = 0;
  size_t y = 0;

  while ((line = fgetln(fp, &line_size)) != NULL) {
    assert(line_size - 1 == GRID_SIZE);

    for (x = 0; x < GRID_SIZE; x++) {
      trees[x + y * GRID_SIZE] = line[x];
    }

    y++;
  }

  fclose(fp);

  return trees;
}

void propagate_visibility_in_dir(uint8_t *trees, uint8_t *visibility, int x,
                                 int y, int dx, int dy) {
  uint8_t shortest_height = 0;

  while (x >= 0 && y >= 0 && x < GRID_SIZE && y < GRID_SIZE) {
    uint8_t height = trees[x + y * GRID_SIZE];

    if (height > shortest_height) {
      shortest_height = height;
      visibility[x + y * GRID_SIZE] = 1;
    }

    x += dx;
    y += dy;
  }
}

uint8_t *get_visbility(uint8_t *trees) {
  uint8_t *visibility = calloc(GRID_SIZE * GRID_SIZE, sizeof(uint8_t));
  int x, y;

  for (x = 0; x < GRID_SIZE; x++) {
    propagate_visibility_in_dir(trees, visibility, x, GRID_SIZE - 1, 0, -1);
    propagate_visibility_in_dir(trees, visibility, x, 0, 0, 1);
  }

  for (y = 0; y < GRID_SIZE; y++) {
    propagate_visibility_in_dir(trees, visibility, 0, y, 1, 0);
    propagate_visibility_in_dir(trees, visibility, GRID_SIZE - 1, y, -1, 0);
  }

  return visibility;
}

int count_visible(uint8_t *visibility) {
  int total_visible = 0;
  int x, y;

  for (y = 0; y < GRID_SIZE; y++) {
    for (x = 0; x < GRID_SIZE; x++) {
      total_visible += visibility[x + y * GRID_SIZE];
    }
  }

  return total_visible;
}

int in_grid(int i) { return i >= 0 && i < GRID_SIZE; }

int count_visible_in_dir(uint8_t *trees, int x, int y, int dx, int dy) {
  int x_next = x + dx;
  int y_next = y + dy;
  int count = 0;
  uint8_t max_height = trees[x + y * GRID_SIZE];

  if (!in_grid(x_next) || !in_grid(y_next)) {
    return 0;
  }

  while (in_grid(x_next) && in_grid(y_next)) {
    count++;

    if (trees[x_next + y_next * GRID_SIZE] >= max_height) {
      break;
    }

    x_next += dx;
    y_next += dy;
  }

  return count;
}

int get_scenic_score(uint8_t *trees, int x, int y) {
  return count_visible_in_dir(trees, x, y, 0, -1) *
         count_visible_in_dir(trees, x, y, 0, 1) *
         count_visible_in_dir(trees, x, y, -1, 0) *
         count_visible_in_dir(trees, x, y, 1, 0);
}

void day8() {
  uint8_t *trees = get_trees();
  int x, y;
  uint8_t *visibility = get_visbility(trees);
  int total_visible = count_visible(visibility);
  int max_scenic_score = 0;

  for (x = 0; x < GRID_SIZE; x++) {
    for (y = 0; y < GRID_SIZE; y++) {
      max_scenic_score = max(max_scenic_score, get_scenic_score(trees, x, y));
    }
  }

  printf("\n=== Day 8 ===\n");
  printf("Total trees visible: %d\n", total_visible);
  printf("Max scenic score: %d\n", max_scenic_score);
}

/*** day 9 ***/

#define ROPE_FILE "inputs/rope"
#define MAX_NUM_MOVES 2000
#define ROPE_GRID_SIZE 1000

struct rope_move {
  dir_t dir;
  int distance;
};

dir_t rope_dir_from_char(char c) {
  switch (c) {
  case 'U':
    return N;
  case 'D':
    return S;
  case 'L':
    return W;
  case 'R':
    return E;
  default:
    assert(0);
  }
}

size_t parse_rope_move(char *s, size_t s_len, void **buffer, size_t ln) {
  struct rope_move *moves = (struct rope_move *)*buffer;
  char dir;

  sscanf(s, "%c %d\n", &dir, &moves[ln].distance);
  moves[ln].dir = rope_dir_from_char(dir);

  (void)s_len;
  (void)ln;

  return 1;
}

int rope_adjacent(vec_t head, vec_t tail) {
  vec_t diff = vec_sub(head, tail);
  return abs(diff.x) <= 1 && abs(diff.y) <= 1;
}

void set_rope_grid(uint8_t *rope_grid, int x, int y, int bit) {
  x += ROPE_GRID_SIZE / 2;
  y += ROPE_GRID_SIZE / 2;

  rope_grid[x + y * ROPE_GRID_SIZE] |= 1 << bit;
}

int count_visited(uint8_t *rope_grid, int bit) {
  int i;
  int visited = 0;

  for (i = 0; i < ROPE_GRID_SIZE * ROPE_GRID_SIZE; i++) {
    if (rope_grid[i] & 1 << bit) {
      visited++;
    }
  }

  return visited;
}

void day9() {
  struct rope_move *moves = calloc(MAX_NUM_MOVES, sizeof(struct rope_move));
  size_t moves_size = parse_file(ROPE_FILE, parse_rope_move, moves);
  uint8_t *rope_grid = calloc(ROPE_GRID_SIZE * ROPE_GRID_SIZE, sizeof(uint8_t));
  vec_t rope[10] = {vec_Z};
  size_t i, j, r;

  set_rope_grid(rope_grid, 0, 0, 0);
  set_rope_grid(rope_grid, 0, 0, 1);

  for (i = 0; i < moves_size; i++) {
    struct rope_move move = moves[i];
    vec_t dir = vec_dirs[move.dir];
    for (j = 0; j < (size_t)move.distance; j++) {
      rope[0] = vec_add(rope[0], dir);

      for (r = 1; r < 10; r++) {
        if (!rope_adjacent(rope[r], rope[r - 1])) {
          vec_t diff = vec_sub(rope[r - 1], rope[r]);
          rope[r] = vec_add(rope[r], vec_sign(diff));
        }
      }

      set_rope_grid(rope_grid, rope[1].x, rope[1].y, 0);
      set_rope_grid(rope_grid, rope[9].x, rope[9].y, 1);
    }
  }

  printf("\n=== Day 9 ===\n");
  printf("Rope tail(1) visited: %d\n", count_visited(rope_grid, 0));
  printf("Rope tail(9) visited: %d\n", count_visited(rope_grid, 1));
}

/*** day 10 ***/

#define CPU_SIGNAL_FILE "inputs/cpu-signal"
#define MAX_CPU_INSTRS 200
#define SCREEN_WIDTH 40
#define SCREEN_HEIGHT 6

enum cpu_instr_type { NOOP, ADDX };

int cpu_instr_cycles[2] = {1, 2};

struct cpu_instr {
  enum cpu_instr_type type;
  int arg;
};

struct cpu {
  int cycle;
  int x;

  struct cpu_instr instr;
  int cycle_instr_start;
};

size_t parse_cpu_instr(char *s, size_t s_len, void **buffer, size_t ln) {
  struct cpu_instr *instrs = (struct cpu_instr *)*buffer;

  if (s[0] == 'n') {
    instrs[ln].type = NOOP;
  } else {
    instrs[ln].type = ADDX;
    sscanf(s, "addx %d\n", &instrs[ln].arg);
  }

  (void)s_len; /* unused */

  return 1;
}

void day10() {
  struct cpu_instr *instrs = calloc(MAX_CPU_INSTRS, sizeof(struct cpu_instr));
  size_t instrs_size = parse_file(CPU_SIGNAL_FILE, parse_cpu_instr, instrs);
  struct cpu cpu;
  int signal_sum = 0;
  size_t i = 1;
  int xs[240];
  int cycles_to_sum[6] = {20, 60, 100, 140, 180, 220};

  cpu.cycle = 0;
  cpu.x = 1;
  cpu.instr = instrs[0];
  cpu.cycle_instr_start = 0;

  while (i < instrs_size && cpu.cycle <= 240) {
    cpu.cycle++;

    xs[cpu.cycle - 1] = cpu.x;

    if (cpu.cycle >= cpu.cycle_instr_start + cpu_instr_cycles[cpu.instr.type]) {
      if (cpu.instr.type == ADDX) {
        cpu.x += cpu.instr.arg;
      }

      cpu.instr = instrs[i];
      cpu.cycle_instr_start = cpu.cycle;

      i++;
    }
  }

  for (i = 0; i < 6; i++) {
    int cycle = cycles_to_sum[i];
    signal_sum += cycle * xs[cycle - 1];
  }

  printf("\n=== Day 10 ===\n");
  printf("Signal sum: %d\n", signal_sum);
  printf("Image:\n");

  for (i = 1; i <= 240; i++) {
    int x = (i - 1) % SCREEN_WIDTH;

    if (x >= xs[i - 1] - 1 && x <= xs[i - 1] + 1) {
      printf("#");
    } else {
      printf(".");
    }

    if (x == SCREEN_WIDTH - 1) {
      printf("\n");
    }
  }
}

/*** main ***/

int main() {
  printf("Advent of Code 2022!\n");

  day1();
  day2();
  day3();
  day4();
  day5();
  day6();
  day7();
  day8();
  day9();
  day10();

  return 0;
}
