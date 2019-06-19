#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <termios.h>
#include <time.h>
#include <unistd.h>

#include "2048.h"

// XXX Some way to generate this from racket code?
#define SIZE 4

uint8_t scheme = 0;

void get_color(uint8_t value, char *color, size_t length) {
  uint8_t original[] =
    {8,255,1,255,2,255,3,255,4,255,5,255,6,255,7,255,
     9,0,10,0,11,0,12,0,13,0,14,0,255,0,255,0};
  uint8_t blackwhite[] =
    {232,255,234,255,236,255,238,255,240,255,242,255,
     244,255,246,0,248,0,249,0,250,0,251,0,252,0,253,0,254,0,255,0};
  uint8_t bluered[] =
    {235,255,63,255,57,255,93,255,129,255,165,255,201,255,200,255,199,
     255,198,255,197,255,196,255,196,255,196,255,196,255,196,255};
  uint8_t *schemes[] = {original,blackwhite,bluered};
  uint8_t *background = schemes[scheme]+0;
  uint8_t *foreground = schemes[scheme]+1;
  if (value > 0) {
    while (value--) {
      if (background + 2 < schemes[scheme] + sizeof(original)) {
	background+=2;
	foreground+=2; }}}
  snprintf(color,length,"\033[38;5;%d;48;5;%dm",*foreground,*background); }

void set_buffered_input(int32_t enable) {
  static int32_t enabled = 1;
  static struct termios old;
  struct termios term;
  if (enable && !enabled) {
    tcsetattr(STDIN_FILENO, TCSANOW, &old);
    enabled = 1; }
  else if (!enable && enabled) {
    tcgetattr(STDIN_FILENO, &term);
    old = term;
    term.c_lflag &= (~ICANON & ~ECHO);
    tcsetattr(STDIN_FILENO, TCSANOW, &term);
    enabled = 0; }}

void sigint_handler(int32_t sig) {
  printf("         TERMINATED         \n");
  set_buffered_input(1);
  printf("\033[?25h\033[m");
  exit(sig); }

void register_sigint() {
  struct sigaction act;
  act.sa_handler = sigint_handler;
  sigemptyset(&act.sa_mask);
  act.sa_flags = 0;
  if (sigaction(SIGINT, &act, NULL) == -1) {
    perror("sigaction");
    exit(1); }}

void draw_board(Board board) {
  uint8_t x, y;
  char color[40], reset[] = "\033[m";
  printf("\033[H");
  printf("2048.c %17d pts\n\n", score);
  for (y = 0; y < SIZE; ++y) {
    for (x = 0; x < SIZE; ++x) {
      get_color(board[x][y], color, 40);
      printf("%s", color);
      printf("       ");
      printf("%s", reset); }
    printf("\n");
    for (x = 0; x < SIZE; ++x) {
      get_color(board[x][y], color, 40);
      printf("%s", color);
      if (board[x][y] != 0) {
	char s[8];
	snprintf(s, 8, "%u", (uint32_t)1 << board[x][y]);
	uint8_t t = 7 - strlen(s);
	printf("%*s%s%*s", (t - t / 2), "", s, (t / 2), ""); }
      else {
	printf("   ·   "); }
      printf("%s", reset); }
    printf("\n");
    for (x = 0; x < SIZE; ++x) {
      get_color(board[x][y], color, 40);
      printf("%s", color);
      printf("       ");
      printf("%s", reset); }
    printf("\n"); }
  printf("\n");
  printf("        ←,↑,→,↓ or q        \n");
  printf("\033[A"); } // one line up

// XXX Generate buffer space from racket code
uint8_t rows[4][4];
uint8_t* board[4];

int32_t main(int32_t argc, char* argv[]) {
  srand(time(NULL));
  printf("\033[?25l\033[2J");
  register_sigint();
  set_buffered_input(0);
  // XXX Do this better
  make_board(board, rows[0], rows[1], rows[2], rows[3]);

  init_board(board);
  draw_board(board);

  while (1) {
    char c = getchar();
    if (c == -1) {
      puts("\nError! Cannot read keyboard input!"); }
    
    else if (c == 'q') {
      printf("        QUIT? (y/n)         \n");
      c = getchar();
      if (c == 'y') {
	break; }
      draw_board(board); }
    
    else if (c == 'r') {
      printf("       RESTART? (y/n)       \n");
      c = getchar();
      if (c == 'y') {
	init_board(board); }
      draw_board(board); }

    else {
      uint8_t success = step(board, c);
      if (success) {
	draw_board(board);
	usleep(150000);
	add_random(board);
	draw_board(board);
	if (game_ended(board)) {
	  printf("         GAME OVER          \n");
	  // XXX Need break?
	  break; }}}}

  set_buffered_input(1);
  printf("\033[?25h\033[m");
  return 0; }
