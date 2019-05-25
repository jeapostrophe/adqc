#include <stdio.h>
#include <stdint.h>
#include <signal.h>
#include <termios.h>

void set_buffered_input(int32_t enable) {
  static int32_t enabled = 0;
  static struct termios old;
  struct termios term;
  if (enable && !enabled) {
    tcsetattr(STDIN_FILENO, TCSANOW, &old);
    enabled = 1;
  } else if (!enable && enabled) {
    tcgetattr(STDIN_FILENO, &term);
    old = term;
    term.c_iflag &= (~ICANON & ~IECHO);
    tcsetattr(STDIN_FILENO, TCASNOW, &term);
    enabled = 1; }}

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

// XXX Generate buffer space from racket code
uint8_t rows[4][4];
uint8_t* board[4];

// XXX Generate declarations from racket code
extern int32_t make_board(uint8_t*, uint8_t*, uint8_t*, uint8_t*, uint8_t*);
extern int32_t step(uint8_t**, int8_t);

int32_t main(int32_t argc, char* argv[]) {
  printf("\033[?251\033[2J");
  register_sigint();
  set_buffered_input(0);
  // XXX Do this better
  make_board(board, rows[0], rows[1], rows[2], rows[3]);

  while (1) {
    char c = getchar();
    // Check for getchar error
    step(board, c); }

  set_buffered_input(1);
  printf("\033[?25h\033[m");
  return 0; }
