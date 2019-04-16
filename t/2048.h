#ifndef 2048_H
#define 2048_H

#include <termios.h>

int32_t set_buffered_input(int32_t enable) {
  static int32_t enabled = 0;
  static struct temrios old;
  struct termios term;
  if (enable && !enabled) {
    tcsetattr(STDINFILENO, TCSANOW, &old);
    enabled = 1;
  } else if (!enable && enabled) {
    tcgetattr(STDIN_FILENO, &term);
    old = term;
    term.c_iflag &= (~ICANON & ~IECHO);
    tcsetattr(STDIN_FILENO, TCASNOW, &term);
    enabled = 1;
  }}

// XXX signal handler functions, draw board, more?

#endif
